import asyncio
import sys
import logging
import time
import aiohttp
import json

valid_server_names = {"Goloman": 12115, "Hands": 12116, "Holiday": 12117, "Welsh": 12118, "Wilkes": 12119}
server_routes = {
    "Goloman": ["Hands", "Holiday", "Wilkes"],
    "Hands": ["Wilkes", "Goloman"],
    "Holiday": ["Welsh", "Wilkes", "Goloman"],
    "Wilkes": ["Goloman", "Hands", "Holiday"],
    "Welsh": ["Holiday"]
}
commands = ["IAMAT", "WHATSAT", "AT"]
google_places_base_link = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?"
google_places_api_key = "AIzaSyBbECdJ3N70JL-N8Li8OqClcDdyO7sG-Ng"

def process_json(json_str):
    new_str = ""
    for i in range(len(json_str) - 1):
        if json_str[i] == '\n' and json_str[i] == '\n':
            new_str += json_str[i]
            i += 1
        else:
            new_str += json_str[i]
    
    new_str += "\n"
    return new_str

def valid_location(location):
    if location[0] != '+' and location[0] != '-':
        return False, -1, -1
    
    op_counter = 0
    for c in location:
        if c == '+' or c == '-':
            op_counter += 1
            if op_counter > 2:
                return False, -1, -1

    index_first_op = 0
    index_counter = 0
    for c in location:
        if index_counter != 0 and (c == '+' or c == '-'):
            index_second_op = index_counter
            break
        index_counter += 1
    
    latitude = location[0 : index_second_op]
    longitude = location[index_second_op :]
    try: 
        float(latitude)
    except:
        return False, -1, -1
    
    try:
        float(longitude)
        return True, latitude, longitude
    except:
        return False, -1, -1

def valid_timestamp(timestamp):
    try:
        float(timestamp)
        return True, float(timestamp)
    except:
        return False

def untokenize(tokens):
    ret_str = ""
    for i in range(len(tokens)):
        if i == len(tokens) - 1:
            ret_str += tokens[i]
        else:
            ret_str += tokens[i]
            ret_str += ' '
    
    return ret_str

class Server:
    def __init__(self, name, host='127.0.0.1'):
        self.name = name
        self.host = host
        self.port = valid_server_names[name]
        self.client_locations = {}
        self.connections = {}
        self.writer = None
    
    async def interpret_message(self, reader, writer):
        while not reader.at_eof():
            data = await reader.readline()
            message = data.decode()
            message = message[:-1]
            tokens = message.split()
            client_num = writer.get_extra_info('peername')
            if (len(tokens) != 4) or (tokens[0] not in commands):
                if len(tokens) == 0:
                    if client_num in self.connections:
                        logging.info("closed connection with client {}".format(self.connections[client_num]))
                        del self.connections[client_num]
                    else:
                        logging.info("closed connection with neighboring server")
                elif (len(tokens) != 6) or (tokens[0] != "AT"):
                    self.invalid_message(message, writer)
                else:
                    #no need to check the parameters of 'AT' because server sent it and it must be correct
                    logging.info("established a connection with neighboring server")
                    logging.info("received from neighboring server: {}".format(message))

                    #if location info is present already, don't flood, else flood
                    location = valid_location(tokens[4])[1:3]
                    if tokens[3] not in self.client_locations or float(self.client_locations[tokens[3]][1]) < float(tokens[5]):
                        logging.info("updating location info for client {}".format(tokens[3]))
                        self.client_locations[tokens[3]] = location, tokens[5]
                        #propagate info to other servers
                        self.loop.create_task(self.flood(message))
                    else:
                        logging.info("location info already up to date for client {}".format(tokens[3]))
            else:
                if tokens[0] == 'WHATSAT':
                    if (not tokens[2].isdigit()) or (not tokens[3].isdigit()):
                        self.invalid_message(message, writer)
                    else:
                        #invalid because client location was not sent before requesting info
                        if tokens[1] not in self.client_locations:
                            to_send = "? " + message
                            to_send_with_newline = to_send + '\n'
                            if client_num in self.connections:
                                sender = self.connections[client_num]
                            else:
                                sender = "unknown client"
                            logging.info("INVALID message received from {} - client location info was not found: {}".format(sender, message))
                            writer.write(to_send_with_newline.encode())
                            logging.info("sent to client {}: {}".format(sender, to_send))
                        else:
                            if client_num not in self.connections:
                                #log the client's info
                                logging.info("established a connection with client {}".format(tokens[1]))
                                self.connections[client_num] = tokens[1]
                            logging.info("received from client {}: {}".format(self.connections[client_num], message))
                            self.process_whatsat(tokens, writer)
                elif tokens[0] == 'IAMAT':
                    if (not valid_timestamp(tokens[3])[0]) or (not valid_location(tokens[2])[0]):
                        self.invalid_message(message, writer)
                    else:
                        #log the client's info
                        if client_num not in self.connections:
                            logging.info("established a connection with client {}".format(tokens[1]))
                            self.connections[client_num] = tokens[1]
                        logging.info("received from client {}: {}".format(self.connections[client_num], message))
                        self.process_iamat(tokens, writer)
                else:
                    self.invalid_message(message, writer)
        await writer.drain()
        writer.close()
    
    def run_until_interrupted(self):
        self.loop = asyncio.get_event_loop()
        coro = asyncio.start_server(self.interpret_message, self.host, self.port, loop=self.loop)
        server = self.loop.run_until_complete(coro)

        try:
            self.loop.run_forever()
        except KeyboardInterrupt:
            pass
        
        server.close()
        self.loop.run_until_complete(server.wait_closed())
        self.loop.close()

    def process_iamat(self, tokens_list, writer):
        #send response to client
        time_diff = time.time() - float(tokens_list[3])
        ret_string = "AT {} {} {} {} {}".format(self.name, time_diff, tokens_list[1], tokens_list[2], tokens_list[3])
        ret_string_with_newline = ret_string + '\n'
        writer.write(ret_string_with_newline.encode())

        #update and flood the neighboring servers only if client's location data was updated
        location = valid_location(tokens_list[2])[1:3]
        if tokens_list[1] not in self.client_locations or float(self.client_locations[tokens_list[1]][1]) < float(tokens_list[3]):
            logging.info("updating location info for client {}".format(tokens_list[1]))
            self.client_locations[tokens_list[1]] = location, tokens_list[3]
            #propagate info to other servers
            self.loop.create_task(self.flood(ret_string))
        else:
            logging.info("location info already up to date for client {}".format(tokens_list[1]))

        #log info
        client_num = writer.get_extra_info('peername')
        logging.info("sent to client {}: {}".format(self.connections[client_num], ret_string))
        return

    def process_whatsat(self, tokens_list, writer):
        #process the number of results to be retrieved 
        if int(tokens_list[3]) > 20:
            num_results = 20
        else:
            num_results = int(tokens_list[3])

        #get the saved location and construct a string
        location = self.client_locations[tokens_list[1]][0]
        location_str = location[0] + ',' + location[1]

        #construct the query string
        query = "{}location={}&radius={}&key={}".format(google_places_base_link, location_str, tokens_list[2], google_places_api_key)
        time_diff = time.time() - float(self.client_locations[tokens_list[1]][1])

        #call the query function
        client_num = writer.get_extra_info('peername')
        self.loop.create_task(self.query_google(query, time_diff, self.connections[client_num], num_results, writer))
        return
    
    async def query_google(self, query, time_diff, client, num_results, writer):
        async with aiohttp.ClientSession() as session:
            async with session.get(query) as response:
                json_str = await response.text()

                json_obj = json.loads(json_str)
                if len(json_obj["results"]) > num_results:
                    json_obj["results"] = json_obj["results"][:num_results]

                json_str = json.dumps(json_obj, indent=4)
                json_str += '}'
                if time_diff > 0:
                    new_time_diff = "+"
                
                new_time_diff += str(time_diff)
                
                location_str = "{}{}".format(self.client_locations[client][0][0], self.client_locations[client][0][1])
                response = "AT {} {} {} {} {}\n{}".format(self.name, time_diff, client, location_str, self.client_locations[client][1], process_json(json_str))
                response_with_newline = response
                response_with_newline += '\n'
                writer.write(response_with_newline.encode())
                await writer.drain()
                logging.info("sent to client {}: {}".format(client, response_with_newline))

    async def flood(self, message):
        flood_str = "flooding to servers "
        for server in server_routes[self.name]:
            flood_str += server
            flood_str += ', '
        logging.info(flood_str[:-2])

        #establish connection with servers
        for server in server_routes[self.name]:
            logging.info("attempting to connect to server {}".format(server))
            neighbor_port = valid_server_names[server]
            try:
                reader, writer = await asyncio.open_connection(self.host, neighbor_port)
                logging.info("successfully connected to server {}".format(server))
                message_with_newline = message + '\n'
                writer.write(message_with_newline.encode())
                logging.info("sent to neighboring server {}: {}".format(server, message))
                await writer.drain()
                writer.close()
                await writer.wait_closed()
                logging.info("closed connection with server {}".format(server))
            except ConnectionRefusedError:
                logging.info("unable to connect to server {}".format(server))
    
    def invalid_message(self, message, writer):
        client_num = writer.get_extra_info('peername')
        if client_num in self.connections:
            sender = self.connections[client_num]
        else:
            sender = "unknown client"
        logging.info("INVALID message received from {} - message format unable to be parsed: {}".format(sender, message))
        response = "? {}".format(message)
        response_with_newline = response + '\n'
        writer.write(response_with_newline.encode())
        logging.info("sent: {}".format(response))

def main():
    if len(sys.argv) != 2:
        print("Invalid number of arguments")
        print("Usage: python3 server.py [Goloman Hands Holiday Welsh Wilkes]")
        exit(1)
    else:
        server_name = sys.argv[1]
        if server_name not in valid_server_names:
            print("Invalid server name")
            print("Usage: python3 server.py [Goloman Hands Holiday Welsh Wilkes]")
            exit(1) 
    
    logging.basicConfig(filename="server_{}.log".format(server_name), filemode='w', level=logging.INFO)

    server = Server(server_name)
    server.run_until_interrupted()

if __name__ == '__main__':
    main()

"""some tests
IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997
WHATSAT kiwi.cs.ucla.edu 10 5
IAMAT dog.cs.ucla.edu +34.068930-118.445127 1520023934.918963997
WHATSAT dog.cs.ucla.edu 10 1
"""