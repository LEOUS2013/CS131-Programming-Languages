import asyncio
import sys
import logging
import time

valid_server_names = {"Goloman": 12115, "Hands": 12116, "Holiday": 12117, "Welsh": 12118, "Wilkes": 12119}
client_commands = ["IAMAT", "WHATSAT"]

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
    
    latitude = location[1 : index_second_op]
    longitude = location[index_second_op + 1 :]
    try: 
        float(latitude)
    except:
        return False, -1, -1
    
    try:
        float(longitude)
        return True, float(latitude), float(longitude)
    except:
        return False, -1, -1

def valid_timestamp(timestamp):
    try:
        float(timestamp)
        return True, float(timestamp)
    except:
        return False

def invalid_message(message, writer):
    logging.info("INVALID received: {}".format(message))
    response = "? {}".format(message)
    response_with_newline = response + '\n'
    writer.write(response_with_newline.encode())
    logging.info("sent: {}".format(response))

class Server:
    def __init__(self, name, host='127.0.0.1'):
        self.name = name
        self.host = host
        self.port = valid_server_names[name]
    
    async def interpret_message(self, reader, writer):
        data = await reader.readline()
        message = data.decode()
        message = message[:-1]
        tokens = message.split()
        if (len(tokens) != 4) or (tokens[0] not in client_commands):
            invalid_message(message, writer)
        else:
            if tokens[0] == 'WHATSAT':
                if (not tokens[2].isdigit()) or (not tokens[3].isdigit()):
                    invalid_message(message, writer)
                else:
                    logging.info("received: {}".format(message))
                    self.process_whatsat(tokens, writer)
            elif tokens[0] == 'IAMAT':
                if (not valid_timestamp(tokens[3])[0]) or (not valid_location(tokens[2])[0]):
                    invalid_message(message, writer)
                else:
                    logging.info("received: {}".format(message))
                    self.process_iamat(tokens, writer)
            else:
                invalid_message(message)

        await writer.drain()
        writer.close()
    
    def run_until_interrupted(self):
        loop = asyncio.get_event_loop()
        coro = asyncio.start_server(self.interpret_message, self.host, self.port, loop=loop)
        server = loop.run_until_complete(coro)

        try:
            loop.run_forever()
        except KeyboardInterrupt:
            pass
        
        server.close()
        loop.run_until_complete(server.wait_closed())
        loop.close()

    def process_iamat(self, tokens_list, writer):
        time_diff = time.time() - float(tokens_list[3])
        ret_string = "AT {} {} {} {} {}".format(self.name, time_diff, tokens_list[1], tokens_list[2], tokens_list[3])
        ret_string_with_newline = ret_string + '\n'
        writer.write(ret_string_with_newline.encode())
        logging.info("sent: {}".format(ret_string))
        return

    def process_whatsat(self, tokens_list, writer):
        return

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
"""