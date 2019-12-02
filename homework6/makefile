.SILENT:

default: check

check: 
	go build -o everyNth
	./everyNth

clean: 
	rm -rf everyNth *.tar.gz

dist:
	tar -czf hw6.tar.gz everyNth.go makefile

