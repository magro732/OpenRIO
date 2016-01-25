.PHONY: help all test clean

help:
	@echo "Available targets:"
	@echo "test           Compile and run all unit tests."
	@echo "testriopacket  Compile and run unit tests for riopacket."
	@echo "testriostack   Compile and run unit tests for riostack."
	@echo "clean          Clean up all created targets."

all: test

test: testriostack testriopacket
	@echo "-----Coverage result from testing riopacket-----" 
	gcov test_riopacket.c
	@echo "-----Coverage result from testing riostack-----" 
	gcov test_riostack.c

testriopacket: rioconfig.h riopacket.c riopacket.h test_riopacket.c
	$(CC) -o testriopacket test_riopacket.c -g -fprofile-arcs -ftest-coverage -lcunit
	./testriopacket

testriostack: rioconfig.h riostack.c riostack.h riopacket.h riopacket.c test_riostack.c
	$(CC) -o testriostack test_riostack.c -g -fprofile-arcs -ftest-coverage -lcunit
	./testriostack

clean:
	rm -f testriostack testriopacket *.gcov *.gcda *.gcno
