.PHONY: all clean

all:
	@echo "Available targets:"
	@echo "test           Compile and run all unit tests."
	@echo "testriopacket  Compile and run unit tests for riopacket."
	@echo "testriostack   Compile and run unit tests for riostack."
	@echo "clean          Clean up all created targets."

test: testriostack testriopacket

testriopacket: rioconfig.h riopacket.c riopacket.h test_riopacket.c
	$(CC) -o testriopacket test_riopacket.c -g -lcunit && ./testriopacket

testriostack: rioconfig.h riostack.c riostack.h riopacket.h riopacket.c test_riostack.c
	$(CC) -o testriostack test_riostack.c -g -lcunit && ./testriostack

clean:
	rm -f testriostack testriopacket
