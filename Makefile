UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
CFLAGS += -fPIC
ext=so
endif

ifeq ($(UNAME), Darwin)
ext=dylib
endif

$(addsuffix .$(ext), keyboard): keyboard.o
	gcc -shared keyboard.o -o $(addsuffix .$(ext), libkeyboard)

keyboard.o: keyboard.c
	gcc -c keyboard.c -o keyboard.o
