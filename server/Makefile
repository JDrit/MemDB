CC     := clang
CFLAGS := -I./include -I../protobuf/include -pg
CFLAGS += `pkg-config --cflags libprotobuf-c` `pkg-config --cflags libprotobuf-c-rpc` -g -Wall -Wextra -std=c11
LIBS   := `pkg-config --libs libprotobuf-c` `pkg-config --libs libprotobuf-c-rpc` -lpthread
SOURCE := $(wildcard src/*.c ../protobuf/src/*.c)

EXECUTABLE = server
OBJECTS = $(SOURCE:.c=.o)

all: $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS)

clean:
	$(RM) $(EXECUTABLE) $(OBJECTS)

%.o: %.c
	$(CC) -c $(CFLAGS) -o $@ $<
