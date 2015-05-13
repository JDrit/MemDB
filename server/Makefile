CC     := gcc
CFLAGS := -I./include -I../protobuf/include
CFLAGS += `pkg-config --cflags glib-2.0` `pkg-config --cflags 'libprotobuf-c >= 1.0.0'` -g -Wall -Wextra -pedantic -std=c99
LIBS   := `pkg-config --libs glib-2.0` `pkg-config --libs 'libprotobuf-c >= 1.0.0'`
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
