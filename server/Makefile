CC     := gcc
CFLAGS := -I./include
CFLAGS += `pkg-config --cflags glib-2.0` -g -Wall -std=c99
LIBS   := `pkg-config --libs glib-2.0`
SOURCE := $(wildcard src/*.c)

EXECUTABLE = server
OBJECTS = $(SOURCE:.c=.o)

all: $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS)

clean:
	$(RM) $(EXECUTABLE) $(OBJECTS)

%.o: %.c
	$(CC) -c $(CFLAGS) -o $@ $<
