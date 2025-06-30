CC = gcc
# OPT = -O2 -fomit-frame-pointer -DNDEBUG=1
OPT = -g
CFLAGS = -Wall -pedantic $(OPT)
LDFLAGS = $(OPT)
SRCS = mix.c
OBJS = $(SRCS:.c=.o)

TARGET = mix

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS)

clean:
	$(RM) $(TARGET)
	$(RM) $(OBJS)

# vim:set ts=4 sw=4 noet:
