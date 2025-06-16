CFLAGS = -Wall -pedantic -g
LDFLAGS = -g
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
