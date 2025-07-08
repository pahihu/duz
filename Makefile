CC = gcc
OPT = -O2 -fomit-frame-pointer -DNDEBUG=1
# OPT = -g
CFLAGS = -Wall -pedantic $(OPT)
LDFLAGS = $(OPT)
SRCS = mix.c
OBJS = $(SRCS:.c=.o)

TARGET = mix

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS)

fpexptab: fpexptab.o
	$(CC) $(LDFLAGS) -o $@ fpexptab.o

clean:
	$(RM) fpexptab
	$(RM) $(TARGET)
	$(RM) $(OBJS)

# vim:set ts=4 sw=4 noet:
