CC = gcc
# OPT = -O2 -fomit-frame-pointer -DNDEBUG=1
OPT = -g -ffloat-store -frounding-math
CFLAGS = -Wall -pedantic $(OPT)
LDFLAGS = $(OPT)
SRCS = mix.c
OBJS = $(SRCS:.c=.o)

TARGET = mix

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) -lm

fpexptab: fpexptab.o
	$(CC) $(LDFLAGS) -o $@ fpexptab.o -lm

clean:
	$(RM) fpexptab fpexptab.o
	$(RM) $(TARGET)
	$(RM) $(OBJS)
	$(RM) *.prn *.log *.tra *.tre

# vim:set ts=4 sw=4 noet:
