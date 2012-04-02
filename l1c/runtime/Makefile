CC = gcc
CFLAGS = -Wall -Werror -m32 -c
RM = rm

all: runtime.c
	$(CC) $(CFLAGS) -O2 -o runtime.o runtime.c

debug: runtime.c
	$(CC) $(CFLAGS) -g -DDEBUG -o runtime.o runtime.c

clean: 
	@$(RM) -f runtime.o
