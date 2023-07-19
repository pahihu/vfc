# CC = cc -m32
CC = cc -flto -ffunction-sections -fdata-sections -Wl,-dead_strip  #-g
OPT = -Os -fomit-frame-pointer
CFLAGS = $(OPT) -Wall -Wextra -Ddarwin
STRIP = strip
# STRIP = :

all: vfc

vfc: vfc.c
	$(CC) $(CFLAGS) -o $@ $< -lm
	$(STRIP) -S -x $@
	size $@
	ls -l $@

clean:
	$(RM) vfc vfc.o
	$(RM) -r *.dSYM
