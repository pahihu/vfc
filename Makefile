CC = cc
OPT = -Os -fomit-frame-pointer
CFLAGS = $(OPT) -Wall -Wextra
SHFLAGS = -fPIC
LIBS = -lm
STRIP = strip

ifeq ($(shell uname -s), Linux)
  LIBS += -ldl
endif

all: vfc libtest.so

vfc: vfc.c
	$(CC) $(CFLAGS) -o $@ $< $(LIBS)
	$(STRIP) -S -x $@
	size $@
	ls -l $@

libtest.so: test.c
	$(CC) -shared $(SHFLAGS) -o $@ $<

clean:
	$(RM) vfc vfc.o libtest.so
	$(RM) -r *.dSYM
