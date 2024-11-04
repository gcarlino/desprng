# CC = gcc
# CFLAGS = -O2 -ffast-math -finline-functions -funroll-loops -fomit-frame-pointer
# LDFLAGS =

# CC = nvc
CC = gcc
# CFLAGS = -O2 -acc -Minfo 
# CFLAGS = -O0 -g -Minfo
CFLAGS = -O0 -g 
# FORTRAN = nvfortran
FORTRAN = gfortran
# FFLAGS = -O2 -acc -Minfo
FFLAGS = -O0 -g
FFLAGS = -O0 -g -fopenacc
# FFLAGS = -O0 -g
# LDFLAGS = -O2 -acc 
LDFLAGS = -fopenacc

FILES = desprng.h desprng.c des.c desprng_f.f90 toypicmcc.c toypicmcc_f.f90 xiplot.py xiplot_f.py oldnewcomparison.c d3des.h d3des.c Makefile crush0.c crush1.c crush2.c Makefile.crush

.PHONY : all
all : libdesprng.a toypicmcc toypicmcc_f

libdesprng.a : desprng.o des.o
	ar cr libdesprng.a desprng.o des.o

desprng_f.o: desprng_f.f90
	$(FORTRAN) $(FFLAGS) -c desprng_f.f90

desprng.o : desprng.h desprng.c
	$(CC) $(CFLAGS) -c desprng.c

des.o : des.c
	$(CC) $(CFLAGS) -c des.c

toypicmcc : toypicmcc.o libdesprng.a
	$(CC) -o toypicmcc toypicmcc.o -L. -ldesprng $(LDFLAGS) -lm

toypicmcc_f: desprng_f.o toypicmcc_f.o libdesprng.a
	$(FORTRAN) -o toypicmcc_f toypicmcc_f.o desprng_f.o -L. -ldesprng $(LDFLAGS) -lm

toypicmcc.o : toypicmcc.c
	$(CC) $(CFLAGS) -c toypicmcc.c

toypicmcc_f.o : toypicmcc_f.f90
	$(FORTRAN) $(FFLAGS) -c toypicmcc_f.f90

oldnewcomparison : oldnewcomparison.o d3des.o libdesprng.a
	$(CC) -o oldnewcomparison oldnewcomparison.o d3des.o -L. -ldesprng

oldnewcomparison.o : oldnewcomparison.c
	$(CC) $(CFLAGS) -c oldnewcomparison.c

d3des.o : d3des.h d3des.c
	$(CC) $(CFLAGS) -c d3des.c

desprng.tgz : $(FILES)
	tar cfvz desprng.tgz $(FILES)

.PHONY : dist
dist : desprng.tgz

.PHONY : linecount
linecount :
	wc -l $(FILES)

.PHONY : clean
clean :
	rm -f libdesprng.a *.o *.mod toypicmcc toypicmcc_f oldnewcomparison d3des.out desprng.out desprng_f.out *~ *.core
