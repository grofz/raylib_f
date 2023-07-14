# compiler, linker and flags
.PHONY = all clean
fflags =-Ofast -O5 -fPIC
#fflags =-fcheck=all -fPIC
cc = gfortran $(fflags)

# path to sources and objects

dir = src

vpath %.f90 $(dir)
vpath %.o $(dir)

# object files

modobjects = \
						 $(dir)/raylib.o \
             $(dir)/example01.o

main1 = $(dir)/main.o

allobjects = $(modobjects) $(main1)

# modules

modules = raylib_mod

# libraries
#LIBS="`pkg-config --libs raylib` -ldl -lpthread"

# default goal and dependencies
all: a.x

a.x : $(modobjects) Makefile $(main1)
	$(cc) -o $@ $(modobjects) $(main1) `pkg-config --libs raylib` -ldl -lpthread

$(modules) $(allobjects) : Makefile

$(main1) : $(modobjects)

#.f.o:

%.o : %.f90
	$(cc) -c $< -o $@

# one phoney target

clean : 
	rm $(dir)/*.o *.mod
# end of makefile
