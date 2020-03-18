## makefile for SVD
FSOURCE    = solve1.f90

.PHONY: clean

main.exe: $(MODULES) $(OBJECTS)
	gfortran -llapack solve1.f90
	./a.out
       
%.o: %.f90
	gfortran $<

%.mod: %.f90
	gfortran $<

clean:
	rm -f ${ALLOBJ} ${EXECBIN} *.o *.exe