.PHONY: run
run:
	rm -rf *.dat 
	gfortran main.f90
	./a.out 

.PHONY: runplot
runplot: run 
	python plot.py
