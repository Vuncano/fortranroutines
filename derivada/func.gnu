set xlabel "x"
set ylabel "y"

set title "f(x) = xsin(x**2)+1"

plot 'func.dat' with lines, 'funca.dat' with lines lt rgb "blue", 'funcnu.dat' with lines lt rgb "red"
