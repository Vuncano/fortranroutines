set xlabel "x"
set ylabel "y"

set title "1/(1-x)"

plot '1-x.dat' with lines, '1-x2.dat' with lines
