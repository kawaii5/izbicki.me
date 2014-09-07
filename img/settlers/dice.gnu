#set terminal postscript enhanced color 'Times-Roman' 14 
#set output 'plots.ps' 
#set terminal png size 400,400
set terminal pngcairo transparent enhanced font "arial,10" fontscale 1.0 size 400,400;
set output 'plots.png' 
set size 1,1
set tmargin at screen 1
set bmargin at screen 0.1
set lmargin at screen 0.05
set rmargin at screen 0.95

unset title
unset key
unset colorbox

set style data histogram
set style histogram cluster gap 1
set style fill solid border -1
set boxwidth 0.9

###################
# dirichlet math

pdf(a,b,c,x,y,z)=(1/B(a,b,c))*(x**(a-1)*y**(b-1)*z**(c-1))

B(a,b,c)=0.000001 
#B(a,b,c)=(gamma(a)*gamma(b)*gamma(c))/gamma(a+b+c)
#B(a,b,c) = exp(lgamma(a)+lgamma(b)+lgamma(c) - lgamma(a+b+c))

pos(x)=x>=0?x:1/0

#tri(a,b,c,x,y)=pdf(a,b,c,x,y,pos(1-x-y))
tri(a,b,c,x,y)=pdf(a,b,c,pos(2*y/sqrt(3)),pos(x-y/sqrt(3)),pos(1-y/sqrt(3)-x))

###################
# histograms

#set yrange [0:150]
#plot 'dice0.dat'  using 2:xtic(1) 
#plot 'dice0.dat'  using 3:xtic(1) 
#plot 'dice0.dat'  using 4:xtic(1) 
#plot 'dice0.dat'  using 5:xtic(1) 
#plot 'dice0.dat'  using 6:xtic(1) 

###################
# dirichlet plots

#plot sin(x)

#set samples 125, 125
#set isosamples 150, 150
set samples 400, 400
set isosamples 400, 400
set pm3d implicit at s flush center
#set palette color positive
#set palette rgbformulae -3,-7,-5
set palette rgbformulae -23,-21,-7

set arrow 1 from 0,0 to .5,sqrt(3)/2 nohead front lt -1 lw 2
set arrow 2 from 0,0 to 1,0          nohead front lt -1 lw 2
set arrow 3 from 1,0 to .5,sqrt(3)/2 nohead front lt -1 lw 2

set arrow 4 from 0,0 to 0.75,sqrt(3)/4  nohead front lt -1 lw 1 lc rgb "#333333"
set arrow 5 from 1,0 to 0.25,sqrt(3)/4  nohead front lt -1 lw 1 lc rgb "#333333"
set arrow 6 from 0.5,sqrt(3)/2 to 0.5,0 nohead front lt -1 lw 1 lc rgb "#333333"

set label 3 "1" at -.03,0
set label 1 "2345" at 0.5,sqrt(3)/2+.05 center
set label 2 "6" at 1+.015,0

set view map
#set view 180, 315, 1, 1.48

set xrange [ 0 : 1 ] noreverse nowriteback
set yrange [ 0 : 1 ] noreverse nowriteback
set zrange [ : ] noreverse nowriteback
#set cbrange [ : ] noreverse nowriteback

unset border
unset xtics
unset ytics
unset ztics


#set samples 200
#set isosamples 210
#set xlabel "X axis"
#set ylabel "Y axis"
#set zlabel "Z axis" offset 1, 0
#set view 60, 30, 0.85, 1.1
#set title "3D gnuplot demo - contour plot"
#set dgrid3d
#splot 'grid.dat' using 1:2:($1*$2)

#splot '++' using 1:2:(tri(103,126,462/4,$1,$2))
#splot '++' using 1:2:(tri(111,102,435/4,$1,$2))
#splot '++' using 1:2:(tri(108,142,537/4,$1,$2))
#splot '++' using 1:2:(tri(104,140,422/4,$1,$2))
#splot '++' using 1:2:(tri( 99,137,445/4,$1,$2))
#splot '++' using 1:2:(tri( 97,125,428/4,$1,$2))
#splot '++' using 1:2:(tri(118,197,589/4,$1,$2))
#splot '++' using 1:2:(tri(106,263,940/4,$1,$2))
#splot '++' using 1:2:(tri(106,263,940/4,$1,$2))
#splot '++' using 1:2:(tri( 57,182,415/4,$1,$2))
#splot '++' using 1:2:(tri(10,10,10,$1,$2))

###################
## histograms

f(a,b,x) = (a+b==x)?1:0

hist(x,p1,p2,p6) = \
    f(1,1,x)*p1*p1 + \
    f(1,2,x)*p1*p2 + \
    f(1,3,x)*p1*p2 + \
    f(1,4,x)*p1*p2 + \
    f(1,5,x)*p1*p2 + \
    f(1,6,x)*p1*p6 + \
    f(2,1,x)*p2*p1 + \
    f(2,2,x)*p2*p2 + \
    f(2,3,x)*p2*p2 + \
    f(2,4,x)*p2*p2 + \
    f(2,5,x)*p2*p2 + \
    f(2,6,x)*p2*p6 + \
    f(3,1,x)*p2*p1 + \
    f(3,2,x)*p2*p2 + \
    f(3,3,x)*p2*p2 + \
    f(3,4,x)*p2*p2 + \
    f(3,5,x)*p2*p2 + \
    f(3,6,x)*p2*p6 + \
    f(4,1,x)*p2*p1 + \
    f(4,2,x)*p2*p2 + \
    f(4,3,x)*p2*p2 + \
    f(4,4,x)*p2*p2 + \
    f(4,5,x)*p2*p2 + \
    f(4,6,x)*p2*p6 + \
    f(5,1,x)*p2*p1 + \
    f(5,2,x)*p2*p2 + \
    f(5,3,x)*p2*p2 + \
    f(5,4,x)*p2*p2 + \
    f(5,5,x)*p2*p2 + \
    f(5,6,x)*p2*p6 + \
    f(6,1,x)*p6*p1 + \
    f(6,2,x)*p6*p2 + \
    f(6,3,x)*p6*p2 + \
    f(6,4,x)*p6*p2 + \
    f(6,5,x)*p6*p2 + \
    f(6,6,x)*p6*p6 + \
    0

unset arrow 1
unset arrow 2
unset arrow 3
unset arrow 4
unset arrow 5
unset arrow 6
unset label 1
unset label 2
unset label 3

set style histogram

set xrange [-0.5:10.5]
set yrange [0:10]

set border
set xtics
set ytics

plot 'hist.dat' using (hist($1,1,1,1)):xtic(1), \
     'hist.dat' using (hist($1,0.9,1,1.1)):xtic(1), \
     'hist.dat' using (hist($1,0.8,1,1.2)):xtic(1), \
     'hist.dat' using (hist($1,0.7,1,1.3)):xtic(1), \
     'hist.dat' using (hist($1,0.5,1,1.5)):xtic(1)
