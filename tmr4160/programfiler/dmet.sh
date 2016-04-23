#Kompilerer hovedprogram
make clean
make
./fprogram

make c
./cprogram

echo
echo ----------------------------------
echo Hei og velkommen til DMET DELUXE 2000
echo Vennligst følg instruksene på skjermen
echo Trykk j for ja, eller ENTER for nei
echo ----------------------------------
echo

read -p "Vil du plotte resultatgrafene fra programmet? (j/n) " answer
case ${answer:0:1} in
    j|J )
      #Starter gnuplot
	gnuplot -persist << __EOF 
set yrange[0:50]
set term wxt 7
set title "Beregnet JONSWAP-spekter"
set ylabel "Sn"
set xlabel "omega [rad/s]"
plot "spekterplot.dat" with lines title "JONSWAP"

set yrange [0:4]
set xrange [0:2]
set term wxt 1
set title "Tilleggsmasse for begge lektere"
set xlabel "omega (B/2g)^0.5 [-]"
set ylabel "A33 2D"
plot "A33plot.txt"  with lines title "A33 lekter A"
replot "A33plot.txt" using 3:4 with lines title "A33 lekter B"

set yrange [-0.2:1.1]
set term wxt 2
set xzeroaxis linetype 3 linewidth 2
set title "Dempekoeffisient for begge lektere"
set ylabel "B33 2D"
plot "B33plot.txt" with lines title "B33 lekter A"
replot "B33plot.txt" using 3:4 with lines title "B33 lekter B"
unset xzeroaxis

set yrange[0:1.1]
set xrange[0:3]
set term wxt 20
set title "Transferfunksjon hiv for begge lektere"
set xlabel "omega [rad/s]"
set ylabel "|H3(omega)|[-]"
plot "H3plot.txt" with lines title "H3 lekter A [-]"
replot "H3plot.txt" using 1:3 with lines title "H3 lekter B [-]"

set yrange[0:1.1]
set term wxt 21
set ylabel "|H5(omega)| [-]"
set title "Transferfunksjon stamp for begge lektere"
plot "H5plot22.txt"  with lines title "H5 lekter A [-]"
replot "H5plot22.txt" using 1:3 with lines title "H5 lekter B [-]"

set yrange[-4:0]
set term wxt 11
set xzeroaxis
set ylabel "delta3 [-]"
set title "Fasevinkel hiv for lekter A"
plot "fase3sjekk.txt" with lines title "Delta3 for lekter "
replot "fase3sjekk.txt" using 1:3 with lines title "Delta3 for lekter B"

set term wxt 12
set yrange[0:4]
set ylabel "delta5 [-]"
set title "Fasevinkel stamp for lekter A"
plot "fase5sjekk.txt" with lines title "Delta5 for lekter A"
replot "fase5sjekk.txt" using 1:3 with lines title "Delta5 for lekter B"
unset xzeroaxis

__EOF
    ;;
    * )
	echo OK, fortsetter kjøring
    ;;
esac

read -p "Vil du ha en utskrift fra bølgestatistikken i  programmet? (j/n) " answer
case ${answer:0:1} in
    j|J )
	#skriver til skjerm
	cat statistikk.txt << __EOF

__EOF
    ;;
    * )
	echo OK, kjører visualiseringsprogrammet
    ;;
esac

read -p "Vil du plotte respons mot regulær sjø? (j/n) " answer
case ${answer:0:1} in
    j|J )
	#plotter
	gnuplot -persist << __EOF 
set term wxt 15
set title "Respons mot overflateheving, lekter A, omega = 0.399"
set ylabel "Utslag [m]"
set xlabel "Tid [s]"
plot "overflateplott.dat" using 1 with lines title "zeta", \
     "responsplott.dat" every ::1::600 using 1 with lines title "eta3",\
     "responsplott.dat" every ::1::600 using 2 with lines title "eta5"
__EOF

echo Programmet er ferdig, takk for i dag!

    ;;
    * )
	echo Programmet er ferdig, takk for i dag!
    ;;
esac


