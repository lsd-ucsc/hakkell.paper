set datafile separator ','
set terminal svg size 620,465
set style fill transparent solid 0.2 noborder
set logscale x 2
set logscale y 2
set key top center
#set xtics rotate by 10 center offset 0,-0.5

set xrange [:65536]

s(ns) = ns / 1e9


set output "machine_macbookpro11,5-time.svg"
set title "Elapsed time vs Ring size (MacBookPro11,5; +RTS -N8)"
set ylabel "Elapsed time (Seconds)"
set xlabel "Nodes"
plot \
    "008_macbookpro11,5/series/fork & kill,time.dat"  using 1:(s($2)) with linespoints  title "control"  linecolor rgb "#990000", \
    "008_macbookpro11,5/series/actor ring,time.dat"   using 1:(s($2)) with linespoints  title "actors"   linecolor rgb "#009900", \
    "008_macbookpro11,5/series/channel ring,time.dat" using 1:(s($2)) with linespoints  title "channels" linecolor rgb "#000099", \
    "008_macbookpro11,5/series/fork & kill,time.dat"  using 1:(s($3)):(s($4)) with filledcurves notitle linecolor rgb "#ff6666", \
    "008_macbookpro11,5/series/actor ring,time.dat"   using 1:(s($3)):(s($4)) with filledcurves notitle linecolor rgb "#66ff66", \
    "008_macbookpro11,5/series/channel ring,time.dat" using 1:(s($3)):(s($4)) with filledcurves notitle linecolor rgb "#6666ff", \


set output "machine_macbookpro11,5-mean.svg"
set title "Mean elapsed time vs Ring size (MacBookPro11,5; +RTS -N8)"
set ylabel "Seconds"
set xlabel "Nodes"
plot \
    "008_macbookpro11,5/series/fork & kill,mean.dat"  using 1:(s($2)) with linespoints  title "control"  linecolor rgb "#990000", \
    "008_macbookpro11,5/series/actor ring,mean.dat"   using 1:(s($2)) with linespoints  title "actors"   linecolor rgb "#009900", \
    "008_macbookpro11,5/series/channel ring,mean.dat" using 1:(s($2)) with linespoints  title "channels" linecolor rgb "#000099", \
    "008_macbookpro11,5/series/fork & kill,mean.dat"  using 1:(s($3)):(s($4)) with filledcurves notitle linecolor rgb "#ff6666", \
    "008_macbookpro11,5/series/actor ring,mean.dat"   using 1:(s($3)):(s($4)) with filledcurves notitle linecolor rgb "#66ff66", \
    "008_macbookpro11,5/series/channel ring,mean.dat" using 1:(s($3)):(s($4)) with filledcurves notitle linecolor rgb "#6666ff", \


set output "machine_c3.8xlarge-mean.svg"
set title "Mean elapsed time vs Ring size (AWS c3.8xlarge; +RTS -N32)"
set ylabel "Seconds"
set xlabel "Nodes"
plot \
    "032_c3.8xlarge/series/control,mean.dat"  using 1:2 with linespoints  title "control"  linecolor rgb "#990000", \
    "032_c3.8xlarge/series/actors,mean.dat"   using 1:2 with linespoints  title "actors"   linecolor rgb "#009900", \
    "032_c3.8xlarge/series/channels,mean.dat" using 1:2 with linespoints  title "channels" linecolor rgb "#000099", \
    "032_c3.8xlarge/series/control,mean.dat"  using 1:3:4 with filledcurves notitle linecolor rgb "#ff6666", \
    "032_c3.8xlarge/series/actors,mean.dat"   using 1:3:4 with filledcurves notitle linecolor rgb "#66ff66", \
    "032_c3.8xlarge/series/channels,mean.dat" using 1:3:4 with filledcurves notitle linecolor rgb "#6666ff", \


set output "machine_c6a.48xlarge-mean.svg"
set title "Mean elapsed time vs Ring size (AWS c6a.48xlarge; +RTS -N192)"
set ylabel "Seconds"
set xlabel "Nodes"
plot \
    "192_c6a.48xlarge/series/control,mean.dat"  using 1:2 with linespoints  title "control"  linecolor rgb "#990000", \
    "192_c6a.48xlarge/series/actors,mean.dat"   using 1:2 with linespoints  title "actors"   linecolor rgb "#009900", \
    "192_c6a.48xlarge/series/channels,mean.dat" using 1:2 with linespoints  title "channels" linecolor rgb "#000099", \
    "192_c6a.48xlarge/series/control,mean.dat"  using 1:3:4 with filledcurves notitle linecolor rgb "#ff6666", \
    "192_c6a.48xlarge/series/actors,mean.dat"   using 1:3:4 with filledcurves notitle linecolor rgb "#66ff66", \
    "192_c6a.48xlarge/series/channels,mean.dat" using 1:3:4 with filledcurves notitle linecolor rgb "#6666ff", \

set output "machine_all-mean.svg"
set title "Mean elapsed time vs Ring size (MacBookPro11,5; +RTS -N8)"
set ylabel "Seconds"
set xlabel "Nodes"
plot \
    "008_macbookpro11,5/series/fork & kill,mean.dat"  using 1:(s($2)) with linespoints  title "control"  linecolor rgb "#990000", \
    "008_macbookpro11,5/series/actor ring,mean.dat"   using 1:(s($2)) with linespoints  title "actors"   linecolor rgb "#009900", \
    "008_macbookpro11,5/series/channel ring,mean.dat" using 1:(s($2)) with linespoints  title "channels" linecolor rgb "#000099", \
    "008_macbookpro11,5/series/fork & kill,mean.dat"  using 1:(s($3)):(s($4)) with filledcurves notitle linecolor rgb "#ff6666", \
    "008_macbookpro11,5/series/actor ring,mean.dat"   using 1:(s($3)):(s($4)) with filledcurves notitle linecolor rgb "#66ff66", \
    "008_macbookpro11,5/series/channel ring,mean.dat" using 1:(s($3)):(s($4)) with filledcurves notitle linecolor rgb "#6666ff", \
    "032_c3.8xlarge/series/control,mean.dat"  using 1:2 with linespoints  title "control"  linecolor rgb "#990000", \
    "032_c3.8xlarge/series/actors,mean.dat"   using 1:2 with linespoints  title "actors"   linecolor rgb "#009900", \
    "032_c3.8xlarge/series/channels,mean.dat" using 1:2 with linespoints  title "channels" linecolor rgb "#000099", \
    "032_c3.8xlarge/series/control,mean.dat"  using 1:3:4 with filledcurves notitle linecolor rgb "#ff6666", \
    "032_c3.8xlarge/series/actors,mean.dat"   using 1:3:4 with filledcurves notitle linecolor rgb "#66ff66", \
    "032_c3.8xlarge/series/channels,mean.dat" using 1:3:4 with filledcurves notitle linecolor rgb "#6666ff", \
    "192_c6a.48xlarge/series/control,mean.dat"  using 1:2 with linespoints  title "control"  linecolor rgb "#990000", \
    "192_c6a.48xlarge/series/actors,mean.dat"   using 1:2 with linespoints  title "actors"   linecolor rgb "#009900", \
    "192_c6a.48xlarge/series/channels,mean.dat" using 1:2 with linespoints  title "channels" linecolor rgb "#000099", \
    "192_c6a.48xlarge/series/control,mean.dat"  using 1:3:4 with filledcurves notitle linecolor rgb "#ff6666", \
    "192_c6a.48xlarge/series/actors,mean.dat"   using 1:3:4 with filledcurves notitle linecolor rgb "#66ff66", \
    "192_c6a.48xlarge/series/channels,mean.dat" using 1:3:4 with filledcurves notitle linecolor rgb "#6666ff", \





set output "group_actors-mean.svg"
set title "Mean elapsed time vs Ring size (Actors)"
set ylabel "Seconds"
set xlabel "Nodes"
plot \
    "008_macbookpro11,5/series/actor ring,mean.dat"   using 1:(s($2)):xtic(1) with linespoints  title "MacBookPro11,5 (+RTS -N8)" linecolor rgb "#009900", \
    "008_macbookpro11,5/series/actor ring,mean.dat"   using 1:(s($3)):(s($4)) with filledcurves notitle linecolor rgb "#66ff66", \
    "032_c3.8xlarge/series/actors,mean.dat"   using 1:2:xtic(1) with linespoints  title "c3.8xlarge (+RTS -N32)"   linecolor rgb "#009900", \
    "032_c3.8xlarge/series/actors,mean.dat"   using 1:3:4 with filledcurves notitle linecolor rgb "#66ff66", \
    "192_c6a.48xlarge/series/actors,mean.dat"   using 1:2:xtic(1) with linespoints  title "c6a.48xlarge (+RTS -N192)"   linecolor rgb "#009900", \
    "192_c6a.48xlarge/series/actors,mean.dat"   using 1:3:4 with filledcurves notitle linecolor rgb "#66ff66", \


set output "group_channels-mean.svg"
set title "Mean elapsed time vs Ring size (Channels)"
set ylabel "Seconds"
set xlabel "Nodes"
plot \
    "008_macbookpro11,5/series/channel ring,mean.dat" using 1:(s($2)) with linespoints  title "MacBookPro11,5 (+RTS -N8)" linecolor rgb "#000099", \
    "008_macbookpro11,5/series/channel ring,mean.dat" using 1:(s($3)):(s($4)) with filledcurves notitle linecolor rgb "#6666ff", \
    "032_c3.8xlarge/series/channels,mean.dat" using 1:2 with linespoints  title "c3.8xlarge (+RTS -N32)" linecolor rgb "#000099", \
    "032_c3.8xlarge/series/channels,mean.dat" using 1:3:4 with filledcurves notitle linecolor rgb "#6666ff", \
    "192_c6a.48xlarge/series/channels,mean.dat" using 1:2 with linespoints  title "c6a.48xlarge (+RTS -N192)" linecolor rgb "#000099", \
    "192_c6a.48xlarge/series/channels,mean.dat" using 1:3:4 with filledcurves notitle linecolor rgb "#6666ff", \


set output "group_control-mean.svg"
set title "Mean elapsed time vs Ring size (Control)"
set ylabel "Seconds"
set xlabel "Nodes"
plot \
    "008_macbookpro11,5/series/fork & kill,mean.dat"  using 1:(s($2)):xtic(1) with linespoints  title "MacBookPro11,5 (+RTS -N8)" linecolor rgb "#990000", \
    "008_macbookpro11,5/series/fork & kill,mean.dat"  using 1:(s($3)):(s($4)) with filledcurves notitle linecolor rgb "#ff6666", \
    "032_c3.8xlarge/series/control,mean.dat"  using 1:2:xtic(1) with linespoints  title "c3.8xlarge (+RTS -N32)"  linecolor rgb "#990000", \
    "032_c3.8xlarge/series/control,mean.dat"  using 1:3:4 with filledcurves notitle linecolor rgb "#ff6666", \
    "192_c6a.48xlarge/series/control,mean.dat"  using 1:2:xtic(1) with linespoints  title "c6a.48xlarge (+RTS -N192)"  linecolor rgb "#990000", \
    "192_c6a.48xlarge/series/control,mean.dat"  using 1:3:4 with filledcurves notitle linecolor rgb "#ff6666", \
