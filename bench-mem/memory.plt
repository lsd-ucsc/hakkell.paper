set datafile separator ','
set terminal svg size 620,465
set style fill transparent solid 0.2 noborder
set logscale y 2
set logscale x 2
set key top center
#set xtics rotate by 10 center offset 0,-0.5

set xrange [:65536]

# convert bytes to megabytes
mb(b) = b / (2**20)

set title "Allocations vs Ring size (MacBookPro11,5; +RTS -N8)"
set xlabel "Nodes"
set ylabel "Mebibytes"
set output "total-allocated.svg"
plot \
    "008_macbookpro11,5/series/control,bytes allocated.dat"  using 1:(mb($2))  with linespoints  title "control"  linecolor rgb "#990000", \
    "008_macbookpro11,5/series/actors,bytes allocated.dat"   using 1:(mb($2))  with linespoints  title "actors"   linecolor rgb "#009900", \
    "008_macbookpro11,5/series/channels,bytes allocated.dat" using 1:(mb($2))  with linespoints  title "channels" linecolor rgb "#000099", \
    "008_macbookpro11,5/series/control,bytes allocated.dat"  using 1:(mb($3)):(mb($4)) with filledcurves notitle linecolor rgb "#ff6666", \
    "008_macbookpro11,5/series/actors,bytes allocated.dat"   using 1:(mb($3)):(mb($4)) with filledcurves notitle linecolor rgb "#66ff66", \
    "008_macbookpro11,5/series/channels,bytes allocated.dat" using 1:(mb($3)):(mb($4)) with filledcurves notitle linecolor rgb "#6666ff", \


set title "Live data sampled during GCs vs Ring size"
set xlabel "Nodes"
set ylabel "Mebibytes"
set output "residency.svg"
plot \
    "008_macbookpro11,5/series/control,average_bytes_used.dat"  using 1:(mb($2))  with linespoints  title "control (average over run)"  linecolor rgb "#990000", \
    "008_macbookpro11,5/series/actors,average_bytes_used.dat"   using 1:(mb($2))  with linespoints  title "actors (average over run)"   linecolor rgb "#009900", \
    "008_macbookpro11,5/series/channels,average_bytes_used.dat" using 1:(mb($2))  with linespoints  title "channels (average over run)" linecolor rgb "#000099", \
    "008_macbookpro11,5/series/control,average_bytes_used.dat"  using 1:(mb($3)):(mb($4)) with filledcurves notitle linecolor rgb "#ff6666", \
    "008_macbookpro11,5/series/actors,average_bytes_used.dat"   using 1:(mb($3)):(mb($4)) with filledcurves notitle linecolor rgb "#66ff66", \
    "008_macbookpro11,5/series/channels,average_bytes_used.dat" using 1:(mb($3)):(mb($4)) with filledcurves notitle linecolor rgb "#6666ff", \
    "008_macbookpro11,5/series/control,max_bytes_used.dat"  using 1:(mb($2))  with linespoints  title "control (max over run)"  linecolor rgb "#990000", \
    "008_macbookpro11,5/series/actors,max_bytes_used.dat"   using 1:(mb($2))  with linespoints  title "actors (max over run)"   linecolor rgb "#009900", \
    "008_macbookpro11,5/series/channels,max_bytes_used.dat" using 1:(mb($2))  with linespoints  title "channels (max over run)" linecolor rgb "#000099", \
    "008_macbookpro11,5/series/control,max_bytes_used.dat"  using 1:(mb($3)):(mb($4)) with filledcurves notitle linecolor rgb "#ff6666", \
    "008_macbookpro11,5/series/actors,max_bytes_used.dat"   using 1:(mb($3)):(mb($4)) with filledcurves notitle linecolor rgb "#66ff66", \
    "008_macbookpro11,5/series/channels,max_bytes_used.dat" using 1:(mb($3)):(mb($4)) with filledcurves notitle linecolor rgb "#6666ff", \


set title "Number of GCs during whole program run vs Ring size"
set xlabel "Nodes"
set ylabel "Count"
set output "gc.svg"
plot \
    "008_macbookpro11,5/series/control,num_GCs.dat"  using 1:2 with linespoints  title "control (num GCs)"  linecolor rgb "#990000", \
    "008_macbookpro11,5/series/actors,num_GCs.dat"   using 1:2 with linespoints  title "actors (num GCs)"   linecolor rgb "#009900", \
    "008_macbookpro11,5/series/channels,num_GCs.dat" using 1:2 with linespoints  title "channels (num GCs)" linecolor rgb "#000099", \
    "008_macbookpro11,5/series/control,num_GCs.dat"  using 1:3:4       with filledcurves notitle linecolor rgb "#ff6666", \
    "008_macbookpro11,5/series/actors,num_GCs.dat"   using 1:3:4       with filledcurves notitle linecolor rgb "#66ff66", \
    "008_macbookpro11,5/series/channels,num_GCs.dat" using 1:3:4       with filledcurves notitle linecolor rgb "#6666ff", \
    "008_macbookpro11,5/series/control,num_byte_usage_samples.dat"  using 1:2 with linespoints  title "control (Live data samples)"  linecolor rgb "#990000", \
    "008_macbookpro11,5/series/actors,num_byte_usage_samples.dat"   using 1:2 with linespoints  title "actors (Live data samples)"   linecolor rgb "#009900", \
    "008_macbookpro11,5/series/channels,num_byte_usage_samples.dat" using 1:2 with linespoints  title "channels (Live data samples)" linecolor rgb "#000099", \
    "008_macbookpro11,5/series/control,num_byte_usage_samples.dat"  using 1:3:4       with filledcurves notitle linecolor rgb "#ff6666", \
    "008_macbookpro11,5/series/actors,num_byte_usage_samples.dat"   using 1:3:4       with filledcurves notitle linecolor rgb "#66ff66", \
    "008_macbookpro11,5/series/channels,num_byte_usage_samples.dat" using 1:3:4       with filledcurves notitle linecolor rgb "#6666ff", \


set title "Peak RTS allocation from OS vs Ring size"
set xlabel "Nodes"
set ylabel "Megabytes"
set output "peak.svg"
plot \
    "008_macbookpro11,5/series/control,peak_megabytes_allocated.dat"  using 1:2 with linespoints  title "control"  linecolor rgb "#990000", \
    "008_macbookpro11,5/series/actors,peak_megabytes_allocated.dat"   using 1:2 with linespoints  title "actors"   linecolor rgb "#009900", \
    "008_macbookpro11,5/series/channels,peak_megabytes_allocated.dat" using 1:2 with linespoints  title "channels" linecolor rgb "#000099", \
    "008_macbookpro11,5/series/control,peak_megabytes_allocated.dat"  using 1:3:4       with filledcurves notitle linecolor rgb "#ff6666", \
    "008_macbookpro11,5/series/actors,peak_megabytes_allocated.dat"   using 1:3:4       with filledcurves notitle linecolor rgb "#66ff66", \
    "008_macbookpro11,5/series/channels,peak_megabytes_allocated.dat" using 1:3:4       with filledcurves notitle linecolor rgb "#6666ff", \
