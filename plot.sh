
FONTSCALE=0.5
FONTSCALE2=`echo $FONTSCALE*2 | bc`
# INIT_GNUPLOT="set terminal wxt lw $FONTSCALE; set terminal wxt fontscale $FONTSCALE"
INIT_GNUPLOT="set terminal wxt lw $FONTSCALE"
INIT_GNUPLOT_LINES="set terminal wxt lw $FONTSCALE2"

function age() {
   local filename=$1
   local changed=`stat -c %Y "$filename"`
   local now=`date +%s`
   local elapsed

   let elapsed=now-changed
   echo $elapsed
}

agents="`head -n1 stateValuesAgents`"

echo "AGENTS: $agents"

gnuplot -e "$INIT_GNUPLOT; set key autotitle columnhead; plot for [col=2:2] 'episodeLength' using 0:col with points; pause mouse close; " &

START=2
END=$((START+agents))
START2=$((3+agents*2))
END2=$((START2+agents-1))
echo "col=$START:$END"
gnuplot -e "$INIT_GNUPLOT_LINES; set key autotitle columnhead; plot for [col=$START:$END] 'stateValues' using 0:col with lines, for [col2=$START2:$END2] 'stateValues' using 0:col2 with lines; pause mouse close; " &

START=2
END=$((START+agents*4))
echo "col=$START:$END"
gnuplot -e "$INIT_GNUPLOT_LINES; set key autotitle columnhead; plot for [col=$START:$END] 'stateValues' using 0:col with lines; pause mouse close; " &

START=$((END+1))
END=$((START+agents*2-1))
START2=$((END+agents*2+1))
END2=$((START2+1))
echo "col=$START:$END"
# gnuplot -e "$INIT_GNUPLOT; set key autotitle columnhead; plot for [col=$START:$END] 'stateValues' using 0:col with points; pause mouse close; " &
gnuplot -e "$INIT_GNUPLOT; set key autotitle columnhead; plot for [col=$START:$END] 'stateValues' using 0:col with points, for [col2=$START2:$END2] 'stateValues' using 0:col2 with lines; pause mouse close; " &


START=$((END+1))
END=$((START+agents*2-1))
echo "col=$START:$END"
gnuplot -e "$INIT_GNUPLOT; set key autotitle columnhead; plot for [col=$START:$END] 'stateValues' using 0:col with points; pause mouse close; " &

gnuplot -e "$INIT_GNUPLOT; set key autotitle columnhead; plot for [col=2:3] 'costs' using 0:col with points; pause mouse close; " &
gnuplot -e "$INIT_GNUPLOT; set key autotitle columnhead; plot for [col=2:2] 'reward' using 0:col with points; pause mouse close; " &

# if [[ $(age "$file") < 300 ]];
# then
NR="`head -n1 stateValuesAllStatesCount`"
MAX=100
if [ $? -eq 0 ] && [[ "$NR" < "$MAX" ]]; then
    # NR="`head -n1 stateValuesAllStatesCount`"
    gnuplot -e "$INIT_GNUPLOT_LINES; set key autotitle columnhead; plot for [col=2:$((NR+1))] 'stateVAllStates' using 1:col with lines; set key title 'All V Values'; pause mouse close; " &
    gnuplot -e "$INIT_GNUPLOT_LINES; set key autotitle columnhead; plot for [col=2:$((NR+1))] 'stateVAllStates_scaled' using 1:col with lines; set key title 'All V Values (Scaled)'; pause mouse close; " &
    gnuplot -e "$INIT_GNUPLOT_LINES; set key autotitle columnhead; plot for [col=2:$((NR+1))] 'stateWAllStates' using 1:col with lines; set key title 'All W Values'; pause mouse close; " &
    gnuplot -e "$INIT_GNUPLOT_LINES; set key autotitle columnhead; plot for [col=2:$((NR+1))] 'statePsiVAllStates' using 1:col with lines; set key title 'All Psi V Values'; pause mouse close; " &
    gnuplot -e "$INIT_GNUPLOT_LINES; set key autotitle columnhead; plot for [col=2:$((NR+1))] 'statePsiWAllStates' using 1:col with lines; set key title 'All Psi W Values'; pause mouse close; " &
fi

# set term wxt 0
# plot for [col=2:2] 'episodeLength' using 0:col with points
# set term wxt 1
# plot for [col=2:6] 'stateValues' using 0:col with lines
# pause mouse close

# DELETE ALL CONTENT OF THE FILES
# sed -i -n '1p' {episodeLength,plts,stateValues,costs,reward,stateVAllStates,stateWAllStates,statePsiVAllStates,statePsiWAllStates}

# WATCH PERFORMANCE
# watch 'pr -m -t reward costs | tail -n 1000 - | awk "{ sum += \$2; sum4 += \$4; n++ } END { if (n > 0) print (sum / n, sum4 /n) ; }"; pr -m -t reward costs | tail -n 10000 - | awk "{ sum += \$2; sum4 += \$4; n++ } END { if (n > 0) print (sum / n, sum4 /n) ; }"; pr -m -t reward costs | tail -n 100000 - | awk "{ sum += \$2; sum4 += \$4; n++ } END { if (n > 0) print (sum / n, sum4 /n) ; }"'
