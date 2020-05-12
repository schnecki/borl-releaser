
gnuplot -e "set key autotitle columnhead; plot for [col=2:2] 'episodeLength' using 0:col with points; pause mouse close; " &
gnuplot -e "set key autotitle columnhead; plot for [col=2:3] 'plts' using 0:col with points; pause mouse close; " &
gnuplot -e "set key autotitle columnhead; plot for [col=2:6] 'stateValues' using 0:col with lines; pause mouse close; " &
gnuplot -e "set key autotitle columnhead; plot for [col=7:8] 'stateValues' using 0:col with points; pause mouse close; " &
gnuplot -e "set key autotitle columnhead; plot for [col=2:3] 'costs' using 0:col with points; pause mouse close; " &
gnuplot -e "set key autotitle columnhead; plot for [col=2:2] 'reward' using 0:col with points; pause mouse close; " &

NR="`head -n1 stateValuesAllStatesCount`"
if [ $? -eq 0 ]; then
    gnuplot -e "set key autotitle columnhead; plot for [col=2:$((NR+1))] 'stateVAllStates' using 1:col with lines; set key title 'All V Values'; pause mouse close; " &
    gnuplot -e "set key autotitle columnhead; plot for [col=2:$((NR+1))] 'stateWAllStates' using 1:col with lines; set key title 'All W Values'; pause mouse close; " &
    gnuplot -e "set key autotitle columnhead; plot for [col=2:$((NR+1))] 'statePsiVAllStates' using 1:col with lines; set key title 'All Psi V Values'; pause mouse close; " &
    gnuplot -e "set key autotitle columnhead; plot for [col=2:$((NR+1))] 'statePsiWAllStates' using 1:col with lines; set key title 'All Psi W Values'; pause mouse close; " &
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
