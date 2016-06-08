grep "Elapsed time" | awk '{print $3}' | awk '{s+=$1}END{print "mean:",s/NR}' RS=" "
