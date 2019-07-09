

module Releaser.Util where


gnuplot :: String
gnuplot = "FILES=\"\"                                             \n" ++
          "                                                       \n" ++
          "for arg in $@; do                                      \n" ++
          "     FILES=\"$FILES `find . -type f -name \"*$arg*\"`\"\n" ++
          "done                                                   \n" ++
          "                                                       \n" ++
          "echo $FILES                                            \n" ++
          "                                                       \n" ++
          "ARRAY=($FILES)                                         \n"++
          "for col in {2,3,4}; do                                 \n"++
          "    CMD=\"set key autotitle columnhead; plot \"        \n"++
          "    for f in $FILES; do                                \n"++
          "        echo $f                                        \n"++
          "        CMD=\"$CMD '$f' using 0:$col with lines \"     \n"++
          "        if [ \"$f\" != \"${ARRAY[-1]}\" ]; then        \n"++
          "            CMD=\"$CMD, \"                             \n"++
          "        fi                                             \n"++
          "    done                                               \n"++
          "    CMD=\"$CMD; pause mouse close; \"                  \n"++
          "    echo $CMD                                          \n"++
          "    gnuplot -e \"$CMD\" &                              \n"++
          "done                                                   \n"

