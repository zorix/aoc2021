#!/bin/bash

declare -a matrix=
size=10
offset=100

i=0
for line in $(cat input.txt); do
  j=0
  for c in $(echo "$line" | fold -w1); do
    matrix[$j*$offset+$i]=$c
    let "j++"
  done
  let "i++"
done
flashes=0

update_energy() {
  #echo "args" $1 $2 $3 $4
  local start_i=$( (( $1 >= 0 )) && echo "$1" || echo "0" )
  local end_i=$( (( $2 <= 9 )) && echo "$2" || echo 9 )
  local start_j=$( (( $3 >= 0 )) && echo "$3" || echo "0" )
  local end_j=$( (( $4 <= 9 )) && echo "$4" || echo 9 )
  #echo "idxs" $start_i $end_i $start_j $end_j
  for ((i=$start_i;i<=$end_i;++i)); do
    for ((j=$start_j;j<=$end_j;++j)); do
      let "matrix[$j*$offset+$i]+=1"
    done
  done
}


check_flash() {
  local res=0
  for ((i=0;i<$size;++i)); do
    for ((j=0;j<$size;++j)); do
      if (( matrix[$j*$offset+$i] >= 10 && flashed[$j*$offset+$i] == 0 )); then
        let "matrix[$j*$offset+$i]=0"
        let "flashed[$j*$offset+$i]=1"
        let "++flashes"
        update_energy $(echo $(($i-1))) $(echo $(($i+1))) $(echo $(($j-1))) $(echo $(($j+1)))
        #echo "$j*$offset+$i"
        res=1
      fi
    done
  done
  return $res
}


step() {
  for ((i=0;i<$size;++i)); do
    for ((j=0;j<$size;++j)); do
      flashed[$j*$offset+$i]=0
    done
  done

  update_energy 0 9 0 9

  while : ; do
    check_flash
    res=$?
    #echo "res" $res
    if (("$res" == 0)); then
      break
    fi
  done

  #echo "flashed  " ${flashed[*]}
  keep_work=0
  for ((i=0;i<$size;++i)); do
    for ((j=0;j<$size;++j)); do
      if ((flashed[$j*$offset+$i] == 1)); then
        matrix[$j*$offset+$i]=0
      else
        keep_work=1
      fi
    done
  done
  #echo "matrix   " ${matrix[*]}

  #echo ''
}

declare -A flashed

keep_work=1
for ((x=0;$keep_work;)); do
  let "++x"
  echo "step: $x"
  step
done

echo "partB $x"
