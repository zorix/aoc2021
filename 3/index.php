<?php
$input = file_get_contents("./input.txt");
$dataArr = explode("\n", $input);

$numberOfRows = count($dataArr);
$numberOfBits = strlen($dataArr[0]);

$arr = array();
for ($i = 0; $i < $numberOfBits; ++$i) {
  array_push($arr, 0);
}

function countNumberOfBitsAtPos($arr, $pos) {
  $size = count($arr);
  $count = 0;
  for ($i = 0; $i < $size; ++$i) {
    $record = $arr[$i];
    if ($record[$pos] == 1) {
      $count += 1;
    }
  }
  return $count;
}

for ($i = 0; $i < $numberOfBits; ++$i) {
  $arr[$i] = countNumberOfBitsAtPos($dataArr, $i);
}

$gamma = 0;
$epsilon = 0;

for ($j = 0; $j < $numberOfBits; ++$j) {
  if ($arr[$j] > ($numberOfRows/2)) {
    $gamma = $gamma | (1 << ($numberOfBits -1 - $j));
  } else {
    $epsilon = $epsilon | (1 << ($numberOfBits -1 - $j));
  }
}

$powerConsumption = $gamma * $epsilon;
printf("power: %s\n", $powerConsumption);

function part2($arr, $idx, $takeGreater) {
  $size = count($arr);
  if ($size < 2) {
    printf("%s\n", $arr[0]);
    return $arr[0];
  }

  $count = countNumberOfBitsAtPos($arr, $idx);
  if (($count >= (count($arr)/2)) == $takeGreater) {
    $subArr = array_filter($arr, function($v) use ($idx) {
      return $v[$idx] == '1';
    });
  } else {
    $subArr = array_filter($arr, function($v) use ($idx) {
      return $v[$idx] == '0';
    });
  }
  $subArr = array_values($subArr);
  #printf("%s %s\n", $idx, count($subArr));
  return part2($subArr, $idx+1, $takeGreater);
}

$oxygen = bindec(part2($dataArr, 0, true));
$co2 = bindec(part2($dataArr, 0, false));

printf("x %s\n", $oxygen * $co2);
