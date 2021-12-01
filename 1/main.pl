#!/usr/bin/perl

open(my $inputFile, "<", "input.txt") or die "$!";
local $/;
@data = split("\n", <$inputFile> );
$numberOfLines = @data;

$increases = 0;
$prevNumber = @data[0];
for $num (@data) {
  if ($num > $prevNumber) {
    $increases++;
  }
  $prevNumber = $num;
}

print "PartA: $increases\n";

$increases = 0;
$prevSum = @data[0] + @data[1] + @data[2];
for ( my $i = 0; $i < $numberOfLines-2; $i++ ) {
  $sum = @data[$i] + @data[$i+1] + @data[$i+2];
  if ($sum > $prevSum) {
    $increases++;
  }
  $prevSum = $sum;
}

print "PartB: $increases\n";