<?php

$lines = file("MagicMovesRook.hs");

$count = 0;
foreach ($lines as $line) {
	if (strpos($line, "V.fromList") !== false) {
        $leftBracket = strpos($line, "[");
        $rightBracket = strpos($line, "]");
        $len = $rightBracket - $leftBracket - 1;
        $csv = substr($line, $leftBracket + 1, $len);
        $parts = explode(',', $csv);
        $count2 = 0;
        foreach ($parts as $part) {
            print "magicMovesBishop $count $count2 = $part" . PHP_EOL;
            $count2 ++;
        }
        $count ++;
    }
}
