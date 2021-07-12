cutechess-cli -engine cmd=exe/rival-$1.exe -engine cmd="java -jar /home/chris/Chess/rivalchess-43.0.5-1.jar" -each st=1 proto=uci timemargin=15000 book=exe/ProDeo.bin -rounds 100 -pgnout "out.pgn" 


