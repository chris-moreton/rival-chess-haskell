cutechess-cli -engine cmd=exe/rival-$1.exe st=25 -engine cmd="java -jar /home/chris/Chess/rivalchess-43.0.5-1.jar" st=0.1 -each proto=uci timemargin=15000 book=exe/ProDeo.bin -rounds 100 -pgnout "out.pgn" 


