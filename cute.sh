cutechess-cli -engine cmd=exe/rival-$1.exe nodes=1000 -engine cmd=exe/rival-$2.exe -each st=0.01 proto=uci timemargin=1500 book=exe/ProDeo.bin -rounds 1 -pgnout "out.pgn" -debug

