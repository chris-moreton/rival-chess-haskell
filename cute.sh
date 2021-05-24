cutechess-cli -engine cmd=exe/rival-$1.exe nodes=1000 -engine cmd=exe/rival-$2.exe -each depth=$4 st=$5 proto=uci timemargin=15000 book=exe/ProDeo.bin -rounds $3 -pgnout "out.pgn"

