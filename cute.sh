cutechess-cli -engine cmd=dist-newstyle/build/x86_64-linux/ghc-8.10.4/rival-0.1.0.0/x/rival-exe/build/rival-exe/rival-exe nodes=1000 -engine cmd=exe/rival-$1.exe -each st=0.01 proto=uci timemargin=1500 book=exe/ProDeo.bin -rounds 100 -pgnout "out.pgn" -debug

