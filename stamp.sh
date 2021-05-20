sed -i "s/Rival Haskell Build -/Rival Haskell Build $1/g" app/Main.hs
cabal build
cp dist-newstyle/build/x86_64-linux/ghc-8.10.4/rival-0.1.0.0/x/rival-exe/build/rival-exe/rival-exe exe/rival-$1.exe
sed -i "s/Rival Haskell Build $1/Rival Haskell Build -/g" app/Main.hs
