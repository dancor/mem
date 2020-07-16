mem: src/Main.hs
	mkdir -p build; ghc -o mem -hidir build -odir build --make src/Main.hs
