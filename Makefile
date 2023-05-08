interpreter:
	ghc --make Main.hs -o interpreter

clean:
	rm *.o *.hi interpreter/*.o interpreter/*.hi Main
