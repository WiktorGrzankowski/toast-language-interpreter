interpreter:
	ghc --make Main.hs -o interpreter

clean:
	rm *.o *.hi Toast/*.o Toast/*.hi interpreter
