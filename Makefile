COMPILER = ghc -Wall -dynamic --make

DIR = src/
MAIN = main
MAIN_CAP = Main
IMPORT = Import

all: target clean

target: 
	$(COMPILER) $(MAIN_CAP).hs -o $(MAIN)

clean: 
	find . -name "*.o" -delete -o -name "*.hi" -delete

start:
	./$(MAIN)
