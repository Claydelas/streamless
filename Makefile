# Files that need to be generated from other files
DEPEND += Tokens.hs Grammar.hs Eval.hs

all: $(DEPEND) myinterpreter

# Build an executable for Toy interpreter
myinterpreter: $(DEPEND) myinterpreter.hs
	ghc myinterpreter.hs

# Generate ML files from a parser definition file
Grammar.hs : Grammar.y
	@rm -f Grammar.hs
	happy Grammar.y
	@chmod -w Grammar.hs

# Generate ML files from a lexer definition file
Tokens.hs : Tokens.x
	@rm -f Tokens.hs
	alex Tokens.x
	@chmod -w Tokens.hs

# Clean up the directory
clean::
	rm -rf Tokens.hs Grammar.hs *.hi *.o *.info *.exe


