DICT := ~/.nix-profile/share/dict/wamerican
OUTPUTS := demo-output.dot sample1-output.dot sample2-output.dot

all: $(OUTPUTS)

words-%.txt: $(DICT).%
	iconv -f iso-8859-1 -t utf-8 <$< >$@

demo-output.dot: words-35.txt
	cabal run -- break-words -v -o $@ -w $^ --demo

sample1-output.dot: words-35.txt sample1-input.txt
	cabal run -- break-words -v -o $@ -w $^

sample2-output.dot: words-40.txt sample2-input.txt
	cabal run -- break-words -v -o $@ -w $^

clean:
	$(RM) $(OUTPUTS) words-35.txt words-40.txt

check:
	$(RM) -r test
	cp -a regression test
	cd test && rm -f $(OUTPUTS)
	$(MAKE) -C test -f ../Makefile
	diff -ru regression test
	$(RM) -r test
