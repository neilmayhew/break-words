Find word breaks in text containing no spaces using a dictionary in the form of a `Trie` (from [word-trie](https://hackage.haskell.org/package/word-trie)).

The algorithm is in the `BreakWords` module and there's a main program in `Main` that provides options for specifying a dictionary file, files of text to process, and style of output.

The results are returned as a `Forest String`. The algorithm runs surprisingly quickly with a reasonable sized dictionary and ~300 characters of input, including the time taken to pre-process the dictionary into a `Trie`.

The output is produced lazily so output of the first few parses is almost instantaneous. The algorithm orders the results using longest-match first, so the first few parses are likely to include the one you want.

However, be warned that even relatively small inputs can produce a great many possible parses (eg >100,000) so the output needs to be piped into a pager. The number of parses is shown at the top of the output.

There's a `--demo` option that uses a small predefined input to give a feel for what the program does.

You will need to supply a word-list file, with one word per line, similar to the `/usr/share/dict/words` that's typically present on Unix systems. However, it will need to be converted to UTF-8 (eg using `iconv`) if it isn't already UTF-8. Also, if there are multiple alternatives for this file it's best to use one that's not too big. Adding obscure words to the dictionary can greatly increase the number of possible parses into the tens of millions.
