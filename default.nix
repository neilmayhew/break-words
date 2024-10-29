{ mkDerivation, base, containers, deepseq, graphviz, lib
, optparse-applicative, terminal-size, text, time, word-trie
}:
mkDerivation {
  pname = "break-words";
  version = "0.1.0.0";
  src = lib.cleanSource ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers deepseq graphviz optparse-applicative terminal-size
    text time word-trie
  ];
  description = "Find word breaks in text containing no spaces";
  license = lib.licenses.bsd3;
  mainProgram = "break-words";
}
