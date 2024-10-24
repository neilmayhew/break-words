{ mkDerivation, base, containers, lib, optparse-applicative
, terminal-size, word-trie
}:
mkDerivation {
  pname = "break-words";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers optparse-applicative terminal-size word-trie
  ];
  description = "Find word breaks in text containing no spaces";
  license = lib.licenses.bsd3;
  mainProgram = "break-words";
}
