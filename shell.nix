{ ghcName ? null, pkgs ? import <nixpkgs> { } }:

let
  haskellPackages = if (ghcName != null) then
    pkgs.haskell.packages.${ghcName}
  else
    pkgs.haskellPackages;

  ghc = haskellPackages.ghcWithHoogle (h:
    with h; [
      aeson
      brick
      containers
      cursor
      filepath
      megaparsec
      mtl
      parser-combinators
      process
      text
      vty
      vty-unix
    ]);

in pkgs.mkShell {
  buildInputs = with pkgs; [
    cabal-install
    gdb
    ghc
    haskellPackages.haskell-language-server
    hlint
    stylish-haskell
  ];
}
