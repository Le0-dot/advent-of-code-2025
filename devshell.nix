{ pkgs }:
pkgs.mkShell {
  packages = [
    pkgs.nil
    pkgs.leiningen
    pkgs.clojure-lsp
  ];
}
