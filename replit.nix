{ pkgs }: {
    deps = [
        pkgs.busybox
        pkgs.leiningen
        pkgs.clojure
        pkgs.clojure-lsp
    ];
}