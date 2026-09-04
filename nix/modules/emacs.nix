{pkgs, ...}: {
  services.emacs = {
    enable = true;
    package = let
      emacsPackage =
        if pkgs.stdenv.hostPlatform.isLinux
        then pkgs.emacs-pgtk
        else pkgs.emacs-macport;
      svelteGrammar = pkgs.tree-sitter-grammars.tree-sitter-svelte.overrideAttrs {
        version = "0-unstable-2024-09-07";
        src = pkgs.fetchFromGitHub {
          owner = "tree-sitter-grammars";
          repo = "tree-sitter-svelte";
          rev = "6fe714f0c9026a986b916bf39c1021ffcb1e995b";
          hash = "sha256-8JqA+2BzIYUYXRh0d3vdjR1Ghj1qbZyIOmRC7aXBe7Q=";
        };
      };
    in
      emacsPackage.pkgs.emacsWithPackages (epkgs: [
        pkgs.stdenv.cc
        epkgs.vterm
        (epkgs.treesit-grammars.with-grammars (grammars:
          [svelteGrammar]
          ++ builtins.attrValues (builtins.removeAttrs grammars ["tree-sitter-svelte"])))
      ]);
  };
}
