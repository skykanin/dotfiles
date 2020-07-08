{ stdenv, fetchFromGitHub, makeWrapper, clang, chez }:

# Uses scheme to bootstrap the build of idris2
stdenv.mkDerivation rec {
  name = "idris2-master";

  src = fetchFromGitHub {
    owner = "idris-lang";
    repo = "Idris2";
    rev = "master";
    sha256 = "12af7c3zh2byqywjndgx9rdkfa78rgdzv2pmfnq8g42a2w9qk9ld";
  };

  strictDeps = true;
  nativeBuildInputs = [ makeWrapper clang chez ];
  buildInputs = [ chez ];

  prePatch = ''
    patchShebangs --build tests
  '';

  makeFlags = [ "PREFIX=$(out)" ]
    ++ stdenv.lib.optional stdenv.isDarwin "OS=";

  # The name of the main executable of pkgs.chez is `scheme`
  buildFlags = [ "bootstrap-build" "SCHEME=scheme" ];

  checkTarget = "bootstrap-test";

  # idris2 needs to find scheme at runtime to compile
  postInstall = ''
    wrapProgram "$out/bin/idris2" --set CHEZ "${chez}/bin/scheme"
  '';

  meta = {
    description = "A purely functional programming language with first class types";
    homepage = "https://github.com/idris-lang/Idris2";
    license = stdenv.lib.licenses.bsd3;
    maintainers = with stdenv.lib.maintainers; [ wchresta ];
    inherit (chez.meta) platforms;
  };
}
