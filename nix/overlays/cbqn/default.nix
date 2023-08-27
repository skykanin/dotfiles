# Update CBQN to newer commit so that we get the latest features
final: prev: {
  cbqn = prev.cbqn.overrideAttrs (oldAttrs: {
    version = "0.pre+date=2023-02-01";
    src = final.fetchFromGitHub {
      owner = "dzaima";
      repo = "CBQN";
      rev = "05c1270344908e98c9f2d06b3671c3646f8634c3";
      sha256 = "wKeyYWMgTZPr+Ienz3xnsXeD67vwdK4sXbQlW+GpQho=";
    };
  });
}
