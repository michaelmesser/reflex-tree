{ mkDerivation, base, binary, bytestring, constraints, containers
, dependent-map, dependent-sum, dependent-sum-template
, freer-effects, ghc-mod, ghcjs-dom, jsaddle-wkwebview, lens
, linear, reflex, reflex-dom, safe, stdenv, template-haskell, text
, these, transformers, vinyl
}:
mkDerivation {
  pname = "reflex-test";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base binary bytestring constraints containers dependent-map
    dependent-sum dependent-sum-template freer-effects ghc-mod
    ghcjs-dom jsaddle-wkwebview lens linear reflex reflex-dom safe
    template-haskell text these transformers vinyl
  ];
  license = stdenv.lib.licenses.unfree;
}
