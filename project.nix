{ mkDerivation, fetchFromGitHub , aeson, base, bytestring, conduit, conduit-extra
, containers, directory, DOH, filemanip, filepath, hpack
, http-client, http-client-tls, http-conduit, http-types, ini
, microlens, microlens-platform, mtl, optparse-applicative
, pretty-simple, rio, stdenv, tar-conduit, text, typed-process
, unix, unordered-containers, yaml
}:
let
  gitignoreSrc = fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore";
    # put the latest commit sha of gitignore Nix library here:
    rev = "f9e996052b5af4032fe6150bba4a6fe4f7b9d698";
    # use what nix suggests in the mismatch message here:
    sha256 = "sha256:0jrh5ghisaqdd0vldbywags20m2cxpkbbk5jjjmwaw0gr8nhsafv";
  };
  inherit (import gitignoreSrc {}) gitignoreSource;
in
mkDerivation {
  pname = "forge";
  version = "0.3.2.0";
  src = gitignoreSource ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring conduit conduit-extra containers directory
    DOH filemanip filepath http-client http-client-tls http-conduit
    http-types ini microlens microlens-platform mtl
    optparse-applicative pretty-simple rio tar-conduit text
    typed-process unix unordered-containers yaml
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring conduit conduit-extra containers directory
    DOH filemanip filepath http-client http-client-tls http-conduit
    http-types ini microlens microlens-platform mtl
    optparse-applicative pretty-simple rio tar-conduit text
    typed-process unix unordered-containers yaml
  ];
  testHaskellDepends = [
    aeson base bytestring conduit conduit-extra containers directory
    DOH filemanip filepath http-client http-client-tls http-conduit
    http-types ini microlens microlens-platform mtl
    optparse-applicative pretty-simple rio tar-conduit text
    typed-process unix unordered-containers yaml
  ];
  doHaddock = false;
  enableSharedExecutables = false;
  enableLibraryProfiling = false;
  prePatch = "hpack";
  homepage = "https://github.com/denibertovic/forge#readme";
  license = stdenv.lib.licenses.lgpl3;
}
