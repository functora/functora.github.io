let
  src = fetchTarball {
    url = "https://github.com/functora/functora.github.io/archive/53356131206ffdc2eae2f1a18f55333ef3465639.tar.gz";
    sha256 = "1qsg53kakcjpk1qn2vwj90ypwynqw6zvj224664498a6mxl6j868";
  };
  prj = import "${src}/nix/project.nix" {};
in
  prj.proto-lens-protoc.components.exes.proto-lens-protoc
