{ lib
, rustPlatform
, libspike
, libspike_interfaces
, rtlDesignMetadata
}:

{ outputName
, emuType ? ""
, moduleType
, enableTrace ? false
}:

rustPlatform.buildRustPackage {
  name = outputName;
  src = with lib.fileset; toSource {
    root = ./.;
    fileset = unions [
      ./spike_rs
      ./offline
      ./Cargo.lock
      ./Cargo.toml
      ./.rustfmt.toml
    ];
  };

  buildAndTestSubdir = "./${moduleType}";

  env = {
    SPIKE_LIB_DIR = "${libspike}/lib";
    SPIKE_INTERFACES_LIB_DIR = "${libspike_interfaces}/lib";
    SPIKE_ISA_STRING = rtlDesignMetadata.march;
  };

  cargoLock = {
    lockFile = ./Cargo.lock;
  };

  passthru = {
    dpiLibPath = "/lib/libdpi_${moduleType}.a";
    inherit enableTrace;
  };
}
