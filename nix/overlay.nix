# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2024 Jiuyang Liu <liu@jiuyang.me>

final: prev: {
  espresso = final.callPackage ./pkgs/espresso.nix { };
  mill =
    let jre = final.jdk21;
    in (prev.mill.override { inherit jre; }).overrideAttrs
      (_: { passthru = { inherit jre; }; });
  fetchMillDeps = final.callPackage ./pkgs/mill-builder.nix { };
  circt-full = final.callPackage ./pkgs/circt-full.nix { };
  add-determinism =
    final.callPackage ./pkgs/add-determinism { }; # faster strip-undetereminism

  # Using VCS need to set VC_STATIC_HOME and SNPSLMD_LICENSE_FILE to impure env, and add sandbox dir to VC_STATIC_HOME
  # Remember to add "--impure" flag for nix to read this value from environment
  vcStaticHome = builtins.getEnv "VC_STATIC_HOME";
  snpslmdLicenseFile = builtins.getEnv "SNPSLMD_LICENSE_FILE";
  vcs-fhs-env = assert final.lib.assertMsg (final.vcStaticHome != "")
    "You forget to set VC_STATIC_HOME or the '--impure' flag";
    assert final.lib.assertMsg (final.snpslmdLicenseFile != "")
      "You forget to set SNPSLMD_LICENSE_FILE or the '--impure' flag";
    final.callPackage ./pkgs/vcs-fhs-env.nix { };
  jasperHome = builtins.getEnv "JASPER_HOME";
  cdsLicenseFile = builtins.getEnv "CDS_LIC_FILE";
  cds-fhs-env = assert final.lib.assertMsg (final.jasperHome != "")
    "You forget to set JASPER_HOME or the '--impure' flag";
    assert final.lib.assertMsg (final.cdsLicenseFile != "")
      "You forget to set CDS_LIC_FILE or the '--impure' flag";
    final.callPackage ./pkgs/cds-fhs-env.nix { };


  projectDependencies = final.callPackage ./pkgs/project-dependencies.nix { };
  riscv-tests = final.pkgsCross.riscv32-embedded.stdenv.mkDerivation rec {
    pname = "riscv-tests";
    version = "7878085d2546af0eb7af72a1df00996d5d8c43fb";
    src = final.fetchFromGitHub {
      owner = "riscv-software-src";
      repo = "riscv-tests";
      rev = "${version}";
      hash = "sha256-3SUfmUHwvEG4Fi6YWLLhzMhASyL07euMmkIoc9leYFE=";
      fetchSubmodules = true;
    };

    postUnpack = ''
      rm -rf $sourceRoot/env
      cp -r ${../nix/dependencies/riscv-test-env} $sourceRoot/env
    '';

    enableParallelBuilding = true;

    configureFlags = [
      # to match rocket-tools path
      "--prefix=${placeholder "out"}/riscv32-unknown-elf"
    ];
    buildPhase = "make RISCV_PREFIX=riscv32-none-elf-";
    installPhase = ''
      runHook preInstall
      make install
      runHook postInstall
    '';
  };

  hia = final.callPackage ./hia { };
}
