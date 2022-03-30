nativePkgs:
with nativePkgs.pkgsCross; {
  # x86-gnu32 = gnu32;
  #x86-gnu64 = nativePkgs; #gnu64; # should be == nativePkgs
  # x86-musl32 = musl32;
  x86-musl64 = musl64;
  #x86-win64 = mingwW64;
  #rpi1-gnu = raspberryPi;
  #rpi1-musl = muslpi;
  #rpi32-gnu = armv7l-hf-multiplatform;
  # sadly this one is missing from the nixpkgs system examples
  #rpi32-musl = import nixpkgsSrc (nativePkgs.lib.recursiveUpdate nixpkgsArgs
    #{ crossSystem = nativePkgs.lib.systems.examples.armv7l-hf-multiplatform
                  #// { config = "armv7l-unknown-linux-musleabihf"; }; });
  #rpi64-gnu = aarch64-multiplatform;
  #rpi64-musl = aarch64-multiplatform-musl;
}
