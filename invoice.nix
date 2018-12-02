{ mkDerivation, aeson, base, bytestring, containers, data-default
, data-default-class, Decimal, directory, filepath, hledger-lib
, lens, lens-aeson, process, req, stdenv, text, time
, unordered-containers, vector
}:
mkDerivation {
  pname = "invoice";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers data-default data-default-class
    Decimal directory filepath hledger-lib lens lens-aeson process req
    text time unordered-containers vector
  ];
  description = "Create invoices from timeclock data";
  license = stdenv.lib.licenses.gpl3;
}
