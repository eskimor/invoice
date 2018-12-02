# Simple setup for time clocking and invoicing
 
# Getting started

- Clone this repo 
- Copy src/Config-Template.hs to src/Config.hs
- Adjust settings in Config.hs to your needs.
- Copy over baseDirSkel to your baseDir.
- Modify invoice-template.tex in your baseDir appropriately.
- Install the tools with `nix-env -f ./default.nix -i`

# Usage

Now you have the following tools installed in your environment:

- `ti` ... For clocking in. Run `ti Project-name`
- `to` ... For clocking out. run `to "What have you done"`
- `timecamp` ... For reporting your clocked in time to timecamp. Run `timecamp`.
- `invoice` ... Creates an invoice in your billed directory, displays it and
  also reports to timecamp.
  
# Runtime dependencies

hledger and xelatex should be in path.


# Warnings

Config.hs contains your timecamp auth token, so make sure you never check it in. It is listed in .gitignore for this reason.
