### :warning: This repo is no longer maintained
Beginning with [Cryptomator 1.6.0 Beta 1](https://github.com/cryptomator/cryptomator/releases/tag/1.6.0-beta1) we moved our installers to the [main repo](https://github.com/cryptomator/cryptomator/tree/develop/dist).

cryptomator-win
==================

This repository contains the scripts to build a windows installer for Cryptomator. Checkout and start `.\build.bat`.

## Requirements

* recent JDK installed and `JAVA_HOME` environment variable set pointing to its location
* innoSetup

## Remarks

The branch you are currently viewing is a feature branch changing the default build goal from exe (using innosetup) to msi (using [wix](https://wixtoolset.org/)). This section documents the this change.

With calling the powershell script `build.ps1` an msi-Installer can be build.
 It is not necessary to install the wix tool set beforehand, the script downloads and runs (not installs) a correct version.
 Current development status:
* the build msi artifact is branded under the name "MyTestApp" manufactured by "Cryptobot", such that it can be easily installed next to a normal Cryptomator installation and tested.
* the registry is not changed, except that an installation entry is added under HKCU
* Dokany or WinFSP are not included in the msi

Under `.\resources\wix` the necessary wix resource files are located.
 The file `properties.wxi` contains variables and definitions used in different wix resource files.
 The file `Product.wxs` contains the main "target", the Product tag.
 The wix file describing the files contained in the main application (`mainApp.wxs`) is not present but generated on the fly with the wix harvest tool [`heat`](https://wixtoolset.org/documentation/manual/v3/overview/heat.html). (see `build.ps1`)
 The file `heatPostProcessing.xsl` is a [XSLT](https://en.wikipedia.org/wiki/XSLT) file which is used during the execution of heat to add include statements and remove components which are handled elsewhere. 

The msi build process contains 4 steps:
1. Download the wix toolkit
2. Generate `mainApp.wxs` with the heat tool
3. Generate `*.wixobj` files with candle.exe
4. Generate the actual msi file with light.exe 

Warning: Currently the different files needed to generate the msi (wxs, wxi, xsl) dependet heavily onto each other, hence changing variable names should be done with caution!

For the development, the documentation of wix can be found here: https://wixtoolset.org/documentation/manual/v3/.