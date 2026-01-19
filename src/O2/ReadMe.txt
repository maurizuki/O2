O2 3.3 Copyright (C) 2004-2026 Maurizio Basaglia. All rights reserved.


CONTENTS

1. INTRODUCTION
2. CHANGE LOG
3. ACKNOWLEDGEMENTS
4. CONTACT INFO


1. INTRODUCTION

Welcome to O2, a new way to store and organize your personal data.

O2 is open source software, released under the Mozilla Public License
Version 2.0 (see License.txt or http://mozilla.org/MPL/2.0/ for full
details).

To work properly, O2 needs Microsoft Edge WebView2 Runtime. If it is not
already installed, download it from the official webpage (the evergreen
version is recommended):
https://developer.microsoft.com/en-us/microsoft-edge/webview2/

In order to start to use O2, see the examples in the "Examples"
directory (the password to open PasswordWallet.o2 is "password").

For further informations see: https://github.com/maurizuki/O2


2. CHANGE LOG

Version 3.3.1
- Fix missing leading white-spaces in object notes view.

Version 3.3
- Export to HTML: the style selection is now stored in the settings and
  restored every time the preview is opened.
- Rich Text Format files (License.rtf, ReadMe.rtf) replaced with plain
  text versions.

Version 3.2
- Multi-language support (a.k.a. Italian translation) removed. The
  feature is deprecated and no longer supported in Delphi 12.
- Import improvement: title, description and author are now imported.
- Import from XML improvement: the file is no longer completely
  overwritten, as it is for the other imports.
- Export to XML improvement: the export can now be limited to the
  selection, as it is for the other exports.
- Export to HTML: new style sheet "Sticky Notes".
- Export to HTML: small improvements to the HTML code and CSS classes.

Version 3.1
- Cryptography enhancement: the cipher algorithm initialization vector
  is now randomly generated.
- New feature: custom style sheets for Export to HTML. Each .css file
  found in the Styles folder can be selected in the Style menu of the
  preview window.
- Export to HTML: new dark theme added.

Version 3.0
- Extensive source code rewrite in order to upgrade it to much modern
  coding techniques. This may or may not cause some bugs 🙂
- Object notes view: note text is now displayed using an Edge-based
  control instead of an Internet Explorer-based one.
- Export to HTML: export preview is now displayed using an Edge-based
  control instead of an Internet Explorer-based one.
- Object properties dialog: simplified tags management.
- Object properties dialog: the password strength indicator is now
  automatically displayed for fields that matches rules of type Password.


3. ACKNOWLEDGEMENTS

The development and deployment of O2 were made possible thanks to the
following tools:

DCPCrypt Cryptographic Component Library
Copyright (C) 1999-2009 David Barton.

Inno Setup
Copyright (C) 1997-2025 Jordan Russell.
Portions Copyright (C) 2000-2025 Martijn Laan.

JEDI Visual Component Library
Copyright (C) 1999-2025 the Project JEDI community.

Spring4D
Copyright (C) 2009-2025 Spring4D Team.

SZCRC32 unit
Copyright (C) 2004 Sasa Zeman.

UPX - The Ultimate Packer for eXecutables
Copyright (C) 1996-2025 Markus Oberhumer, Laszlo Molnar, John Reiser.

zxcvbn
Copyright (C) 2012-2016 Dan Wheeler and Dropbox, Inc.
Delphi porting by TCardinal.


4. CONTACT INFO

For bug reports and suggestions or if you want to contribute:

https://github.com/maurizuki/O2
