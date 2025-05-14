# O2

[![release](https://img.shields.io/github/v/release/maurizuki/O2)](https://github.com/maurizuki/O2/releases/latest)
[![release date](https://img.shields.io/github/release-date/maurizuki/O2)](https://github.com/maurizuki/O2/releases/latest)
[![issues](https://img.shields.io/github/issues/maurizuki/O2)](https://github.com/maurizuki/O2/issues)
[![sourceforge.net downloads](https://img.shields.io/sourceforge/dt/o2project?logo=sourceforge)](https://github.com/maurizuki/O2/releases/latest)
[![github downloads](https://img.shields.io/github/downloads/maurizuki/O2/total?logo=github)](https://github.com/maurizuki/O2/releases/latest)

Welcome to O2, a new way to store and organize your personal data.

O2 is open source software, released under the [Mozilla Public License Version 2.0](http://mozilla.org/MPL/2.0/).

## Getting started

In order to start to use O2 - after installing it on your computer - see the examples in the *Examples* directory:

- __AddressBook.o2__ shows how to use the *rules* of type *E-mail address*, *Highlight*, *Internet link*, *Recurrence* to organize contact informations and recurrences of your relatives, friends, co-workers. The example also shows how to use the *relations* to connect husbands with wives, brothers with sisters, companies with employees.
- __PasswordWallet.o2__ shows how to use the *rule* of type *Password* to create a safe place to store the credentials of your accounts. The password to open the file is *password*.

### System requirements

To work properly, O2 needs Microsoft Edge WebView2 Runtime. If it is not already installed, download it from the official [webpage](https://developer.microsoft.com/en-us/microsoft-edge/webview2/) (the evergreen version is recommended).

### Install with Windows Package Manager

To install O2 with the Windows Package Manager - a.k.a. *winget* - use the following command:
```
winget install --id=maurizuki.O2 -e
```

## Change log

### Version 3.1
-	Cryptography enhancement: the cipher algorithm initialization vector is now randomly generated.
-	New feature: custom style sheets for *Export to HTML*. Each *.css* file found in the *Styles* folder can be selected in the *Style* menu of the preview window.
-	*Export to HTML*: new dark theme added.

### Version 3.0
- Extensive source code rewrite in order to upgrade it to much modern coding techniques. This may or may not cause some bugs 🙂
- *Object notes* view: note text is now displayed using an Edge-based control instead of an Internet Explorer-based one.
- *Export to HTML*: export preview is now displayed using an Edge-based control instead of an Internet Explorer-based one.
- *Object properties* dialog: simplified tags management.
- *Object properties* dialog: the password strength indicator is now automatically displayed for fields that matches rules of type *Password*.

### Version 2.4
- New feature: support for [Markdown syntax](https://commonmark.org/) in notes text. Text and paragraph styles are applied to objects notes in the notes view and in the HTML export.
- *About* box: the release notes are now displayed through an internal viewer.

### Version 2.3
- *Encryption* dialog: added a color-based indication of the strength of the password typed, with some useful suggestions on how to strengthen it.
- *Object properties* dialog: added an option to enable a color based indication of the strength of the password typed in the field value, with some useful suggestions on how to strengthen it displayed hovering the indicator.
- Main window, objects view and fields view: objects and fields highlight indicates the strength of the passwords if the fields match rules of type *Password* with the new option *Display password strength*.
- Developed with Embarcadero® Delphi 11.

## Acknowledgements

The development and deployment of O2 were made possible thanks to the following tools:  

__DCPCrypt Cryptographic Component Library__  
Copyright (C) 1999-2009 David Barton.  

__delphi-markdown__  
Copyright (C) 2011+ Health Intersections Pty Ltd.  

__Inno Setup__  
Copyright (C) 1997-2025 Jordan Russell.  
Portions Copyright (C) 2000-2025 Martijn Laan.  

__JEDI Visual Component Library__  
Copyright (C) 1999-2025 the Project JEDI community.  

__Spring4D__  
Copyright (C) 2009-2025 Spring4D Team.  

__SZCRC32 unit__  
Copyright (C) 2004 Sasa Zeman.  

__UPX - The Ultimate Packer for eXecutables__  
Copyright (C) 1996-2025 Markus Oberhumer, Laszlo Molnar, John Reiser.  

__zxcvbn__  
Copyright (C) 2012-2016 Dan Wheeler and Dropbox, Inc.  
Delphi porting by TCardinal.
