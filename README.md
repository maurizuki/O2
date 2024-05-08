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

### Version 2.4
- New feature: support for [Markdown syntax](https://commonmark.org/) in notes text. Text and paragraph styles are applied to objects notes in the notes view and in the HTML export.
- *About* box: the release notes are now displayed through an internal viewer.

### Version 2.3
- *Encryption* dialog: added a color-based indication of the strength of the password typed, with some useful suggestions on how to strengthen it.
- *Object properties* dialog: added an option to enable a color based indication of the strength of the password typed in the field value, with some useful suggestions on how to strengthen it displayed hovering the indicator.
- Main window, objects view and fields view: objects and fields highlight indicates the strength of the passwords if the fields match rules of type *Password* with the new option *Display password strength*.
- Developed with Embarcadero® Delphi 11.

### Version 2.2.7
- New feature: export events (fields that match rules of type *Expiration date* and *Recurrence*) in *iCalendar* format, compatible with Microsoft Outlook, Google Calendar, Apple Calendar, etc.
- In the *Rule properties* dialog, *Date format* tab, the text field *Date format* has been replaced with a more friendly drop-down list.
- *Help* menu rationalization.

### Version 2.2.6
- Fixed: multiple consecutive white spaces in notes are not preserved during export to HTML.
- Minor improvements.

### Version 2.2.5
- Export to HTML: new page style in three flavors.
- Check for updates: integration with the GitHub REST API.
- Minor improvements.
- Developed with Embarcadero® Delphi 10.4.

### Version 2.2.4
- Fixed: the selections of the search options *Find by tag* and *Find by rule* aren't initialized opening another file.

### Version 2.2.3
- Deprecated cipher and hash algorithms: the cipher algorithms Blowfish, DES, Ice, Thin Ice, Misty1, RC2, RC4, TEA and the hash algorithms MD5 and SHA-1 have been deprecated due to evidences of their lack of security. It's still possible to open files encrypted using the deprecated algorithms, but it's no longer possible to save files encrypted with them.

### Version 2.2.2
- Main window, fields view: added the menu item *Show passwords* to the context menu.
- Developed with Embarcadero® Delphi 10.3.
- Compiled with Jedi VCL 3.50.

### Version 2.2.1
- Some aesthetic retouches to the main window, print preview window and HTML export preview window.
- New style sheet for the HTML export.
- Added the menu item *Documentation* to the *Help* menu: links to the wiki page of the project.

### Version 2.2
- New portable release that installs directly on removable media.
- Added the new feature *Replace role* to the objects menu: replaces the role of the selected objects in their relations.
- Minor bug fixes and improvements.

## Acknowledgements

The development and deployment of O2 were made possible thanks to the following tools:  

__DCPCrypt Cryptographic Component Library__  
Copyright (C) 1999-2009 David Barton.  

__delphi-markdown__  
Copyright (C) 2011+ Health Intersections Pty Ltd.  

__Inno Setup__  
Copyright (C) 1997-2023 Jordan Russell.  
Portions Copyright (C) 2000-2023 Martijn Laan.  

__JEDI Visual Component Library__  
Copyright (C) 1999-2023 the Project JEDI community.  

__Spring4D__  
Copyright (C) 2009-2023 Spring4D Team.  

__SZCRC32 unit__  
Copyright (C) 2004 Sasa Zeman.  

__UPX - The Ultimate Packer for eXecutables__  
Copyright (C) 1996-2023 Markus Oberhumer, Laszlo Molnar, John Reiser.  

__zxcvbn__  
Copyright (C) 2012-2016 Dan Wheeler and Dropbox, Inc.  
Delphi porting by TCardinal.
