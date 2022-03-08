# O2

[![release](https://img.shields.io/github/v/release/maurizuki/O2)](https://github.com/maurizuki/O2/releases/latest)
[![release date](https://img.shields.io/github/release-date/maurizuki/O2)](https://github.com/maurizuki/O2/releases/latest)
[![issues](https://img.shields.io/github/issues/maurizuki/O2)](https://github.com/maurizuki/O2/issues)
[![sourceforge.net downloads](https://img.shields.io/sourceforge/dt/o2project?logo=sourceforge)](https://github.com/maurizuki/O2/releases/latest)
[![github downloads](https://img.shields.io/github/downloads/maurizuki/O2/total?logo=github)](https://github.com/maurizuki/O2/releases/latest)

Welcome to O2, a new way to store and organize your personal data.

O2 is open source software, released under the [Mozilla Public License Version 2.0](http://mozilla.org/MPL/2.0/).

## Getting started

In order to start to use O2, please read the [*Getting started* section of the wiki](https://github.com/maurizuki/O2/wiki#getting-started).

## Change log

### Version 2.2.5
-	Export to HTML: new page style in three flavors.
- Check for updates: integration with the GitHub REST API.
- Minor improvements.
- Developed with Embarcadero® Delphi 10.4.

### Version 2.2.4
- Fixed: the selections of the search options "Find by tag" and "Find by rule" aren't initialized opening another file.

### Version 2.2.3
- Deprecated cipher and hash algorithms: the cipher algorithms Blowfish, DES, Ice, Thin Ice, Misty1, RC2, RC4, TEA and the hash algorithms MD5 and SHA-1 have been deprecated due to evidences of their lack of security. It's still possible to open files encrypted using the deprecated algorithms, but it's no longer possible to save files encrypted with them.

### Version 2.2.2
- Main window, fields view: added the menu item "Show passwords" to the context menu.
- Developed with Embarcadero® Delphi 10.3.
- Compiled with Jedi VCL 3.50.

### Version 2.2.1
- Some aesthetic retouches to the main window, print preview window and HTML export preview window.
- New style sheet for the HTML export.
- Added the menu item "Documentation" to the "Help" menu: links to the wiki page of the project.

### Version 2.2
- New portable release that installs directly on removable media.
- Added the new feature "Replace role" to the objects menu: replaces the role of the selected objects in their relations.
- Minor bug fixes and improvements.

### Version 2.1.2
- Added accelerator characters to almost all the labels to navigate trough their associated input controls more quickly.
- Object properties dialog, fields editor: added the buttons "Move up" and "Move down" to replace the unhandy popup menu.
- Rule properties dialog: added a box with an explanation of valid field name mask and field value mask composition.
- Rule properties dialog: added the name of the rule to the window title to keep it always visible.
- Rule properties dialog (italian translation): translated the color names for the color boxes in the highlight tab.
- Minor visual improvements.

### Version 2.1.1
- Added a drop-down menu to the "Find" button to add the new feature "Clear search".
- Added the "Find by rule" search option that search for objects with at least one field matching one of the selected rules.
- The columns of all the views are now resized automatically to fit the whole available width.
- Main window, rules view: improved the behavior of the functions "Move up" and "Move down", now the item remains visible while it is moved.
- Object properties dialog, fields editor: improved the behavior of the functions "Move up" and "Move down", now the item remains visible while it is moved. Improved also the behavior of the function "Delete", now is automatically selected the next item after the deleted one to continue deleting in sequence.
- Minor bug fixes and code improvements.

### Version 2.1
- Main window redesigned, clearer and cleaner. Click "Find" (Ctrl+F) toolbar button to show or hide the search options box (formerly filters): find by name, event, tags.

## Acknowledgements

The development and deployment of O2 were made possible thanks to the following tools:  

__DCPCrypt Cryptographic Component Library__  
Copyright (C) 1999-2009 David Barton.  

__Inno Setup__  
Copyright (C) 1997-2020 Jordan Russell.  
Portions Copyright (C) 2000-2020 Martijn Laan.  

__Jedi VCL Project__  
Copyright (C) 1999-2020 The Jedi VCL Team.  

__SZCRC32 unit__  
Copyright (C) 2004 Sasa Zeman.  

__UPX - The Ultimate Packer for eXecutables__  
Copyright (C) 1996-2020 Markus Oberhumer, Laszlo Molnar, John Reiser.
