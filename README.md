# O2

Welcome to O2, a new way to store and organize your personal data.  
O2 is open source software, released under the [Mozilla Public License Version 2.0](http://mozilla.org/MPL/2.0/).

## Change log

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

### Version 2.0.7
- Fixed a bug that prevented to save the application settings.
- Main window default size increased to 800x600 pixels.

### Version 2.0.6
- Object properties dialog: new tag editor with shortcut menu item "Edit tags" in the object menu.
- Object menu: new sub-menus "Add tag" (adds tag to the selected objects) and "Delete tag" (deletes tag from the selected objects).
- Replace tag dialog, replace field name dialog and replace field value dialog: added a drop-down list to the replace-with-value field.
- Added a prompt to delete the selected item from the recent file list if the file is not found.
- Minor user interface improvements.
- Updated example files.

### Version 2.0.5
- The sorting of the recent file list is now calculated by the usage count of the files.
- Added "Clear recent file list" to hide all the recent file list items.
- Compliance to the PortableApps.com Format 3.0 (1) specifications.

### Version 2.0.4
- The fields editor (object properties) now has a drop-down list of field values already used in other objects for the same field name.
- Compiled with Jedi VCL version 3.45.

### Version 2.0.3
- Added the option "Only if deactivated" in the "Transparency" sub-menu to apply the transparency percentage only if the application is not active.
- Minor bug fixes and code improvements.

### Version 2.0.2
- New HTML export preview (similar to the print preview) with new export options: include tags, include relations, include notes, include passwords.
- The print preview fits automatically the desktop area.
- Compiled with Jedi VCL version 3.40.

### Version 2.0.1
- Fixed bug: leading zeroes of numeric field values were deleted even if they were significant.
- Fixed bug: "handle not valid" error message on print preview page scroll.
- The name of the object selected in the object view is now displayed in the statusbar with the object tags.
- The font used to display the object notes on screen and to print them is now the same and it is monospaced to prevent differences of alignment for text tabulations.
- Minor changes to get maximum compliance to the PortableApps.com Format 2.0 (1) specifications.

### Version 2.0
- Massive code rewriting to get maximum compliance to the Unicode Standard specifications (http://unicode.org).

(1) Copyright (c) John T. Haller.

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
