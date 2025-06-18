### Brief summary of what the package does

org-opml lets you edit OPML (Outline Processor Markup Language) files using Org mode in Emacs. When you open an OPML file, it's automatically converted to Org mode format for editing with stars for headlines, dashes for list items, etc. When you save, it converts back to OPML format. This provides the best of both worlds: the power of Emacs/Org mode for editing outlines while maintaining standard XML/OPML compatibility on disk.

Key features:
- Handles headlines, plain list items, and paragraphs
- HTML entities are safely escaped
- Can set OPML attributes via property drawers
- Uses standard export settings in the <head> element
- Follows the OPML 2.0 specification
- Pure Elisp implementation with no external dependencies
- Supports both simple outlines and complex structured documents with attributes

### Direct link to the package repository

https://github.com/gfrmin/org-opml

### Your association with the package

I am the maintainer of this package. This is a major rewrite (version 2.0.0) that removes the previous Python dependency and implements everything in pure Elisp for better MELPA compliance and easier installation.

### Relevant communications with the upstream package maintainer

**None needed** - I am the package maintainer submitting this for MELPA inclusion.

### Checklist

<!-- Please confirm by replacing `[]` with `[x]`: -->

- [x] The package is released under a [GPL-Compatible Free Software License](https://www.gnu.org/licenses/license-list.en.html#GPLCompatibleLicenses)
- [x] I've read [CONTRIBUTING.org](https://github.com/melpa/melpa/blob/master/CONTRIBUTING.org)
- [x] I've used the latest version of [package-lint](https://github.com/purcell/package-lint) to check for packaging issues, and addressed its feedback
- [x] My elisp byte-compiles cleanly
- [x] I've used `M-x checkdoc` to check the package's documentation strings
- [x] I've built and installed the package using the instructions in [CONTRIBUTING.org](https://github.com/melpa/melpa/blob/master/CONTRIBUTING.org)

<!-- After submitting, please fix any problems the CI reports. -->
