;;; ox-opml.el --- Export Org files to OPML -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2025 Eric Davis, Guy Freeman

;; Author: Eric Davis <eric@davising.com>
;; Maintainer: Eric Davis <eric@davising.com>
;; Version: 2.0.0
;; Package-Requires: ((emacs "24.4") (org "8.0"))
;; Keywords: opml, xml, org, outlines, export
;; URL: https://github.com/org-opml/org-opml
;; Homepage: https://github.com/org-opml/org-opml

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements an OPML export backend for Org mode.
;;
;; OPML (Outline Processor Markup Language) is a standardized XML file
;; format for storing outlines.  This backend allows you to export Org
;; mode files to OPML format.
;;
;; The backend supports:
;; - Headlines with properties (converted to OPML attributes)
;; - Plain lists (converted to OPML outline elements)
;; - Paragraphs (converted to OPML outline elements)
;; - Basic text formatting (bold, italic, underline)
;; - Links
;; - Export settings in the OPML head element
;;
;; Usage:
;; From an Org mode buffer, use C-c C-e m to export to OPML.
;;
;; This backend is automatically loaded when you use org-opml.el.

;;; Code:

(require 'ox)

;;; Backend Definition

(org-export-define-backend 'opml
  '((bold . org-opml-bold)
    (entity . org-opml-entity)
    (headline . org-opml-headline)
    (italic . org-opml-italic)
    (item . org-opml-item)
    (link . org-opml-link)
    (paragraph . org-opml-paragraph)
    (plain-list . (lambda (plain-list contents info) contents))
    (section . (lambda (section contents info) contents))
    (subscript . org-ascii-subscript)
    (superscript . org-ascii-superscript)
    (template . org-opml-template)
    (underline . org-opml-underline))
  :options-alist '((:opml-link "OPML_LINK" nil nil t)
                   (:opml-owner-id "OPML_OWNER_ID" nil
                    (if (boundp 'opml-owner-id) opml-owner-id nil) t))
  :menu-entry '(?m "Export to OPML"
                (lambda (a s v b) (org-opml-export-to-opml a s v b)))
  :filters-alist '((:filter-final-output . org-opml-final-function)))

;;; Export Functions

;;;###autoload
(defun org-opml-export-to-opml (&optional async subtreep visible-only body-only)
  "Export current buffer to an OPML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

Return output file's name."
  (interactive)
  (let ((file (org-export-output-file-name ".opml" subtreep)))
    (org-export-to-file 'opml file async subtreep visible-only body-only)))

;;; Transcode Functions

(defun org-opml-entity (entity _contents _info)
  "Transcode an ENTITY object from Org to OPML.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (org-element-property :latex entity))

(defun org-opml-clean-text (str)
  "Remove problematic elements from STR.

1) Escape HTML entities (&, <, >, etc.)
2) Translate newlines into spaces
3) Remove any double spaces
4) Remove any trailing whitespace"
  (when str
    (let* ((text (url-insert-entities-in-string str))
           (text (replace-regexp-in-string "\n" " " text))
           (text (replace-regexp-in-string "[']" "&apos;" text))
           (text (replace-regexp-in-string "[ ][ ]+" " " text))
           (text (replace-regexp-in-string " $" "" text)))
      text)))

(defun org-opml-build-attributes (headline)
  "Build a key=value string from all property nodes for a given HEADLINE."
  (let* ((pom (org-element-property :begin headline))
         (properties (org-entry-properties pom))
         (attributes (mapconcat
                      (lambda (element)
                        (let ((key (car element))
                              (value (cdr element))
                              (case-fold-search nil))
                          (unless (string-match-p "\\`[A-Z]+\\'" key) ; skip all upcase keys
                            (format "%s=\"%s\"" key (org-opml-clean-text value)))))
                      properties " ")))
    attributes))

(defun org-opml-headline (headline contents _info)
  "Transcode a HEADLINE element from Org to OPML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let ((text (org-opml-clean-text (org-element-property :raw-value headline)))
        (attributes (org-opml-build-attributes headline))
        (contents (if (string= contents "\n") "" (or contents ""))))
    (format "<outline text='%s' structure=\"headline\" %s>%s</outline>"
            text attributes contents)))

(defun org-opml-paragraph (paragraph contents _info)
  "Transcode a PARAGRAPH element from Org to OPML.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (let* ((parent (org-element-type (org-export-get-parent paragraph)))
         (text (org-opml-clean-text contents)))
    ;; Only display paragraphs when not in a list item
    (unless (eq parent 'item)
      (format "<outline text='%s' structure=\"paragraph\"/>" text))))

(defun org-opml-item (item contents _info)
  "Transcode an ITEM element from Org to OPML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((p (car (org-element-contents item)))
         (elements (org-element-contents p))
         (text (mapconcat
                ;; TODO: This part of code needs rework,
                ;; formatting should be handled by the appropriate
                ;; formatting functions alone and not separately
                ;; here. The whole process of how to reformat the items
                ;; needs rethink. Currently we go over one word a time,
                ;; and use cond to format them accordingly, this
                ;; requires working on all possible conditions of
                ;; different org element types, not ideal!
                (lambda (el)
                  (cond ((stringp el) (org-opml-clean-text el))
                        ((equal (car el) 'link)
                         (let ((url (org-element-property :raw-link el))
                               (text (org-element-contents el)))
                           (org-opml-clean-text (format "<a href=\"%s\">%s</a>" url (car text)))))
                        ((equal (car el) 'italic)
                         (format "/%s/" (car (org-element-contents el))))
                        ((equal (car el) 'bold)
                         (format "*%s*" (car (org-element-contents el))))
                        ((equal (car el) 'underline)
                         (format "_%s_" (car (org-element-contents el))))
                        ((equal (car el) 'verbatim)
                         (format "=%s=" (org-element-property :value el)))
                        ((equal (car el) 'code)
                         (format "~%s~" (org-element-property :value el)))))
                elements " ")))
    (format "<outline text='%s' structure='list'>%s</outline>" text contents)))

(defun org-opml-link (link _contents _info)
  "Transcode a LINK object from Org to OPML.
CONTENTS is the description of the link, as a string, or the
empty string.  INFO is a plist holding contextual information."
  (let ((url (org-element-property :raw-link link))
        (text (car (org-element-contents link))))
    (if text
        (format "[[%s][%s]]" url text)
      url)))

(defun org-opml-italic (_italic contents _info)
  "Transcode ITALIC from Org to OPML.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (format "/%s/" contents))

(defun org-opml-bold (_bold contents _info)
  "Transcode BOLD from Org to OPML.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format "*%s*" contents))

(defun org-opml-underline (_underline contents _info)
  "Transcode UNDERLINE from Org to OPML.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (format "_%s_" contents))

;;; Template and Header Functions

(defun org-opml-add-header (key info &optional tag)
  "Add header element for KEY from INFO, optionally using TAG name."
  (let ((tag (or tag (substring (symbol-name key) 1)))
        (value (plist-get info key)))
    (when value
      (format "<%s>%s</%s>" tag (if (listp value) (car value) value) tag))))

(defun org-opml-add-timestamp-headers ()
  "Add dateModified and dateCreated headers with current timestamps."
  (let* ((fmt "%a, %d %b %Y %H:%M:%S")
         (attr (if (buffer-file-name) (file-attributes (buffer-file-name)) nil))
         (modified (if attr (nth 5 attr) (current-time)))
         (creation (current-time)))
    (concat
     (format "<dateModified>%s GMT</dateModified>" (format-time-string fmt modified t))
     (format "<dateCreated>%s GMT</dateCreated>" (format-time-string fmt creation t)))))

(defun org-opml-template (contents info)
  "Return complete document string after OPML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   "<?xml version='1.0'?>"
   (format "<!-- OPML generated by %s on %s GMT -->"
           org-export-creator-string
           (format-time-string "%a, %d %b %Y %H:%M:%S" (current-time) t))
   "<opml version='2.0'>"
   "<head>"
   (if (equal (plist-get info :title) " *Format Temp 0*")
       "<title>Untitled</title>"
     (org-opml-add-header :title info))
   (org-opml-add-header :description info)
   (org-opml-add-header :author info "ownerName")
   (org-opml-add-header :email info "ownerEmail")
   (org-opml-add-header :opml-owner-id info "ownerId")
   (org-opml-add-timestamp-headers)
   (org-opml-add-header :opml-link info "link")
   "<generator>https://github.com/org-opml</generator>"
   "<docs>http://dev.opml.org/spec2.html</docs>"
   "</head>"
   "<body>"
   contents
   "</body>"
   "</opml>"))

(defun org-opml-final-function (contents _backend _info)
  "Apply final formatting to CONTENTS.
BACKEND and INFO are ignored.  This function formats the XML
using xmllint if available."
  (with-temp-buffer
    (insert contents)
    (when (executable-find "xmllint")
      (shell-command-on-region (point-min) (point-max) "xmllint --format -" nil t))
    (buffer-substring-no-properties (point-min) (point-max))))

(provide 'ox-opml)

;;; ox-opml.el ends here
