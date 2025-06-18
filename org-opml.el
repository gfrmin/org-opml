;;; org-opml.el --- Edit OPML files using Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2025 Eric Davis, Guy Freeman

;; Author: Eric Davis <eric@davising.com>
;; Maintainer: Eric Davis <eric@davising.com>
;; Version: 2.0.0
;; Package-Requires: ((emacs "24.4") (org "8.0"))
;; Keywords: opml, xml, org, outlines
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

;; org-opml lets you edit OPML files using Org mode in Emacs.
;;
;; OPML (Outline Processor Markup Language) is a standardized XML file
;; format for storing outlines.  This package allows you to open OPML
;; files in Emacs and edit them using Org mode's powerful outlining
;; interface.
;;
;; When you open an OPML file, it is automatically converted to Org mode
;; format for editing.  When you save the file, it is converted back to
;; OPML format.
;;
;; Features:
;; - Handles headlines, plain list items, and paragraphs
;; - HTML entities (<, >, &, etc.) are safely escaped
;; - Can set <outline> attributes via property drawers
;; - Uses standard export settings in the <head> element
;; - Follows the OPML 2.0 specification
;; - Pure Elisp implementation with no external dependencies
;;
;; Usage:
;; Simply open an .opml file in Emacs.  It will automatically be
;; displayed in Org mode format.  Edit as normal, then save to
;; convert back to OPML.
;;
;; You can also export any Org mode file to OPML using the export
;; dispatcher: C-c C-e m

;;; Code:

(require 'xml)
(require 'org)
(require 'ox)

;;; OPML to Org conversion

(defvar org-opml--conversion-data nil
  "Internal data structure for tracking OPML conversion state.")

(defun org-opml--make-conversion-data ()
  "Create a new conversion data structure."
  (list :has-headline-attributes nil
        :headline-depth 1
        :list-depth 0))

(defun org-opml--get-conversion-data (key)
  "Get value for KEY from conversion data."
  (plist-get org-opml--conversion-data key))

(defun org-opml--set-conversion-data (key value)
  "Set VALUE for KEY in conversion data."
  (setq org-opml--conversion-data
        (plist-put org-opml--conversion-data key value)))

(defun org-opml--increment-conversion-data (key &optional delta)
  "Increment value for KEY in conversion data by DELTA (default 1)."
  (let ((current (org-opml--get-conversion-data key))
        (delta (or delta 1)))
    (org-opml--set-conversion-data key (+ current delta))))

(defun org-opml--xml-get-attribute (node attr)
  "Get attribute ATTR from XML NODE."
  (cdr (assq attr (xml-node-attributes node))))

(defun org-opml--xml-get-children (node)
  "Get child elements from XML NODE."
  (xml-get-children node 'outline))

(defun org-opml--xml-node-name (node)
  "Get the name of XML NODE."
  (xml-node-name node))

(defun org-opml--clean-text (text)
  "Clean TEXT by unescaping HTML entities and normalizing whitespace."
  (when text
    (let ((cleaned text))
      ;; Unescape HTML entities
      (setq cleaned (replace-regexp-in-string "&lt;" "<" cleaned))
      (setq cleaned (replace-regexp-in-string "&gt;" ">" cleaned))
      (setq cleaned (replace-regexp-in-string "&amp;" "&" cleaned))
      (setq cleaned (replace-regexp-in-string "&quot;" "\"" cleaned))
      (setq cleaned (replace-regexp-in-string "&apos;" "'" cleaned))
      ;; Normalize whitespace
      (setq cleaned (replace-regexp-in-string "[ \t\n\r]+" " " cleaned))
      (setq cleaned (replace-regexp-in-string "^ +" "" cleaned))
      (setq cleaned (replace-regexp-in-string " +$" "" cleaned))
      cleaned)))

(defun org-opml--wrap-text (text &optional width)
  "Wrap TEXT to specified WIDTH (default 68 for 2-space indentation)."
  (let ((width (or width 68)))
    (if (<= (length text) width)
        text
      (with-temp-buffer
        (insert text)
        (fill-region (point-min) (point-max))
        (buffer-string)))))

(defun org-opml--determine-structure (outline)
  "Determine the structure type for OUTLINE element."
  (let* ((attrib (xml-node-attributes outline))
         (structure-attr (cdr (assq 'structure attrib)))
         (children (org-opml--xml-get-children outline))
         (has-custom-attrs (cl-some (lambda (attr)
                                      (not (memq (car attr) '(text structure))))
                                    attrib)))
    (cond
     ;; Explicit structure attribute
     (structure-attr (intern structure-attr))
     ;; Has custom attributes - force headline
     (has-custom-attrs
      (org-opml--set-conversion-data :has-headline-attributes t)
      'headline)
     ;; Has children and we're in simple OPML context
     ((and children
           (not (org-opml--get-conversion-data :has-headline-attributes))
           (= (org-opml--get-conversion-data :list-depth) 0))
      'headline)
     ;; Has children but not in simple context
     (children 'list)
     ;; Leaf node in simple OPML context with depth > 1
     ((and (not (org-opml--get-conversion-data :has-headline-attributes))
           (> (org-opml--get-conversion-data :headline-depth) 1))
      'list)
     ;; Default to paragraph
     (t 'paragraph))))

(defun org-opml--process-outline (outline)
  "Process a single OUTLINE element and return list of org lines."
  (let* ((text-attr (org-opml--xml-get-attribute outline 'text))
         (text (org-opml--clean-text text-attr))
         (structure (org-opml--determine-structure outline))
         (children (org-opml--xml-get-children outline))
         (attrib (xml-node-attributes outline))
         (custom-attrs (cl-remove-if (lambda (attr)
                                       (memq (car attr) '(text structure)))
                                     attrib))
         (lines '()))
    
    (cond
     ((eq structure 'headline)
      (let ((depth (org-opml--get-conversion-data :headline-depth)))
        (setq lines (append lines (list (format "%s %s" (make-string depth ?*) text))))
        
        ;; Add properties if present
        (when custom-attrs
          (setq lines (append lines '("  :PROPERTIES:")))
          (dolist (attr custom-attrs)
            (setq lines (append lines (list (format "  :%s: %s" (car attr) (cdr attr))))))
          (setq lines (append lines '("  :END:"))))
        
        ;; Process children
        (when children
          (when custom-attrs
            (setq lines (append lines '("")))) ; blank line after properties
          (org-opml--increment-conversion-data :headline-depth)
          (dolist (child children)
            (setq lines (append lines (org-opml--process-outline child))))
          (org-opml--increment-conversion-data :headline-depth -1)
          ;; Add blank line after headline section if at depth 1 or 2
          (when (<= (org-opml--get-conversion-data :headline-depth) 2)
            (setq lines (append lines '("")))))))
     
     ((eq structure 'list)
      (let ((indent (make-string (org-opml--get-conversion-data :list-depth) ?\s)))
        (setq lines (append lines (list (format "%s- %s" indent text))))
        
        ;; Process children (sub-lists)
        (when children
          (org-opml--increment-conversion-data :list-depth 2)
          (dolist (child children)
            (setq lines (append lines (org-opml--process-outline child))))
          (org-opml--increment-conversion-data :list-depth -2))))
     
     ((eq structure 'paragraph)
      ;; Handle multi-line text
      (if (string-match-p "\n" text)
          (dolist (line (split-string text "\n"))
            (when (not (string-empty-p (string-trim line)))
              (setq lines (append lines (list (format "  %s" (string-trim line)))))))
        ;; Single line text - wrap if needed
        (let ((wrapped (org-opml--wrap-text text)))
          (dolist (line (split-string wrapped "\n"))
            (when (not (string-empty-p (string-trim line)))
              (setq lines (append lines (list (format "  %s" line))))))))
      (setq lines (append lines '(""))))) ; blank line after paragraph
    
    lines))

(defun org-opml--process-body (body-element)
  "Process BODY-ELEMENT and return list of org mode lines."
  (let ((org-opml--conversion-data (org-opml--make-conversion-data))
        (lines '()))
    (dolist (outline (org-opml--xml-get-children body-element))
      (setq lines (nconc lines (org-opml--process-outline outline))))
    lines))

(defun org-opml--extract-header (head-element tag &optional export-tag)
  "Extract header from HEAD-ELEMENT with TAG, optionally using EXPORT-TAG."
  (let* ((element (car (xml-get-children head-element tag)))
         (text (when element (car (xml-node-children element))))
         (key (or export-tag (upcase (symbol-name tag)))))
    (when (and text (not (string-empty-p (string-trim text))))
      (format "#+%s: %s" key (string-trim text)))))

(defun org-opml--parse-opml (xml-content)
  "Parse XML-CONTENT as OPML and return org mode content."
  (let* ((xml-tree (with-temp-buffer
                     (insert xml-content)
                     (xml-parse-region (point-min) (point-max))))
         (opml-root (car xml-tree))
         (head (car (xml-get-children opml-root 'head)))
         (body (car (xml-get-children opml-root 'body)))
         (headers '())
         (org-lines '()))
    
    (unless body
      (error "OPML file must contain a <body> element"))
    
    ;; Extract headers
    (when head
      (let ((description-header (org-opml--extract-header head 'description)))
        (when description-header
          (push "#+STARTUP: indent" headers)))
      
      (dolist (header-info '((title) (description)))
        (let ((header (apply #'org-opml--extract-header head header-info)))
          (when header
            (push header headers)))))
    
    ;; Process body
    (setq org-lines (org-opml--process-body body))
    
    ;; Combine headers and body
    (let ((header-text (when headers
                         (mapconcat #'identity (nreverse headers) "\n")))
          (body-text (when org-lines
                       (mapconcat #'identity org-lines "\n"))))
      (cond
       ((and header-text body-text)
        (concat header-text "\n\n" body-text))
       (header-text header-text)
       (body-text body-text)
       (t "")))))

;;; Format conversion functions

;;;###autoload
(defun org-opml-decode (begin end)
  "Decode OPML content between BEGIN and END, replacing with Org content."
  (let ((xml-content (buffer-substring begin end)))
    (condition-case err
        (progn
          ;; Only proceed if content looks like OPML
          (if (string-match-p "^\\s-*<\\?xml\\|^\\s-*<opml" xml-content)
              (let ((org-content (org-opml--parse-opml xml-content)))
                (delete-region begin end)
                (insert org-content)
                (point))
            ;; If it doesn't look like OPML, don't process it
            end))
      (error
       (message "Error parsing OPML: %s" (error-message-string err))
       ;; Return original end point on error to avoid corruption
       end))))

;;;###autoload
(defun org-opml-encode (begin end buffer)
  "Encode Org content between BEGIN and END as OPML."
  (let ((org-export-show-temporary-export-buffer nil)
        (temp-buffer-name "*OPML Export Buffer*"))
    (org-export-to-buffer 'opml temp-buffer-name)
    (with-current-buffer buffer
      (delete-region begin end)
      (insert-buffer-substring (get-buffer temp-buffer-name))
      (kill-buffer temp-buffer-name)
      (point-max))))

;;; File format registration

(defun org-opml--set-buffer-file-format ()
  "Set buffer-file-format to '(opml) when visiting an .opml file."
  (when (and buffer-file-name
             (string-match-p "\\.opml\\'" buffer-file-name))
    (setq buffer-file-format '(opml))))

;;;###autoload
(add-hook 'find-file-hook #'org-opml--set-buffer-file-format)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.opml\\'" . org-mode))

;; Load the export backend
(require 'ox-opml)

;; Register the format conversion
;;;###autoload
(add-to-list 'format-alist
             '(opml
               "Outline Processor Markup Language"
               "<[?]xml version=\"1.0\"[^>]*[?]>[\n]?.*[\n]?.*[\n]?<opml version=\"[1|2].0\">"
               org-opml-decode
               org-opml-encode
               t))

(provide 'org-opml)

;;; org-opml.el ends here
