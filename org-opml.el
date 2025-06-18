;; Locate the OPML import backend
(defun find-opml2org ()
  "Find opml2org.py script in various locations."
  (or (executable-find "opml2org.py")
      (let ((script-path (expand-file-name "opml2org.py" default-directory)))
        (when (file-executable-p script-path) script-path))
      (let ((script-path (expand-file-name "opml2org.py" (file-name-directory load-file-name))))
        (when (file-executable-p script-path) script-path))))

;;;###autoload
(defun opml-decode (begin end)
  (let ((opml2org (find-opml2org)))
    (if (eq opml2org nil)
        (error "Could not locate opml2org.py. Make sure it's executable and in PATH or current directory.")
      (let ((status (call-process-region
                     (point-min) (point-max)
                     opml2org
                     ;; three 't's = redisplay current buffer with processed text
                     t t t)))
        (cond ((eq status 0)
               ;; on success, return end point
               (point-max))

              (t ;; otherwise, signal an error
               (error "Could not call opml2org.py. Exit status: %s" status)))))))

;; If it ends with .opml, use `opml-encode' when saving.
(defun set-buffer-file-format-to-opml ()
  "Set buffer-file-format to '(opml) when visiting an .opml file.

This is needed as otherwise newly created .opml files wouldn't
know to pass their contents through `opml-encode' because they
don't yet contain the `format-alist' regexp pattern."
  (when (string-match "\.opml$" (buffer-file-name))
    (setq buffer-file-format '(opml))))

;; Run the above function each time Emacs opens a file.
(add-hook 'find-file-hooks 'set-buffer-file-format-to-opml)

;; Activate org-mode when opening OPML files.
(add-to-list 'auto-mode-alist '("\\.opml\\'" . org-mode))

(load-library "ox-opml")

;;;###autoload
(defun opml-encode (begin end buffer)
  "Export Org mode buffer to OPML."
  (let ((org-export-show-temporary-export-buffer nil)
        (name "*OPML Export Buffer*"))
    (org-export-to-buffer 'opml name)
    (erase-buffer)
    (insert-buffer-substring (get-buffer name))
    (point-max)))

;; Define the format conversion going to and from Org mode/OPML.
(add-to-list 'format-alist
             '(opml
               "Outline Processor Markup Language"
               "<[?]xml version=\"1.0\"[^>]*[?]>[\n]?.*[\n]?.*[\n]?<opml version=\"[1|2].0\">"
               opml-decode
               opml-encode
               t))

(provide 'org-opml)
