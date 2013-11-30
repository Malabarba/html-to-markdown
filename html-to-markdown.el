;;; html-to-markdown.el --- HTML to Markdown converter written in Emacs-lisp.

;; Copyright (C) 2013 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/BruceConnor/html-to-markdown
;; Version: 1.0
;; Keywords: extensions
;; Prefix: htm
;; Separator: -

;;; Commentary:
;;
;; HTML to Markdown converter written in Emacs-lisp.
;; 
;; Instructions
;; ============
;; 
;; To use this package, simply install it from Melpa (M-x
;; `package-install') and the relevant functions will be autoloaded.
;; 
;; This package defines two functions: `html-to-markdown' and
;; `html-to-markdown-string'.
;; They are written entirely in Emacs-lisp (which means they'll work on
;; any platform with no external dependencies), and they convert HTML
;; source code into Markdown format. Of course, HTML has many more
;; features than Markdown, so any tags that can't be converted are left
;; as-is (or deleted, if the user so requests).
;; 
;; - `html-to-markdown'  
;;   Is meant for interactive use. It takes the current buffer (or
;;   region), converts to Markdown, and displays the result in a separate
;;   window.
;; 
;; - `html-to-markdown-string'  
;;   Is meant for lisp code. It takes a string argument, which is
;;   converted to Markdown, and the result is returned.
;; 
;; They both take an extra boolean argument `erase-unknown'. If it's
;; non-nil, tags which can't be converted will be erased.

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 

;;; Change Log:
;; 1.0  - 2013/11/30 - First Release.
;;; Code:
(require 'thingatpt)

(defconst html-to-markdown-version "1.0" "Version of the html-to-markdown.el package.")
;; Not really necessary, but useful if you like counting how many versions you've released so far. 
(defconst html-to-markdown-version-int 2 "Version of the html-to-markdown.el package, as an integer.")
(defun htm-bug-report ()
  "Opens github issues page in a web browser. Please send any bugs you find.
Please include your emacs and html-to-markdown versions."
  (interactive)
  (message "Your htm-version is: %s, and your emacs version is: %s.\nPlease include this in your report!"
           html-to-markdown-version emacs-version)
  (browse-url "https://github.com/BruceConnor/html-to-markdown/issues/new"))

(defun htm--find-close-while-parsing (tag)
  "Search forward for TAG, while parsing other tags found on the way."
  (setq tag (or tag ""))  
  (let ((is-searching t) is-close)
    (while (and is-searching
                (search-forward-regexp "<[/a-z]" nil t))
      (let ((delimiter (in-string-p))) ;thingatpt.el
        ;; If we're inside a string, than it's not tag, move on...
        (if delimiter
            (search-forward (char-to-string delimiter))
          ;; If it IS a tag, check if it opens or closes.
          (if (looking-back "/")
              (setq is-close t)
            (forward-char -1))
          ;; If we found what we were looking for, that's it.
          (if (and is-close (string= (thing-at-point 'word) tag))
              (setq is-searching nil)
            ;; If not, keep parsing.
            (if is-close
                (error "Found </%s>, while expected </%s>."
                       (thing-at-point 'word) tag)
              (htm--parse-tag (thing-at-point 'word)))))))))

(defun htm--parse-tag (&optional tag)
  "Parse TAG or tag under point."
  (let* ((tag (or tag (thing-at-point 'word)))
         (func (intern (concat "htm--parse-" tag))))
    (if (fboundp func)
        (funcall func)
      (when htm--erase-unknown-tags
        (htm--delete-tag-at-point)))))

(defvar htm--erase-unknown-tags nil "")

(defvar htm--list-depth 0
  "How many spaces should we currently indent list items?")

(defvar htm--ordered-list-counter nil
  "If in ordered-list, this is the current counter. o.w. this is nil.")

(defvar htm--ordered-list-step 0 "")

(defun htm--define-list-replacer (tag mds ordered)
  (let ((step (length mds)))
    (eval
     `(defun ,(intern (concat "htm--parse-" tag)) ()
        ,(format "Convert <li> inside a <%s> into %s." tag mds)
        (htm--delete-tag-at-point)
        (when (= htm--ordered-list-step 0)
          (htm--ensure-blank-line))
        (incf htm--list-depth htm--ordered-list-step)
        (let ((htm--ordered-list-counter ,ordered)
              (htm--ordered-list-step (+ ,step htm--ordered-list-step)))
          (htm--find-close-while-parsing ,tag))
        (decf htm--list-depth htm--ordered-list-step)
        (htm--delete-tag-at-point)))))
(htm--define-list-replacer "ul" "-" nil)
(htm--define-list-replacer "ol" "1." 0)

(defun htm--ensure-blank-line ()
  (if (not (looking-back "^ *"))
      (insert "\n\n")
    (unless (looking-back "\n *\n *")
      (insert "\n"))))

(defun htm--parse-li ()
  "Convert <li> into 1. or -."
  (htm--delete-tag-at-point)
  (let ((indent (make-string htm--list-depth 32)))
    (if (looking-back "^ +")
        (replace-match indent :fixedcase :literal)
      (unless (looking-back "^") (insert "\n"))
      (insert indent))
    (if (null htm--ordered-list-counter)
        (insert "- ")
      (incf htm--ordered-list-counter)
      (insert (format "%s. " htm--ordered-list-counter))))
  (htm--find-close-while-parsing "li")
  (htm--delete-tag-at-point))

(defvar htm--inside-code nil "Are we inside a code-block?")

(defun htm--parse-p ()
  "Convert <p> into blank lines."
  (htm--delete-tag-at-point)
  (htm--ensure-blank-line)
  (when (> htm--ordered-list-step 0)
    (insert (make-string (+ 1 htm--ordered-list-step htm--list-depth) ?\s)))
  (when htm--inside-code
    (insert "    "))
  (htm--find-close-while-parsing "p")
  (htm--delete-tag-at-point)
  (htm--ensure-blank-line))

(defun htm--parse-br ()
  "Convert <br> into \"  \\n\".

We need to keep the <br> that don't come directly after text,
otherwise markdown will just swallow the extra blank lines and
the formatting will be lost."
  (if (looking-back "</")
      (htm--delete-tag-at-point)
    (if (looking-back "^ *<")
        (progn (forward-char -1)
               (forward-sexp 1))
      (htm--delete-tag-at-point)
      (insert "  "))
    (insert "\n")
    (when (> htm--ordered-list-step 0)
      (insert (make-string (+ 1 htm--ordered-list-step htm--list-depth) ?\s)))
    (when htm--inside-code
      (insert "    "))))

(defun htm--define-simple-replacer (cons)
  "Define a function which replaces (car CONS) with (cdr CONS)."
  (let ((tag (car cons))
        (mds (cdr cons))) ;; mds = markdown-syntax
    (eval
     `(defun ,(intern (concat "htm--parse-" tag)) ()
        ,(format "Convert <%s> and </%s> tags into %s." tag tag mds)
        (htm--delete-tag-at-point)
        ,(if (and (symbolp mds) (fboundp mds))
             `(funcall ',mds)
           `(insert ,mds))
        (htm--find-close-while-parsing ,tag)
        (htm--delete-tag-at-point)
        (save-excursion
          (skip-chars-backward "\n ")
          ,(if (and (symbolp mds) (fboundp mds))
               `(funcall ',mds)
             `(insert ,mds)))))))

(defvar htm--simple-replacers-alist
  '(("b" . "**")
    ("i" . "_")
    ("strong" . "**")
    ("em" . "_")
    ("strike" . "~~")
    ("p" . htm--ensure-blank-line))
  "List of (TAG . MARKDOWN-SYNTAX) used to define htm--parse- functions.")

(mapc 'htm--define-simple-replacer htm--simple-replacers-alist)

(defun htm--delete-tag-at-point ()
  (save-match-data
    (search-backward "<")
    (let ((opoint (point)))
      (forward-sexp 1)
      (delete-region opoint (point)))))

(defcustom htm-output-buffer-name "*html-to-markdown output*"
  "Name used for the buffer which holds conversion output."
  :type 'string
  :group 'html-to-markdown
  :package-version '(html-to-markdown . "0.1a"))

(defun htm--convert (erase-unknown)
  "Perform the actual conversion.

This sort-of expects a temp buffer, because major-mode will be changed."
  (html-mode)
  (goto-char (point-min))
  (let ((htm--erase-unknown-tags erase-unknown))
    (htm--find-close-while-parsing nil)))

;;;###autoload
(defun html-to-markdown ( &optional erase-unknown)
  "Convert contents of current buffer from html to markdown.

This is meant for interactive use. For lisp code, use:
    (html-to-markdown-string (buffer-string))

If the prefix argument ERASE-UNKNOWN is non-nil, tags which can't
be converted to markdown will be erased (default is to keep them
as-is).

Understands the following html tags: 
p, ol, ul, li, h[1-9], hr, b, it."
  (interactive "P")
  (let* ((l (if (region-active-p) (region-beginning) (point-min)))
         (r (if (region-active-p) (region-end) (point-max)))
         (source (buffer-substring l r)))
    (with-output-to-temp-buffer htm-output-buffer-name
      (set-buffer htm-output-buffer-name)
      (insert source)
      (htm--convert erase-unknown)
      (markdown-mode))))

;;;###autoload
(defun html-to-markdown-string (source &optional erase-unknown)
  "Convert contents of string SOURCE from html to markdown.

Returns a string with the result.

If ERASE-UNKNOWN is non-nil, tags which can't be converted to
markdown will be erased (default is to keep them as-is).

Understands the following html tags: 
p, ol, ul, li, h[1-9], hr, b, it."
  (interactive "MHTML Source: \nP")
  (let ((res
         (with-temp-buffer
           (insert source)
           (htm--convert erase-unknown)
           (buffer-string))))
    (when (called-interactively-p 'any)
      (with-output-to-temp-buffer htm-output-buffer-name
        (set-buffer htm-output-buffer-name)
        (insert res)
        (markdown-mode))
      (kill-new res)
      (message "Resulting Markdown pushed to kill-ring."))))

(provide 'html-to-markdown)
;;; html-to-markdown.el ends here.
