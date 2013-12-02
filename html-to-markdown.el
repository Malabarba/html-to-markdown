;;; html-to-markdown.el --- HTML to Markdown converter written in Emacs-lisp.

;; Copyright (C) 2013 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/html-to-markdown
;; Version: 1.0
;; Keywords: tools wp languages
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
  (browse-url "https://github.com/Bruce-Connor/html-to-markdown/issues/new"))

(defun htm--find-close-while-parsing (tag)
  "Search forward for TAG, while parsing other tags found on the way."
  (let ((tag-name (or tag ""))
        (is-searching t))
    (while (and is-searching
                (search-forward-regexp "<[/a-z]\\|\n" nil t))
      (let ((delimiter (save-match-data (in-string-p))) ;thingatpt.el
            tag-found is-close) 
        ;; If we're inside a string, don't mess with anything, move on...
        (if delimiter
            (search-forward (char-to-string delimiter))
          ;; Check if we matched a tag or a newline.
          (if (looking-back "\n")
              (unless htm--inside-pre
                (delete-char -1)
                (just-one-space 1))
            ;; If it IS a tag, check if it opens or closes.
            (if (looking-back "/")
                (setq is-close t)
              (forward-char -1))
            (setq tag-found (thing-at-point 'word))
            ;; If we found what we were looking for, that's it.
            (if (and is-close (string= tag-found tag-name))
                (setq is-searching nil)
              ;; If not, keep parsing.
              (if (and is-close (fboundp (intern (concat "htm--parse-" tag-found))))
                  (error "Found </%s>, while expected %s."
                         tag-found
                         (if tag (format "</%s>" tag-name) "an openning tag"))
                (htm--parse-any-tag tag-found)))))))
    (and is-searching tag (error "File ended before closing </%s>." tag-name))))

(defun htm--parse-any-tag (&optional tag)
  "Parse TAG or tag under point."
  (let* ((tag (or tag (thing-at-point 'word)))
         (func (intern (concat "htm--parse-" tag))))
    (if (fboundp func)
        (funcall func)
      (when htm--erase-unknown-tags
        (htm--delete-tag-at-point)))))

(defun htm--backtick-unless-inside-pre ()
  "Insert \"`\", unless `htm--inside-pre` is non-nil."
  (unless htm--inside-pre (insert "`")))

(defun htm--define-simple-replacer (cons)
  "Define a function which replaces (car CONS) with (cdr CONS)."
  (let* ((tag (car cons))
         (mds (cdr cons))
         (var-name (intern (concat "htm--inside-" tag)))
         (mdl (car-safe mds))
         (mdr (or (car-safe (cdr-safe mds)) mdl))) ;; mds = markdown-syntax
    (eval
     `(progn
        (defvar ,var-name nil
          ,(format "Variable let-bound to t while we're inside a %s tag." tag))
        (defun ,(intern (concat "htm--parse-" tag)) ()
          ,(format "Convert <%s> and </%s> tags into %s." tag tag mds)
          (htm--delete-tag-at-point)
          ,(if (and (symbolp mdl) (fboundp mdl))
               `(funcall ',mdl)
             `(insert ,mdl))
          (let ((,var-name t))
            (htm--find-close-while-parsing ,tag))
          (htm--delete-tag-at-point)
          (let (point)
            (save-excursion
              (skip-chars-backward "\n ")
              ,(if (and (symbolp mdr) (fboundp mdr))
                   `(funcall ',mdr)
                 `(insert ,mdr))
              (setq point (point)))
            (if (> point (point)) (goto-char point))))))))

(defvar htm--simple-replacers-alist
  '(("i"       "_")
    ("em"      "_")
    ("b"       "**")
    ("strong"  "**")
    ("strike"  "~~")
    ("pre"     "\n```\n")
    ("h1"      "\n" "\n---\n")
    ("h2"      "\n" "\n===\n")
    ("h3"      "\n### " " ###\n")
    ("h4"      "\n#### " " ####\n")
    ("h5"      "\n##### " " #####\n")
    ("h6"      "\n###### " " ######\n")
    ("h7"      "\n####### " " #######\n")
    ("h8"      "\n######## " " ########\n")
    ("h9"      "\n######### " " #########\n")
    ("code"   htm--backtick-unless-inside-pre))
  "List of (TAG . MARKDOWN-SYNTAX) used to define htm--parse- functions.

This defines a function htm--parse-TAG and a variable htm--inside-TAG.

MARKDOWN-SYNTAX is either,
    (SYNTAX)
 or 
    (LEFT-SYNTAX RIGHT-SYNTAX)
where the syntaxes can be strings or symbols. If they're symbols
they are called as functions.")

(mapc 'htm--define-simple-replacer htm--simple-replacers-alist)

(defvar htm--erase-unknown-tags nil "")

(defvar htm--list-depth 0
  "How many spaces should we currently indent list items?")

(defvar htm--ordered-list-counter nil
  "If in ordered-list, this is the current counter. o.w. this is nil.")

(defvar htm--list-step 0 "")

(defun htm--define-list-replacer (tag mds ordered)
  (let ((step (length mds)))
    (eval
     `(defun ,(intern (concat "htm--parse-" tag)) ()
        ,(format "Convert <li> inside a <%s> into %s." tag mds)
        (htm--delete-tag-at-point)
        (when (= htm--list-step 0)
          (htm--ensure-blank-line))
        (incf htm--list-depth htm--list-step)
        (let ((htm--ordered-list-counter ,ordered)
              (htm--list-step (+ ,step htm--list-step)))
          (htm--find-close-while-parsing ,tag))
        (decf htm--list-depth htm--list-step)
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
  (let ((indent (make-string htm--list-depth 32))
        (fill-prefix (concat (or fill-prefix "")
                             (make-string (+ 1 htm--list-depth htm--list-step) ?\s))))
    (if (looking-back "^ +")
        (replace-match indent :fixedcase :literal)
      (unless (looking-back "^") (insert "\n"))
      (insert indent))
    (if (null htm--ordered-list-counter)
        (insert "- ")
      (incf htm--ordered-list-counter)
      (insert (format "%s. " htm--ordered-list-counter)))
    (htm--find-close-while-parsing "li"))
  (htm--delete-tag-at-point))

(defun htm--parse-p ()
  "Convert <p> into blank lines.

Assumes that you won't have plain text and <p>'d text in the same
line. If you do, this will end up merging them together."
  (htm--delete-tag-at-point)
  (when (looking-back "^\\s-*")
    (unless (looking-back "\n\\s-*\n\\s-*")
      (insert "\n"))
    ;; (htm--ensure-blank-line)
    (while (looking-back " ") (delete-char -1))
    (htm--add-padding)
    ;; (when htm--inside-pre
    ;;   (insert "    "))
    )
  (htm--find-close-while-parsing "p")
  (htm--delete-tag-at-point)
  (save-excursion (skip-chars-backward "\n ") (fill-paragraph))
  (htm--ensure-blank-line))

(defun htm--parse-blockquote ()
  "Convert <blockquote> into \"> \"."
  (htm--delete-tag-at-point)
  (unless (looking-back "^[ >]*")
    (insert "\n")
    (htm--add-padding))
  (insert "> ")      
  (let ((fill-prefix (concat (or fill-prefix "") "> ")))
    (htm--find-close-while-parsing "blockquote")
    (htm--delete-tag-at-point)
    (save-excursion (skip-chars-backward "\n ") (fill-paragraph)))
  (insert "\n"))

(defun htm--parse-br ()
  "Convert <br> into \"  \\n\".

We need to keep the <br> that don't come directly after text,
otherwise markdown will just swallow the extra blank lines and
the formatting will be lost."
  (if (looking-back "</")
      ;; On a close tag, do nothing.
      (htm--delete-tag-at-point)
    (if (looking-back "^ *<")
        (progn (forward-char -1)
               (forward-sexp 1))
      (htm--delete-tag-at-point)
      (fill-paragraph)
      (insert "  "))
    (insert "\n\n")
    (while (looking-at "[ \n]")
      (delete-char 1))
    (htm--add-padding)))

(defun htm--add-padding ()
  ;; (when (> htm--list-step 0)
  ;;   (insert (make-string (+ 1 htm--list-step htm--list-depth) ?\s)))
  (when (stringp fill-prefix) (insert fill-prefix)))

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
  :package-version '(html-to-markdown . "1.0"))

(defcustom htm-do-fill-paragraph t
  "If non-nil, paragraphs will be filled during the conversion.

This leads to good results (it won't screw up your line breaks or
anything), but some markdown interpreters treat filled paragraphs
as if they had line breaks. So this may be useful for some
people."
  :type 'boolean
  :group 'html-to-markdown
  :package-version '(html-to-markdown . "1.0"))

(defun htm--convert (erase-unknown)
  "Perform the actual conversion.

This sort-of expects a temp buffer, because major-mode will be changed."
  (html-mode)
  (goto-char (point-min))
  (let ((htm--erase-unknown-tags erase-unknown))
    (htm--find-close-while-parsing nil))
  (goto-char (point-min))
  (while (search-forward-regexp "\\(< *br *>\\|  \\)\n" nil t)
    (replace-match "\\1" :fixedcase)))

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
