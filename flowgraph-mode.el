;;; flowgraph-mode.el --- Major mode for editing flowgraph -*- lexical-binding: t -*-

;; Author: tu10ng
;; URL: https://github.com/tu10ng/flowgraph-mode
;; Version: 20240808
;; Package-Requires: ((emacs "29.0"))
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Keywords: tools

;;; Commentary:

;; A major mode for editing flowgraph.

;; used when we need to manually write down function flowgraph.

;;; Code:
(require 'font-lock)
(require 'thingatpt)
(require 'rx)
(require 'seq)


;;; Customization

(defgroup flowgraph nil
  "Major mode for editing flowgraph."
  :prefix "flowgraph-"
  :group 'text
  :link '(url-link :tag "Github" "https://github.com/tu10ng/flowgraph-mode"))

(defcustom flowgraph-indent-offset 4
  "Indentation offset."
  :type 'integer
  :group 'flowgraph
  :safe #'integerp)


;;; Key bindings

(defvar flowgraph-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") #'flowgraph-tab)
    map)
  "Keymap for `flowgraph-mode'.")


;;; rx-wrappers for flowgraph

(defvar flowgraph--rx-bindings)

(setq
 flowgraph--rx-bindings
 '((symbol (&rest x) (seq symbol-start (or x) symbol-end))
   (ws (* (any " \t")))
   (ws+ (+ (any " \t")))
   (name (symbol (seq (+ (any alpha "_")) (* (any alnum "_")))))
   (funcname name)
   (keyword (symbol "return"))))

(defmacro flowgraph-rx (&rest regexps)
  (eval `(rx-let ,flowgraph--rx-bindings
           (rx ,@regexps))))

(defun flowgraph-rx-to-string (form &optional no-group)
  (rx-let-eval flowgraph--rx-bindings
    (rx-to-string form no-group)))


;;; Font-lock & syntax

(defface flowgraph-inlay-hint-face '((t (:height 0.8 :inherit shadow)))
  "Face used for inlay hint overlays.")

(defvar flowgraph-font-lock-keywords
  `((,(ahk-rx keyword)
     . font-lock-keyword-face)
    
    ;; function parameter
    (,(ahk-rx (seq funcname ws (group-n 1 "(" (*? nonl) ")")))
     (1 'flowgraph-inlay-hint-face))))

(defvar flowgraph-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    (modify-syntax-entry ?\; "<" syntax-table)
    (modify-syntax-entry ?\n ">" syntax-table)
    (modify-syntax-entry ?\^m "> b" syntax-table)

    (modify-syntax-entry ?_ "_" syntax-table)
    (modify-syntax-entry ?+ "." syntax-table)
    (modify-syntax-entry ?- "." syntax-table)
    (modify-syntax-entry ?= "." syntax-table)
    (modify-syntax-entry ?% "." syntax-table)
    (modify-syntax-entry ?< "." syntax-table)
    (modify-syntax-entry ?> "." syntax-table)
    (modify-syntax-entry ?& "." syntax-table)
    (modify-syntax-entry ?| "." syntax-table)
    syntax-table)
  "Syntax table for `flowgraph-mode'.")


;;; indentation

(defun flowgraph-paren-level () (nth 0 (syntax-ppss)))
(defun flowgraph-in-str () (nth 3 (syntax-ppss)))
(defun flowgraph-in-str-or-cmnt () (nth 8 (syntax-ppss)))
(defun flowgraph-rewind-past-str-cmnt () (goto-char (nth 8 (syntax-ppss))))

(defun flowgraph-indent-aligned? ()
  (= (% (current-indentation) flowgraph-indent-offset) 0))

(defun flowgraph-indent-line ()
  "Indent current line.

indentation behaves like haskell."
  (let ((indent (save-excursion
                  (back-to-indentation)
                  (let* ((baseline (save-excursion
                                     (forward-line -1)
                                     (while (not (flowgraph-indent-aligned?))
                                       (forward-line -1))
                                     (current-indentation))))
                    (cond
                     ((and (flowgraph-indent-aligned?)
                           (/= (current-indentation) 0))
                      (current-indentation))
                     (t
                      baseline))))))
    (when indent
      (if (<= (current-column) (current-indentation))
          (indent-line-to indent)
        (save-excursion
          (indent-line-to indent))))))

(defun flowgraph-tab ()
  "Indent current line to the right, then deindent in a loop manner."
  (interactive)
  (let* ((baseline (save-excursion
                     (forward-line -1)
                     (while (not (flowgraph-indent-aligned?))
                       (forward-line -1))
                     (current-indentation)))
         (max-indent (+ baseline flowgraph-indent-offset))
         (indent (current-indentation))
         (next-indent (if (< (- indent flowgraph-indent-offset) 0)
                          max-indent
                        (- indent flowgraph-indent-offset)))
         (indent-positions
          (let (indent-positions)
            (dotimes (i (1+ (/ max-indent flowgraph-indent-offset)))
              (push (* i flowgraph-indent-offset) indent-positions))
            (nreverse indent-positions))))
    (cond
     ((not (eq this-command last-command))
      (indent-line-to max-indent))
     (t
      (indent-line-to next-indent)))))


;;; Navigation


;;; Utility functions


;;; Major mode

;;;###autoload
(define-derived-mode flowgraph-mode prog-mode "flowgraph"
  "Major mode for editing flowgraph.

\\{flowgraph-mode-map}"
  :syntax-table flowgraph-mode-syntax-table
  (setq-local font-lock-defaults '(flowgraph-font-lock-keywords
                                   nil nil nil nil))
  
  (setq-local indent-line-function #'flowgraph-indent-line)
  
  (setq-local parse-sexp-ignore-comments t)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local comment-start ";")
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-use-syntax t)
  (setq-local paragraph-start (concat "$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)

  (add-to-list 'auto-mode-alist '("\\.flowgraph\\'" . flowgraph-mode)))

(provide 'flowgraph-mode)

;;; flowgraph-mode.el ends here
