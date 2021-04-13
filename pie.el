;;; pie.el --- Mode for editing Pie programs        -*- lexical-binding: t; -*-

;; Copyright (C) 2021  jao

;; Author: jao <mail@jao.io>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
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

;; Simple syntax highlighting for Pie, and evaluation using the pie-hs
;; REPL.

;; e ::= (the e e) Type annotation
;; | x Variable reference
;; | Atom Atom type
;; | '⌈sym⌉ Atom literal
;; | (Pair e e) Non-dependent pair type
;; | (Σ ((x e)+) e) Dependent pair type
;; | (cons e e) Pair constructor
;; | (car e) First projection
;; | (cdr e) Second projection
;; | (→ e e+) Non-dependent function type
;; | (Π ((x e)+) e) Dependent function type
;; | (λ (x+) e) Functions
;; | (e e+) Application
;; | Nat Natural number type
;; | zero Zero
;; | (add1 e) Successor
;; | ⌈n⌉ Natural number literal
;; | (which-Nat e e e) Case operator on natural numbers
;; | (iter-Nat e e e) Simply-typed iteration on natural numbers
;; | (rec-Nat e e e) Simply-typed recursion on natural numbers
;; | (ind-Nat e e e e) Induction on natural numbers
;; | (List e) List type
;; | nil Empty list
;; | (:: e e) List expansion
;; | (rec-List e e e) Simply-typed list recursion
;; | (ind-List e e e e) Induction on lists
;; | (Vec e e) Length-indexed vector type
;; | vecnil Empty vector
;; | (vec:: e e) Vector extension
;; | (head e) Head of a vector
;; | (tail e) Tail of a vector
;; | (ind-Vec e e e e e) Induction on vectors
;; | (= e e e) Equality type
;; | (same e) Reﬂexivity of equality
;; | (symm e) Symmetry of equality
;; | (cong e e) Equality is a congruence
;; | (replace e e e) Transportation along equality
;; | (trans e e) Transitivity of equality
;; | (ind-= e e e) Induction on equality
;; | (Either e e) Sum type
;; | (left e) First injection
;; | (right e) Second injection
;; | (ind-Either e e e e) Eliminator for sums
;; | Trivial Unit type
;; | sole Unit constructor
;; | Absurd Empty type
;; | (ind-Absurd e e) Eliminator for empty type (a.k.a. ex falso quodlibet)
;; | U Universe

;;; Code:

(require 'lisp-mode)


;; Customization

(defgroup pie nil
  "Options for pie.")

(defface pie-definition '((t :inherit bold))
  "Face for defining forms (the, claim, define).")

(defface pie-builtin '((t :inherit font-lock-builtin-face))
  "Face for built-ins")

(defface pie-type '((t :inherit italic))
  "Face for type names")

(defface pie-atom '((t :inherit font-lock-keyword-face))
  "Face for atoms")

(defface pie-function '((t :inherit font-lock-function-name-face))
  "Face for functions")


;; Mode

(defvar pie-mode-syntax-table
  (let ((st (make-syntax-table))
        (i (1+ ?9)))
    ;; Symbol constituents (FIXME: we ignore chars outside ascii)
    (while (< i ?A)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?Z))
    (while (< i ?a)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))

    (modify-syntax-entry ?- "_   " st)

    ;; Whitespace
    (modify-syntax-entry ?\t "    " st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?\f "    " st)
    (modify-syntax-entry ?\r "    " st)
    (modify-syntax-entry ?\s "    " st)

    (modify-syntax-entry ?\( "()  " st)
    (modify-syntax-entry ?\) ")(  " st)

    (modify-syntax-entry ?\; "<"    st)
    (modify-syntax-entry ?\" "\"   " st)
    (modify-syntax-entry ?' "'   " st)
    st))

(defvar pie-syntax-forms
  '("Pi" "Sigma" "lambda" "->" "the" "claim" "define" "car" "cdr" "left" "right"))
(defvar pie-builtin-types
  '("Atom" "Pair" "Nat" "List" "Vec" "Either" "Trivial" "Absurd" "U" "="))
(defvar pie-builtin-constructors
  '("cons" "zero" "add1" "nil" "::" "vecnil" "vec::" "sole"))
(defvar pie-builtin-eliminators
  '("which-Nat" "iter-Nat" "rec-Nat" "ind-Nat" "rec-List" "ind-List"
    "head" "tail" "ind-Vec" "ind-Either" "ind-Absurd"))
(defvar pie-builtin-functions
  '("same" "symm" "cong" "replace" "trans" "ind-="))

(defvar pie-font-lock-keywords
  `(("'[a-zA-z-]+" . 'pie-atom)
    ("(\\(define\\|the\\|claim\\)\\>" (1 'pie-definition))
    (,(concat "(" (regexp-opt pie-syntax-forms t)) (1 'pie-builtin))
    (,(regexp-opt (append pie-builtin-functions
                          pie-builtin-constructors
                          pie-builtin-eliminators)
                  t)
     (1 'pie-function))
    (,(concat "\\<" (regexp-opt pie-builtin-types t) "\\>") . 'pie-type)
    ("\\<[A-Z][A-Za-z]*\\>" . 'pie-type)
    ("(\\(define\\)\\>[ \t]*\\(\\sw+\\)[ \t]+(lambda "
     (1 'pie-definition)
     (2 'pie-function))))

(defvar pie--static-completion-list
  (sort (append pie-builtin-functions
                pie-builtin-constructors
                pie-builtin-eliminators
                pie-syntax-forms
                pie-builtin-types)
        'string-lessp))

(defun pie-complete-symbol-at-point ()
  (let* ((beg (save-excursion (skip-syntax-backward "^-()>") (point)))
         (end (+ beg (length (thing-at-point 'symbol)))))
    (when (> end beg)
      (let ((prefix (buffer-substring-no-properties beg end)))
        (list beg
              (min (point-max) end)
              (completion-table-dynamic
               `(lambda (_)
                  (all-completions ,prefix
                                   pie--static-completion-list))))))))

(defvar pie-mode-line-process "")

(defvar pie-mode-map
  (let ((smap (make-sparse-keymap))
	(map (make-sparse-keymap "Pie")))
    (set-keymap-parent smap lisp-mode-shared-map)
    (define-key smap [menu-bar pie] (cons "Pie" map))
    ;; (define-key map [run-pie] '("Run Inferior Pie" . run-pie))
    (define-key map [uncomment-region]
      '("Uncomment Out Region" . (lambda (beg end)
                                   (interactive "r")
                                   (comment-region beg end '(4)))))
    (define-key map [comment-region] '("Comment Out Region" . comment-region))
    (define-key map [indent-region] '("Indent Region" . indent-region))
    (define-key map [indent-line] '("Indent Line" . lisp-indent-line))
    (put 'comment-region 'menu-enable 'mark-active)
    (put 'uncomment-region 'menu-enable 'mark-active)
    (put 'indent-region 'menu-enable 'mark-active)
    smap)
  "Keymap for WhizzML mode.")

(defun pie--indent (&rest args)
  (put 'Pi 'lisp-indent-function 1)
  (put 'Sigma 'lisp-indent-function 1)
  (prog1 (apply #'lisp-indent-function args)
    (put 'Pi 'lisp-indent-function nil)
    (put 'Sigma 'lisp-indent-function nil)))

;;;###autoload
(define-derived-mode pie-mode prog-mode "Pie"
  "Major mode for editing Pie code.

Commands:
\\{pie-mode-map}"
  (set-syntax-table pie-mode-syntax-table)
  (setq-local
   adaptive-fill-mode nil
   paragraph-separate paragraph-start
   paragraph-ignore-fill-prefix t
   fill-paragraph-function 'lisp-fill-paragraph
   indent-line-function 'lisp-indent-line
   lisp-indent-function 'pie--indent
   parse-sexp-ignore-comments t
   comment-start ";"
   comment-add 1
   comment-start-skip ";+[ \t]*"
   comment-use-syntax t
   comment-column 40
   mode-line-process '("" pie-mode-line-process)
   completion-at-point-functions '(pie-complete-symbol-at-point)
   font-lock-defaults
   '(pie-font-lock-keywords nil nil nil
                            beginning-of-defun
                            (font-lock-mark-block-function . mark-defun))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pie\\'" . pie-mode))


;; Repl


(provide 'pie)
;;; pie.el ends here
