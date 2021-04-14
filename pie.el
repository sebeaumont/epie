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
(require 'subr-x)
(require 'comint)


;; Customization

(defgroup pie nil
  "Options for pie."
  :group 'programming)

(defface pie-definition '((t :inherit bold))
  "Face for defining forms (the, claim, define).")

(defface pie-builtin '((t :inherit font-lock-builtin-face))
  "Face for built-ins.")

(defface pie-type '((t :inherit italic))
  "Face for type names.")

(defface pie-atom '((t :inherit font-lock-keyword-face))
  "Face for atoms.")

(defface pie-function '((t :inherit font-lock-function-name-face))
  "Face for functions.")

(defface pie-annotation '((t :inherit font-lock-comment-face))
  "Face for pie-hs annotations.")


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
  `(("^#lang pie[ \t]*$" . font-lock-comment-face)
    ("'[a-zA-z-]+" . 'pie-atom)
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
  "The CAPF function."
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

(defconst pie--one-indent-forms
  '(Pi Sigma which-Nat iter-Nat rec-Nat ind-Nat rec-List ind-List)
  "Internal constant.")

(defun pie--indent (&rest args)
  "Indenting function, based on Lisp's indent function called with ARGS."
  (dolist (f pie--one-indent-forms) (put f 'lisp-indent-function 1))
  (prog1 (apply #'lisp-indent-function args)
    (dolist (f pie--one-indent-forms) (put f 'lisp-indent-function nil))))

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

(defcustom pie-path "pie"
  "The path to your pie-hs compiled executable."
  :type 'string)

(defvar pie--last-pie-buffer nil)

(defvar pie--repl-buffer "*pie*")
(defun pie--repl-buffer ()
  "Grabs repl's buffer, checdock police."
  (get-buffer pie--repl-buffer))

(defun pie-pop-to-repl ()
  "Pop to the active Pie REPL."
  (interactive)
  (pop-to-buffer (pie--repl-buffer)))

(defvar pie--out-buffer "*pie-output*")
(defvar pie--out-font-lock-keywords
  (cons '("^[^:]+\\(: .+\\)$" (1 'pie-annotation)) pie-font-lock-keywords))

(define-derived-mode pie-output-mode pie-mode "pie-output"
  "Mode to display pie-hs output."
  (setq-local font-lock-defaults '(pie--out-font-lock-keywords)))

(defun pie--out-buffer ()
  "Access to the raw pie-hs output."
  (when (not (get-buffer pie--out-buffer))
    (with-current-buffer (get-buffer-create pie--out-buffer)
      (pie-output-mode)))
  (get-buffer pie--out-buffer))

(add-to-list 'display-buffer-alist
             `(,(regexp-quote pie--out-buffer)
               (display-buffer-below-selected)
               (window-height . 20)
               (window-min-height . 10)))

(defun pie-back-to-buffer ()
  "Return to the last Pie code buffer."
  (interactive)
  (when (get-buffer pie--last-pie-buffer)
    (pop-to-buffer pie--last-pie-buffer)))

(defconst pie--input-loc-rx
  "^[^:\n]+:\\([0-9]+\\)\\.\\([0-9]+\\)-\\([0-9]+\\)\\.\\([0-9]+\\)")

(defun pie--extract-subcmd (lines l0 c0 l1 c1)
  "Extract substring from LINES with lines/columns L0, C0, L1, C1."
  (let* ((l0s (elt lines (1- l0)))
         (l0s (if (= l0 l1)
                  (substring l0s (1- c0) (1- c1))
                (substring l0s (1- c0))))
         (l1s (when (not (= l0 l1))
                (substring (elt lines (1- l1)) 0 (1- c1)))))
    (if l1s (concat l0s "\n" l1s) l0s)))

(defun pie--parse-cmd-output (cmd)
  "Parse output of CMD."
  (let ((clines (split-string cmd "\n"))
        (last-line 0))
    (goto-char (point-min))
    (while (re-search-forward pie--input-loc-rx nil t)
      (let ((l0 (string-to-number (match-string 1)))
            (c0 (string-to-number (match-string 2)))
            (l1 (string-to-number (match-string 3)))
            (c1 (string-to-number (match-string 4))))
        (replace-match "")
        (when (not (= last-line l0))
          (insert (format "\n;; line %s\n" l0)))
        (setq last-line l0)
        (insert (pie--extract-subcmd clines l0 c0 l1 c1))
        (forward-line)))))

(defun pie--parse-wait-output (buf proc)
  "Wait for output of PROC to cease in BUF."
  (with-current-buffer buf
    (while (and (null comint-redirect-completed) (accept-process-output proc)))))

(defun pie--send-command (cmd &optional code)
  "Sends CMD to the Pie REPL and collects the result.
CODE is the evaluated code, when not directly given by CMD."
  (when-let ((repl (pie--repl-buffer)))
    (let ((proc (get-buffer-process repl))
          (out (pie--out-buffer)))
      (with-current-buffer out
        (read-only-mode -1)
        (erase-buffer)
        (comint-redirect-send-command-to-process cmd out proc nil t)
        (pie--parse-wait-output repl proc)
        (pie--parse-cmd-output (or code cmd))
        (goto-char (point-min))
        (read-only-mode 1)))))

(defun pie--eval (s &optional code)
  "Asks the REPL to eval the string or form S.
CODE is the code that S causes to evaluate."
  (when s
    (let* ((s (if (stringp s) s (format "%S" s)))
           (s (replace-regexp-in-string "\\(;.*\\)?\n" " " s))
           (s (replace-regexp-in-string " +" " " s)))
      (pie--send-command (format "%s" s) code)
      (display-buffer (pie--out-buffer)))))

(defun pie-eval-last-sexp ()
  "Sends to the Pie repl sexp before point for evaluation."
  (interactive)
  (save-current-buffer
    (let ((p (point)))
      (save-excursion
        (backward-sexp)
        (pie--eval (buffer-substring-no-properties (point) p))))))

;; Needs more work: pie-hs doesn't like more than one form in a single line
;; so we'd need aggregating multiple evaluations
;; (defun pie-eval-region ()
;;   "Sends to the Pie repl sexp before point for evaluation."
;;   (interactive)
;;   (let ((b (region-beginning))
;;         (e (region-end)))
;;     (pie--eval (buffer-substring-no-properties b e))))

(defun pie-load-buffer (&optional buffer)
  "Send the contents of a given BUFFER to the Pie repl, resetting it."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (pie--eval (format ":load %s" (buffer-file-name buffer))
               (with-current-buffer buffer (buffer-string)))))

(defun pie--repl-send-simple (cmd)
  "Just send CMD to the REPL, without looking at th output."
  (if-let (rb (pie--repl-buffer))
      (with-current-buffer rb (comint-simple-send nil cmd))
    (user-error "No active Pie REPL")))

(defun pie-verbose-output ()
  "Set verbose pie-hs output."
  (interactive)
  (pie--repl-send-simple ":verbose"))

(defun pie-concise-output ()
  "Set concise pie-hs output."
  (interactive)
  (pie--repl-send-simple ":concise"))

(defun pie--sentinel (proc _event)
  "Prints a farewell when PROC ended."
  (let ((pb (process-buffer proc)))
    (when (buffer-live-p pb)
      (with-current-buffer pb
        (comint-write-input-ring)
        (insert "\nIt's been nice interacting with you!\n")))))

;;;###autoload
(define-derived-mode pie-repl-mode comint-mode "Pie REPL"
  "A very simple comint-based mode to run pie-hs."
  (setq comint-prompt-read-only t
        comint-use-prompt-regexp t
        comint-prompt-regexp (regexp-quote "Π> ")
        comint-input-ring-file-name
        (expand-file-name "~/.emacs.d/cache/pie.history")
        comint-input-ignoredups t
        comint-input-ring-size 10000)
  (add-hook 'comint-dynamic-complete-functions #'pie-complete-symbol-at-point
            nil t)
  (add-hook 'completion-at-point-functions #'pie-complete-symbol-at-point t t)
  (add-hook 'kill-buffer-hook #'comint-write-input-ring nil t)
  (set-process-sentinel (get-buffer-process (current-buffer)) #'pie--sentinel)
  (comint-read-input-ring t))

(define-key pie-mode-map "\C-cz" #'pie-run)
(define-key pie-mode-map "\C-c\C-z" #'pie-run)
(define-key pie-mode-map "\C-cl" #'pie-load-buffer)
(define-key pie-mode-map "\C-c\C-l" #'pie-load-buffer)
(define-key pie-mode-map "\C-c\C-e" #'pie-eval-last-sexp)
(define-key pie-mode-map "\C-x\C-e" #'pie-eval-last-sexp)
(define-key pie-mode-map "\C-c\C-v" #'pie-verbose-output)
(define-key pie-mode-map "\C-c\C-V" #'pie-concise-output)

(define-key pie-repl-mode-map "\M-p" #'comint-previous-matching-input-from-input)
(define-key pie-repl-mode-map "\M-n" #'comint-next-matching-input-from-input)
(define-key pie-repl-mode-map "\C-cz" #'pie-back-to-buffer)
(define-key pie-repl-mode-map "\C-c\C-z" #'pie-back-to-buffer)
(define-key pie-repl-mode-map "\C-c\C-v" #'pie-verbose-output)
(define-key pie-repl-mode-map "\C-c\C-V" #'pie-concise-output)

;;;###autoload
(defun pie-run ()
  "Run or switch to an existing Pie REPL."
  (interactive)
  (when (derived-mode-p 'pie-mode)
    (setq pie--last-pie-buffer (current-buffer)))
  (pop-to-buffer (make-comint "pie" pie-path))
  (unless (derived-mode-p 'pie-repl-mode)
    (pie-repl-mode)))

(defvar company-global-modes)

(with-eval-after-load "company"
  (when (listp company-global-modes)
    (add-to-list 'company-global-modes 'pie-repl-mode)))


(provide 'pie)
;;; pie.el ends here
