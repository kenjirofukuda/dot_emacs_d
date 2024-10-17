;;; mpascal.el --- major mode for editing Object Pascal source in Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 1998-1999, 2001-2024 Free Software Foundation, Inc.

;; Authors: Ray Blaak <blaak@infomatch.com>,
;;          Simon South <ssouth@member.fsf.org>
;; Maintainer: Simon South <ssouth@member.fsf.org>
;; Modified: Kenjiro Fukuda <kenjirofukuda@gmail.com>
;; Keywords: languages

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; To enter Mpascal mode when you find an Object Pascal source file,
;; one must override the auto-mode-alist to associate Mpascal with
;; .pas (and .dpr and .dpk) files.  Emacs, by default, will otherwise
;; enter Pascal mode.  For example:
;;
;; (add-to-list 'auto-mode-alist
;;              '("\\.\\(pas\\|dpr\\|dpk\\)\\'" . mpascal-mode))

;; When you have entered Mpascal mode, you may get more info by pressing
;; C-h m.

;; This Mpascal mode implementation is fairly tolerant of syntax errors,
;; relying as much as possible on the indentation of the previous statement.
;; This also makes it faster and simpler, since there is less searching for
;; properly constructed beginnings.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defgroup mpascal nil
  "Major mode for editing Mpascal source in Emacs."
  :version "24.4"
  :group 'languages)

(defconst mpascal-debug nil
  "Non-nil if in debug mode.")

(define-obsolete-variable-alias
  'delphi-search-path 'mpascal-search-path "24.4")
(defcustom mpascal-search-path "."
  "Directories to search when finding external units.
It is a list of directory strings.  If only a single directory,
it can be a single string instead of a list.  If a directory
ends in \"...\" then that directory is recursively searched."
  :type 'string)

(define-obsolete-variable-alias
  'delphi-indent-level 'mpascal-indent-level "24.4")
(defcustom mpascal-indent-level 2
  "Indentation of Mpascal statements with respect to containing block.
E.g.

begin
   // This is an indent of 3.
end;"
  :type 'integer)

(define-obsolete-variable-alias
  'delphi-compound-block-indent 'mpascal-compound-block-indent "24.4")
(defcustom mpascal-compound-block-indent 2
  "Extra indentation for blocks in compound statements.  E.g.

// block indent = 0     vs      // block indent = 2
if b then                       if b then
begin                             begin
end else begin                    end
end;                            else
                                  begin
                                  end;"
  :type 'integer)

(define-obsolete-variable-alias
  'delphi-case-label-indent 'mpascal-case-label-indent "24.4")
(defcustom mpascal-case-label-indent mpascal-indent-level
  "Extra indentation for case statement labels.  E.g.

// case indent = 0      vs      // case indent = 3
case value of                   case value of
v1: process_v1;                    v1: process_v1;
v2: process_v2;                    v2: process_v2;
else                            else
   process_else;                   process_else;
end;                            end;"
  :type 'integer)

(define-obsolete-variable-alias 'delphi-verbose 'mpascal-verbose "24.4")
(defcustom mpascal-verbose t ; nil
  "If true then Mpascal token processing progress is reported to the user."
  :type 'boolean)

(define-obsolete-variable-alias
  'delphi-tab-always-indents 'mpascal-tab-always-indents "24.4")
(defcustom mpascal-tab-always-indents tab-always-indent
  "Non-nil means `mpascal-tab' should always reindent the current line.
That is, regardless of where in the line point is at the time."
  :type 'boolean)

(make-obsolete-variable 'mpascal-tab-always-indents
                        "use `indent-for-tab-command' and `tab-always-indent'."
                        "24.4")

(defconst mpascal-directives
  '(absolute abstract assembler automated cdecl default dispid dynamic
    export external far forward index inline message name near nodefault
    overload override pascal private protected public published read readonly
    register reintroduce resident resourcestring safecall stdcall stored
    virtual write writeonly)
  "Mpascal4 directives.")

(defconst mpascal-keywords
  (append
   '(;; Keywords.
     and array as asm at begin case class const constructor contains
     destructor dispinterface div do downto else end except exports
     file finalization finally for function goto if implementation implements
     in inherited initialization interface is label library mod nil not
     of object on or out package packed procedure program property
     raise record repeat requires result self set shl shr then threadvar
     to try type unit uses until var while with xor
		 
		 ;; MPW Pascal/THINKC Pascal
		 leave univ
		 
     ;; These routines should be keywords, if Borland had the balls.
     break exit)

   ;; We want directives to look like keywords.
   mpascal-directives)
  "Mpascal4 keywords.")

(defconst mpascal-previous-terminators '(semicolon comma)
  "Expression/statement terminators that denote a previous expression.")

(defconst mpascal-comments
  '(comment-single-line comment-multi-line-1 comment-multi-line-2)
  "Tokens that represent comments.")

(defconst mpascal-whitespace `(space newline ,@mpascal-comments)
  "Tokens that are considered whitespace.")

(defconst mpascal-routine-statements
  '(procedure function constructor destructor property)
  "Marks the start of a routine, or routine-ish looking expression.")

(defconst mpascal-body-expr-statements '(if while for on)
  "Statements that have either a single statement or a block as a body and also
are followed by an expression.")

(defconst mpascal-expr-statements `(case ,@mpascal-body-expr-statements)
  "Expression statements contain expressions after their keyword.")

(defconst mpascal-body-statements `(else ,@mpascal-body-expr-statements)
  "Statements that have either a single statement or a block as a body.")

(defconst mpascal-expr-delimiters '(then do of)
  "Expression delimiter tokens.")

(defconst mpascal-binary-ops
  '(plus minus equals not-equals times divides div mod and or xor)
  "Mpascal binary operations.")

(defconst mpascal-visibilities '(public private protected published automated)
  "Class visibilities.")

(defconst mpascal-block-statements
  '(begin try case repeat initialization finalization asm)
  "Statements that contain multiple substatements.")

(defconst mpascal-mid-block-statements
  `(except finally ,@mpascal-visibilities)
  "Statements that mark mid sections of the enclosing block.")

(defconst mpascal-end-block-statements '(end until)
  "Statements that end block sections.")

(defconst mpascal-match-block-statements
  `(,@mpascal-end-block-statements ,@mpascal-mid-block-statements)
  "Statements that match the indentation of the parent block.")

(defconst mpascal-decl-sections '(type const var label resourcestring)
  "Denotes the start of a declaration section.")

(defconst mpascal-interface-types '(dispinterface interface)
  "Interface types.")

(defconst mpascal-class-types '(class object)
  "Class types.")

(defconst mpascal-composite-types
  `(,@mpascal-class-types ,@mpascal-interface-types record)
  "Types that contain declarations within them.")

(defconst mpascal-unit-sections
  '(interface implementation program library package)
  "Unit sections within which the indent is 0.")

(defconst mpascal-use-clauses '(uses requires exports contains)
  "Statements that refer to foreign symbols.")

(defconst mpascal-unit-statements
  `(,@mpascal-use-clauses ,@mpascal-unit-sections initialization finalization)
  "Statements indented at level 0.")

(defconst mpascal-decl-delimiters
  `(,@mpascal-decl-sections ,@mpascal-unit-statements
    ,@mpascal-routine-statements)
  "Statements that a declaration statement should align with.")

(defconst mpascal-decl-matchers
  `(begin ,@mpascal-decl-sections)
  "Statements that should match to declaration statement indentation.")

(defconst mpascal-enclosing-statements
  `(,@mpascal-block-statements ,@mpascal-mid-block-statements
    ,@mpascal-decl-sections ,@mpascal-use-clauses ,@mpascal-routine-statements)
  "Delimits an enclosing statement.")

(defconst mpascal-previous-statements
  `(,@mpascal-unit-statements ,@mpascal-routine-statements)
  "Delimits a previous statement.")

(defconst mpascal-previous-enclosing-statements
  `(,@mpascal-block-statements ,@mpascal-mid-block-statements
    ,@mpascal-decl-sections)
  "Delimits a previous enclosing statement.")

(defconst mpascal-begin-enclosing-tokens
  `(,@mpascal-block-statements ,@mpascal-mid-block-statements)
  "Tokens that a begin token indents from.")

(defconst mpascal-begin-previous-tokens
  `(,@mpascal-decl-sections ,@mpascal-routine-statements)
  "Tokens that a begin token aligns with, but only if not part of a \
nested routine.")

(defconst mpascal-space-chars "\000-\011\013- ") ; all except \n
(defconst mpascal-non-space-chars (concat "^" mpascal-space-chars))
(defconst mpascal-spaces-re (concat "[" mpascal-space-chars "]*"))
(defconst mpascal-leading-spaces-re (concat "^" mpascal-spaces-re))
(defconst mpascal-word-chars "a-zA-Z0-9_")

(defvar mpascal-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\\ "." st)    ; bug#22224
    ;; Strings.
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    ;; Comments.
    (modify-syntax-entry ?\{ "<" st)
    (modify-syntax-entry ?\} ">" st)
    (modify-syntax-entry ?\( "()1" st)
    (modify-syntax-entry ?\) ")(4" st)
    (modify-syntax-entry ?*  ". 23b" st)
    (modify-syntax-entry ?/  ". 12c" st)
    (modify-syntax-entry ?\n "> c" st)
    st))

(defmacro mpascal-save-excursion (&rest forms)
  ;; Executes the forms such that any movements have no effect, including
  ;; searches.
  (declare (debug t))
  `(save-excursion
     (save-match-data
      (let ((deactivate-mark nil))
        (progn ,@forms)))))


(eval-when-compile
  (pcase-defmacro mpascal--in (set)
    `(pred (pcase--flip memq ,set))))

(defun mpascal-string-of (start end)
  ;; Returns the buffer string from start to end.
  (buffer-substring-no-properties start end))

(defun mpascal-looking-at-string (p s)
  ;; True if point p marks the start of string s. s is not a regular
  ;; expression.
  (let ((limit (+ p (length s))))
    (and (<= limit (point-max))
         (string= s (mpascal-string-of p limit)))))

(defun mpascal-token-of (kind start end)
  ;; Constructs a token from a kind symbol and its start/end points.
  `[,kind ,start ,end])

(defsubst mpascal-token-kind (token)
  ;; Returns the kind symbol of the token.
  (if token (aref token 0) nil))

(defun mpascal-set-token-kind (token to-kind)
  ;; Sets the kind symbol of the token.
  (if token (aset token 0 to-kind)))

(defsubst mpascal-token-start (token)
  ;; Returns the start point of the token.
  (if token (aref token 1) (point-min)))

(defsubst mpascal-token-end (token)
  ;; Returns the end point of the token.
  (if token (aref token 2) (point-min)))

(defun mpascal-set-token-start (token start)
  ;; Sets the start point of the token.
  (if token (aset token 1 start)))

(defun mpascal-set-token-end (token end)
  ;; Sets the end point of the token.
  (if token (aset token 2 end)))

(defun mpascal-token-string (token)
  ;; Returns the string image of the token.
  (if token
      (mpascal-string-of (mpascal-token-start token) (mpascal-token-end token))
    ""))

(defun mpascal-in-token (p token)
  ;; Returns true if the point p is within the token's start/end points.
  (and (<= (mpascal-token-start token) p) (< p (mpascal-token-end token))))

(defun mpascal-column-of (p)
  ;; Returns the column of the point p.
  (save-excursion (goto-char p) (current-column)))

(defvar mpascal-progress-last-reported-point nil
  "The last point at which progress was reported.")

(defconst mpascal-parsing-progress-step 16384
  "Number of chars to process before the next parsing progress report.")
(defconst mpascal-scanning-progress-step 2048
  "Number of chars to process before the next scanning progress report.")

(defun mpascal-progress-start ()
  ;; Initializes progress reporting.
  (setq mpascal-progress-last-reported-point nil))

(defun mpascal-progress-done (&rest msgs)
  ;; Finalizes progress reporting.
  (setq mpascal-progress-last-reported-point nil)
  (when mpascal-verbose
     (if (null msgs)
         (message "")
       (apply #'message msgs))))

(defun mpascal-step-progress (p desc step-size)
  ;; If enough distance has elapsed since the last reported point, then report
  ;; the current progress to the user.
  (cond ((null mpascal-progress-last-reported-point)
         ;; This is the first progress step.
         (setq mpascal-progress-last-reported-point p))

        ((and mpascal-verbose
              (>= (abs (- p mpascal-progress-last-reported-point)) step-size))
         ;; Report the percentage complete.
         (setq mpascal-progress-last-reported-point p)
         (message "%s %s ... %d%%"
                  desc (buffer-name) (floor (* 100.0 p) (point-max))))))

(defun mpascal-next-line-start (&optional from-point)
  ;; Returns the first point of the next line.
  (let ((curr-point (point))
        (next nil))
    (if from-point (goto-char from-point))
    (end-of-line)
    (setq next (min (1+ (point)) (point-max)))
    (goto-char curr-point)
    next))

(defconst mpascal--literal-start-re (regexp-opt '("//" "{" "(*" "'" "\"")))

(defun mpascal-literal-kind (p)
  ;; Returns the literal kind the point p is in (or nil if not in a literal).
  (when (and (<= (point-min) p) (<= p (point-max)))
    (save-excursion
      (let ((ppss (syntax-ppss p)))
        ;; We want to return non-nil when right in front
        ;; of a comment/string.
        (if (null (nth 8 ppss))
            (when (looking-at mpascal--literal-start-re)
              (pcase (char-after)
                (?/  'comment-single-line)
                (?\{ 'comment-multi-line-1)
                (?\( 'comment-multi-line-2)
                (?\' 'string)
                (?\" 'double-quoted-string)))
          (if (nth 3 ppss)   ;String.
              (if (eq (nth 3 ppss) ?\")
                  'double-quoted-string 'string)
            (pcase (nth 7 ppss)
              (2 'comment-single-line)
              (1 'comment-multi-line-2)
              (_  'comment-multi-line-1))))))))

(defun mpascal-literal-start-pattern (literal-kind)
  ;; Returns the start pattern of the literal kind.
  (cdr (assoc literal-kind
              '((comment-single-line . "//")
                (comment-multi-line-1 . "{")
                (comment-multi-line-2 . "(*")
                (string . "'")
                (double-quoted-string . "\"")))))

(defun mpascal-literal-stop-pattern (literal-kind)
  ;; Returns the pattern that delimits end of the search for the literal kind.
  ;; These are regular expressions.
  (cdr (assoc literal-kind
              '((comment-single-line . "\n")
                (comment-multi-line-1 . "}")
                (comment-multi-line-2 . "\\*)")
                ;; Strings cannot span lines.
                (string . "['\n]")
                (double-quoted-string . "[\"\n]")))))

(defun mpascal-is-literal-end (p)
  ;; True if the point p is at the end point of a (completed) literal.
  (save-excursion
    (and (null (nth 8 (syntax-ppss p)))
         (nth 8 (syntax-ppss (1- p))))))

(defun mpascal-literal-token-at (p)
  "Return the literal token surrounding the point P, or nil if none."
  (save-excursion
    (let ((ppss (syntax-ppss p)))
      (when (or (nth 8 ppss) (looking-at mpascal--literal-start-re))
        (let* ((new-start (or (nth 8 ppss) p))
               (new-end (progn
                          (goto-char new-start)
                          (condition-case nil
                              (if (memq (char-after) '(?\' ?\"))
                                  (forward-sexp 1)
                                (forward-comment 1))
                            (scan-error (goto-char (point-max))))
                          (point))))
          (mpascal-token-of (mpascal-literal-kind p) new-start new-end))))))

(defun mpascal-point-token-at (p kind)
  ;; Returns the single character token at the point p.
  (mpascal-token-of kind p (1+ p)))

(defsubst mpascal-char-token-at (p char kind)
  ;; Returns the token at the point p that describes the specified character.
  ;; If not actually over such a character, nil is returned.
  (when (eq char (char-after p))
    (mpascal-token-of kind p (1+ p))))

(defun mpascal-charset-token-at (p charset kind)
  ;; Returns the token surrounding point p that contains only members of the
  ;; character set.
  (let ((currp (point))
        (end nil)
        (token nil))
    (goto-char p)
    (when (> (skip-chars-forward charset) 0)
       (setq end (point))
       (goto-char (1+ p))
       (skip-chars-backward charset)
       (setq token (mpascal-token-of kind (point) end)))
    (goto-char currp)
    token))

(defun mpascal-space-token-at (p)
  ;; If point p is surrounded by space characters, then return the token of the
  ;; contiguous spaces.
  (mpascal-charset-token-at p mpascal-space-chars 'space))

(defun mpascal-word-token-at (p)
  ;; If point p is over a word (i.e. identifier characters), then return a word
  ;; token. If the word is actually a keyword, then return the keyword token.
  (let ((word (mpascal-charset-token-at p mpascal-word-chars 'word)))
    (when word
      (let* ((word-image (downcase (mpascal-token-string word)))
             (keyword (intern-soft word-image)))
        (when (and (or keyword (string= "nil" word-image))
                   (memq keyword mpascal-keywords))
          (mpascal-set-token-kind word keyword))
        word))))

(defun mpascal-explicit-token-at (p token-string kind)
  ;; If point p is anywhere in the token string then returns the resulting
  ;; token.
  (let ((token (mpascal-charset-token-at p token-string kind)))
    (when (and token (string= token-string (mpascal-token-string token)))
       token)))

(defun mpascal-token-at (p)
  ;; Returns the token from parsing text at point p.
  (when (and (<= (point-min) p) (<= p (point-max)))
     (cond ((mpascal-char-token-at p ?\n 'newline))

           ((mpascal-literal-token-at p))

           ((mpascal-space-token-at p))

           ((mpascal-word-token-at p))

           ((mpascal-char-token-at p ?\( 'open-group))
           ((mpascal-char-token-at p ?\) 'close-group))
           ((mpascal-char-token-at p ?\[ 'open-group))
           ((mpascal-char-token-at p ?\] 'close-group))
           ((mpascal-char-token-at p ?\; 'semicolon))
           ((mpascal-char-token-at p ?. 'dot))
           ((mpascal-char-token-at p ?, 'comma))
           ((mpascal-char-token-at p ?= 'equals))
           ((mpascal-char-token-at p ?+ 'plus))
           ((mpascal-char-token-at p ?- 'minus))
           ((mpascal-char-token-at p ?* 'times))
           ((mpascal-char-token-at p ?/ 'divides))
           ((mpascal-char-token-at p ?: 'colon))

           ((mpascal-explicit-token-at p "<>" 'not-equals))

           ((mpascal-point-token-at p 'punctuation)))))

(defun mpascal-current-token ()
  ;; Returns the mpascal source token under the current point.
  (mpascal-token-at (point)))

(defun mpascal-next-token (token)
  ;; Returns the token after the specified token.
  (when token
     (let ((next (mpascal-token-at (mpascal-token-end token))))
       (if next
           (mpascal-step-progress (mpascal-token-start next) "Scanning"
                                 mpascal-scanning-progress-step))
       next)))

(defun mpascal-previous-token (token)
  ;; Returns the token before the specified token.
  (when token
     (let ((previous (mpascal-token-at (1- (mpascal-token-start token)))))
       (if previous
           (mpascal-step-progress (mpascal-token-start previous) "Scanning"
                                 mpascal-scanning-progress-step))
       previous)))

(defun mpascal-next-visible-token (token)
  ;; Returns the first non-space token after the specified token.
  (let (next-token)
    (while (progn
             (setq next-token (mpascal-next-token token))
             (memq (mpascal-token-kind next-token) '(space newline))))
    next-token))

(defun mpascal-group-start (from-token)
  ;; Returns the token that denotes the start of the ()/[] group.
  (let ((token (mpascal-previous-token from-token))
        (token-kind nil))
    (catch 'done
      (while token
        (setq token-kind (mpascal-token-kind token))
        (cond
         ;; Skip over nested groups.
         ((eq 'close-group token-kind) (setq token (mpascal-group-start token)))
         ((eq 'open-group token-kind) (throw 'done token)))
        (setq token (mpascal-previous-token token)))
      ;; Start not found.
      nil)))

(defun mpascal-group-end (from-token)
  ;; Returns the token that denotes the end of the ()/[] group.
  (let ((token (mpascal-next-token from-token))
        (token-kind nil))
    (catch 'done
      (while token
        (setq token-kind (mpascal-token-kind token))
        (cond
         ;; Skip over nested groups.
         ((eq 'open-group token-kind) (setq token (mpascal-group-end token)))
         ((eq 'close-group token-kind) (throw 'done token)))
        (setq token (mpascal-next-token token)))
      ;; end not found.
      nil)))

(defun mpascal-indent-of (token &optional offset)
  ;; Returns the start column of the token, plus any offset.
  (let ((indent (+ (mpascal-column-of (mpascal-token-start token))
                   (if offset offset 0))))
    (when mpascal-debug
      (mpascal-debug-log
       (concat "\n Indent of: %S %S"
               "\n column: %d indent: %d offset: %d")
       token (mpascal-token-string token)
       (mpascal-column-of (mpascal-token-start token))
       indent (if offset offset 0)))
    indent))

(defmacro mpascal--scan-non-whitespace-backward (token-var last-var
                                                 &rest pcases)
  (declare (debug (symbolp symbolp &rest (pcase-PAT body)))
           (indent 2))
  `(let ((,token-var ,token-var))
     (while (setq ,token-var (mpascal-previous-token ,token-var))
       ,(macroexp-let2 nil kind-var `(mpascal-token-kind ,token-var)
          `(unless (memq ,kind-var mpascal-whitespace)
             (pcase ,kind-var
               ,@pcases)
             ,(when last-var `(setq ,last-var ,token-var)))))))

(defun mpascal-line-indent-of (from-token &optional offset &rest terminators)
  ;; Returns the column of first non-space character on the token's line, plus
  ;; any offset. We also stop if one of the terminators or an open ( or [ is
  ;; encountered.
  (let ((token (mpascal-previous-token from-token))
        (last-token from-token)
        (kind nil))
    (catch 'done
      ;; FIXME: Can't use mpascal--scan-non-whitespace-backward here, because
      ;; we do need to pay attention to `newline'!
      (while token
        (setq kind (mpascal-token-kind token))
        (cond
         ;; Skip over ()/[] groups.
         ((eq 'close-group kind) (setq token (mpascal-group-start token)))

         ;; Stop at the beginning of the line or an open group.
         ((memq kind '(newline open-group)) (throw 'done nil))

         ;; Stop at one of the specified terminators.
         ((memq kind terminators) (throw 'done nil)))
        (unless (memq kind mpascal-whitespace) (setq last-token token))
        (setq token (mpascal-previous-token token))))
    (mpascal-indent-of last-token offset)))

(defun mpascal-stmt-line-indent-of (from-token &optional offset)
  ;; Like `mpascal-line-indent-of' except is also stops on a use clause, and
  ;; colons that precede statements (i.e. case labels).
  (let ((token (mpascal-previous-token from-token))
        (last-token from-token)
        (kind nil))
    (catch 'done
      ;; FIXME: Can't use mpascal--scan-non-whitespace-backward here, because
      ;; we do need to pay attention to `newline'!
      (while token
        (setq kind (mpascal-token-kind token))
        (cond
         ((and (eq 'colon kind)
               (memq (mpascal-token-kind last-token)
                     `(,@mpascal-block-statements
                       ,@mpascal-expr-statements)))
          ;; We hit a label followed by a statement. Indent to the statement.
          (throw 'done nil))

         ;; Skip over ()/[] groups.
         ((eq 'close-group kind) (setq token (mpascal-group-start token)))

         ((memq kind `(newline open-group ,@mpascal-use-clauses))
          ;; Stop at the beginning of the line, an open group, or a use clause
          (throw 'done nil)))
        (unless (memq kind mpascal-whitespace) (setq last-token token))
        (setq token (mpascal-previous-token token))))
    (mpascal-indent-of last-token offset)))

(defun mpascal-open-group-indent (token last-token &optional offset)
  ;; Returns the indent relative to an unmatched ( or [.
  (when (eq 'open-group (mpascal-token-kind token))
    (if last-token
        (mpascal-indent-of last-token offset)
      ;; There is nothing following the ( or [. Indent from its line.
      (mpascal-stmt-line-indent-of token mpascal-indent-level))))

(defun mpascal-composite-type-start (token last-token)
  ;; Returns true (actually the last-token) if the pair equals (= class), (=
  ;; dispinterface), (= interface), (= object), or (= record), and nil
  ;; otherwise.
  (if (and (eq 'equals (mpascal-token-kind token))
           (memq (mpascal-token-kind last-token) mpascal-composite-types))
      last-token))

(defun mpascal-is-simple-class-type (at-token limit-token)
  ;; True if at-token is the start of a simple class type. E.g.
  ;;   class of TClass;
  ;;   class (TBaseClass);
  ;;   class;
  (when (memq (mpascal-token-kind at-token) mpascal-class-types)
    (catch 'done
      ;; Scan until the semi colon.
      (let ((token (mpascal-next-token at-token))
            (token-kind nil)
            (limit (mpascal-token-start limit-token)))
        (while (and token (<= (mpascal-token-start token) limit))
          (setq token-kind (mpascal-token-kind token))
          (cond
           ;; A semicolon delimits the search.
           ((eq 'semicolon token-kind) (throw 'done token))

           ;; Skip over the inheritance list.
           ((eq 'open-group token-kind) (setq token (mpascal-group-end token)))

           ;; Only allow "of" and whitespace, and an identifier
           ((memq token-kind `(of word ,@mpascal-whitespace)))

           ;; Otherwise we are not in a simple class declaration.
           ((throw 'done nil)))
          (setq token (mpascal-next-token token)))))))

(defun mpascal-block-start (from-token &optional stop-on-class)
  ;; Returns the token that denotes the start of the block.
  (let ((token from-token)
        (last-token nil))
    (catch 'done
      (mpascal--scan-non-whitespace-backward token last-token
        ;; Skip over nested blocks.
        ((mpascal--in mpascal-end-block-statements)
         (setq token (mpascal-block-start token)))

        ;; Case block start found.
        ('case
         (throw 'done
                ;; As a special case, when a "case" block appears
                ;; within a record declaration (to denote a variant
                ;; part), the record declaration should be considered
                ;; the enclosing block.
                (let ((enclosing-token
                       (mpascal-block-start token
                                            'stop-on-class)))
                  (if (eq 'record
                          (mpascal-token-kind enclosing-token))
                      (if stop-on-class
                          enclosing-token
                        (mpascal-previous-token enclosing-token))
                    token))))

        ;; Regular block start found.
        ((mpascal--in mpascal-block-statements)
         (throw 'done token))

        ;; A class/record start also begins a block.
        ((guard (mpascal-composite-type-start token last-token))
         (throw 'done (if stop-on-class last-token token)))
        )
      ;; Start not found.
      nil)))

(defun mpascal-else-start (from-else)
  ;; Returns the token of the if or case statement.
  (let ((token from-else)
        (semicolon-count 0))
    (catch 'done
      (mpascal--scan-non-whitespace-backward token nil
        ;; Skip over nested groups.
        ('close-group (setq token (mpascal-group-start token)))

        ;; Skip over any nested blocks.
        ((mpascal--in mpascal-end-block-statements)
         (setq token (mpascal-block-start token)))

        ('semicolon
         ;; Semicolon means we are looking for an enclosing if, unless we
         ;; are in a case statement. Keep counts of the semicolons and decide
         ;; later.
         (setq semicolon-count (1+ semicolon-count)))

        ((and 'if (guard (= semicolon-count 0)))
         ;; We only can match an if when there have been no intervening
         ;; semicolons.
         (throw 'done token))

        ('case
         ;; We have hit a case statement start.
         (throw 'done token)))
      ;; No if or case statement found.
      nil)))

(defun mpascal-comment-content-start (comment)
  ;; Returns the point of the first non-space character in the comment.
  (let ((kind (mpascal-token-kind comment)))
    (when (memq kind mpascal-comments)
      (mpascal-save-excursion
       (goto-char (+ (mpascal-token-start comment)
                     (length (mpascal-literal-start-pattern kind))))
       (skip-chars-forward mpascal-space-chars)
       (point)))))

(defun mpascal-comment-block-start (comment)
  ;; Returns the starting comment token of a contiguous // comment block. If
  ;; the comment is multiline (i.e. {...} or (*...*)), the original comment is
  ;; returned.
  (if (not (eq 'comment-single-line (mpascal-token-kind comment)))
      comment
    ;; Scan until we run out of // comments.
    (let ((prev-comment comment)
          (start-comment comment))
      (while (let ((kind (mpascal-token-kind prev-comment)))
               (cond ((eq kind 'space))
                     ((eq kind 'comment-single-line)
                      (setq start-comment prev-comment))
                     (t nil)))
        (setq prev-comment (mpascal-previous-token prev-comment)))
      start-comment)))

(defun mpascal-comment-block-end (comment)
  ;; Returns the end comment token of a contiguous // comment block. If the
  ;; comment is multiline (i.e. {...} or (*...*)), the original comment is
  ;; returned.
  (if (not (eq 'comment-single-line (mpascal-token-kind comment)))
      comment
    ;; Scan until we run out of // comments.
    (let ((next-comment comment)
          (end-comment comment))
      (while (let ((kind (mpascal-token-kind next-comment)))
               (cond ((eq kind 'space))
                     ((eq kind 'comment-single-line)
                      (setq end-comment next-comment))
                     (t nil)))
        (setq next-comment (mpascal-next-token next-comment)))
      end-comment)))

(defun mpascal-on-first-comment-line (comment)
  ;; Returns true if the current point is on the first line of the comment.
  (save-excursion
    (let ((comment-start (mpascal-token-start comment))
          (current-point (point)))
      (goto-char comment-start)
      (end-of-line)
      (and (<= comment-start current-point) (<= current-point (point))))))

(defun mpascal-comment-indent-of (comment)
  ;; Returns the correct indentation for the comment.
  (let ((start-comment (mpascal-comment-block-start comment)))
    (if (and (eq start-comment comment)
             (mpascal-on-first-comment-line comment))
        ;; Indent as a statement.
        (mpascal-enclosing-indent-of comment)
      (save-excursion
        (let ((kind (mpascal-token-kind comment)))
          (beginning-of-line)
          (cond ((eq 'comment-single-line kind)
                 ;; Indent to the first comment in the // block.
                 (mpascal-indent-of start-comment))

                ((looking-at (concat mpascal-leading-spaces-re
                                     (mpascal-literal-stop-pattern kind)))
                 ;; Indent multi-line comment terminators to the comment start.
                 (mpascal-indent-of comment))

                ;; Indent according to the comment's content start.
                (t
                 (mpascal-column-of (mpascal-comment-content-start comment)))))))
    ))

(defun mpascal-is-use-clause-end (at-token last-token last-colon from-kind)
  ;; True if we are after the end of a uses type clause.
  (when (and last-token
             (not last-colon)
             (eq 'comma (mpascal-token-kind at-token))
             (eq 'semicolon from-kind))
    ;; Scan for the uses statement, just to be sure.
    (let ((token at-token))
      (catch 'done
        (mpascal--scan-non-whitespace-backward token nil
          ((mpascal--in mpascal-use-clauses)
           (throw 'done t))

          ;; Identifiers, strings, "in" keyword, and commas
          ;; are allowed in use clauses.
          ((or 'word 'comma 'in  'string 'double-quoted-string))

          ;; Nothing else is.
          (_ (throw 'done nil)))
        nil))))

(defun mpascal-is-block-after-expr-statement (token)
  ;; Returns true if we have a block token trailing an expression delimiter (of
  ;; presumably an expression statement).
  (when (memq (mpascal-token-kind token) mpascal-block-statements)
    (let ((previous (mpascal-previous-token token))
          (previous-kind nil))
      (while (progn
               (setq previous-kind (mpascal-token-kind previous))
               (eq previous-kind 'space))
        (setq previous (mpascal-previous-token previous)))
      (or (memq previous-kind mpascal-expr-delimiters)
          (eq previous-kind 'else)))))

(defun mpascal-previous-indent-of (from-token)
  ;; Returns the indentation of the previous statement of the token.
  (let ((token from-token)
        (from-kind (mpascal-token-kind from-token))
        (last-colon nil)
        (last-of nil)
        (last-token nil))
    (catch 'done
      (mpascal--scan-non-whitespace-backward token last-token
        ;; An open ( or [ always is an indent point.
        ('open-group
         (throw 'done (mpascal-open-group-indent token last-token)))

        ;; Skip over any ()/[] groups.
        ('close-group (setq token (mpascal-group-start token)))

        ((mpascal--in mpascal-end-block-statements)
         (if (eq 'newline (mpascal-token-kind (mpascal-previous-token token)))
             ;; We can stop at an end token that is right up against the
             ;; margin.
             (throw 'done 0)
           ;; Otherwise, skip over any nested blocks.
           (setq token (mpascal-block-start token))))

        ;; Special case: if we encounter a ", word;" then we assume that we
        ;; are in some kind of uses clause, and thus indent to column 0. This
        ;; works because no other constructs are known to have that form.
        ;; This fixes the irritating case of having indents after a uses
        ;; clause look like:
        ;;   uses
        ;;      someUnit,
        ;;      someOtherUnit;
        ;;      // this should be at column 0!
        ((guard
          (mpascal-is-use-clause-end token last-token last-colon from-kind))
         (throw 'done 0))

        ;; A previous terminator means we can stop. If we are on a directive,
        ;; however, then we are not actually encountering a new statement.
        ((and (guard last-token)
              (mpascal--in mpascal-previous-terminators)
              (guard (not (memq (mpascal-token-kind last-token)
                                mpascal-directives))))
         (throw 'done (mpascal-stmt-line-indent-of last-token 0)))

        ;; Remember any "of" we encounter, since that affects how we
        ;; indent to a case statement within a record declaration
        ;; (i.e. a variant part).
        ('of
         (setq last-of token))

        ;; Remember any ':' we encounter (until we reach an "of"),
        ;; since that affects how we indent to case statements in
        ;; general.
        ('colon
         (unless last-of (setq last-colon token)))

        ;; A case statement delimits a previous statement. We indent labels
        ;; specially.
        ('case
         (throw 'done
                (if last-colon (mpascal-line-indent-of last-colon)
                  (mpascal-line-indent-of token mpascal-case-label-indent))))

        ;; If we are in a use clause then commas mark an enclosing rather than
        ;; a previous statement.
        ((mpascal--in mpascal-use-clauses)
         (throw 'done
                (if (eq 'comma from-kind)
                    (if last-token
                        ;; Indent to first unit in use clause.
                        (mpascal-indent-of last-token)
                      ;; Indent from use clause keyword.
                      (mpascal-line-indent-of token mpascal-indent-level))
                  ;; Indent to use clause keyword.
                  (mpascal-line-indent-of token))))

        ;; Assembly sections always indent in from the asm keyword.
        ('asm
         (throw 'done (mpascal-stmt-line-indent-of token mpascal-indent-level)))

        ;; An enclosing statement delimits a previous statement.
        ;; We try to use the existing indent of the previous statement,
        ;; otherwise we calculate from the enclosing statement.
        ((mpascal--in mpascal-previous-enclosing-statements)
         (throw 'done (if last-token
                          ;; Otherwise indent to the last token
                          (mpascal-line-indent-of last-token)
                        ;; Just indent from the enclosing keyword
                        (mpascal-line-indent-of token mpascal-indent-level))))

        ;; A class or record declaration also delimits a previous statement.
        ((guard (mpascal-composite-type-start token last-token))
         (throw
          'done
          (if (mpascal-is-simple-class-type last-token from-token)
              ;; c = class; or c = class of T; are previous statements.
              (mpascal-line-indent-of token)
            ;; Otherwise c = class ... or r = record ... are enclosing
            ;; statements.
            (mpascal-line-indent-of last-token mpascal-indent-level))))

        ;; We have a definite previous statement delimiter.
        ((mpascal--in mpascal-previous-statements)
         (throw 'done (mpascal-stmt-line-indent-of token 0)))
        )
      ;; We ran out of tokens. Indent to column 0.
      0)))

(defun mpascal-section-indent-of (section-token)
  ;; Returns the indentation appropriate for begin/var/const/type/label
  ;; tokens.
  (let* ((token section-token)
         (last-token nil)
         (nested-block-count 0)
         (expr-delimited nil)
         (last-terminator nil))
    (catch 'done
      (mpascal--scan-non-whitespace-backward token last-token
        ;; Always stop at unmatched ( or [.
        ('open-group
         (throw 'done (mpascal-open-group-indent token last-token)))

        ;; Skip over any ()/[] groups.
        ('close-group (setq token (mpascal-group-start token)))

        ((mpascal--in mpascal-end-block-statements)
         (if (eq 'newline (mpascal-token-kind (mpascal-previous-token token)))
             ;; We can stop at an end token that is right up against the
             ;; margin.
             (throw 'done 0)
           ;; Otherwise, skip over any nested blocks.
           (setq token (mpascal-block-start token)
                 nested-block-count (1+ nested-block-count))))

        ;; Remember if we have encountered any forward routine declarations.
        ('forward
         (setq nested-block-count (1+ nested-block-count)))

        ;; Mark the completion of a nested routine traversal.
        ((and (mpascal--in mpascal-routine-statements)
              (guard (> nested-block-count 0)))
         (setq nested-block-count (1- nested-block-count)))

        ;; Remember if we have encountered any statement terminators.
        ('semicolon (setq last-terminator token))

        ;; Remember if we have encountered any expression delimiters.
        ((mpascal--in mpascal-expr-delimiters)
         (setq expr-delimited token))

        ;; Enclosing body statements are delimiting. We indent the compound
        ;; bodies specially.
        ((and (guard (not last-terminator))
              (mpascal--in mpascal-body-statements))
         (throw 'done
                (mpascal-stmt-line-indent-of token
                                             mpascal-compound-block-indent)))

        ;; An enclosing ":" means a label.
        ((and 'colon
              (guard (and (memq (mpascal-token-kind section-token)
                                mpascal-block-statements)
                          (not last-terminator)
                          (not expr-delimited)
                          (not (eq 'equals
                                   (mpascal-token-kind last-token))))))
         (throw 'done
                (mpascal-stmt-line-indent-of token mpascal-indent-level)))

        ;; Block and mid block tokens are always enclosing
        ((mpascal--in mpascal-begin-enclosing-tokens)
         (throw 'done
                (mpascal-stmt-line-indent-of token mpascal-indent-level)))

        ;; Declaration sections and routines are delimiters, unless they
        ;; are part of a nested routine.
        ((and (mpascal--in mpascal-decl-delimiters)
              (guard (= 0 nested-block-count)))
         (throw 'done (mpascal-line-indent-of token 0)))

        ;; Unit statements mean we indent right to the left.
        ((mpascal--in mpascal-unit-statements) (throw 'done 0))
        )
      ;; We ran out of tokens. Indent to column 0.
      0)))

(defun mpascal-enclosing-indent-of (from-token)
  ;; Returns the indentation offset from the enclosing statement of the token.
  (let ((token from-token)
        (from-kind (mpascal-token-kind from-token))
        (stmt-start nil)
        (last-token nil)
        (equals-encountered nil)
        (before-equals nil)
        (expr-delimited nil))
    (catch 'done
      (mpascal--scan-non-whitespace-backward token last-token
        ;; An open ( or [ always is an indent point.
        ('open-group
         (throw 'done
                (mpascal-open-group-indent
                 token last-token
                 (if (memq from-kind mpascal-binary-ops)
                     ;; Keep binary operations aligned with the open group.
                     0
                   mpascal-indent-level))))

        ;; Skip over any ()/[] groups.
        ('close-group (setq token (mpascal-group-start token)))

        ;; Skip over any nested blocks.
        ((mpascal--in mpascal-end-block-statements)
         (setq token (mpascal-block-start token)))

        ;; An expression delimiter affects indentation depending on whether
        ;; the point is before or after it. Remember that we encountered one.
        ;; Also remember the last encountered token, since if it exists it
        ;; should be the actual indent point.
        ((mpascal--in mpascal-expr-delimiters)
         (setq expr-delimited token stmt-start last-token))

        ;; With a non-delimited expression statement we indent after the
        ;; statement's keyword, unless we are on the delimiter itself.
        ((and (guard (not expr-delimited))
              (mpascal--in mpascal-expr-statements))
         (throw 'done
                (cond
                 ((memq from-kind mpascal-expr-delimiters)
                  ;; We are indenting a delimiter. Indent to the statement.
                  (mpascal-stmt-line-indent-of token 0))

                 ((and last-token (memq from-kind mpascal-binary-ops))
                  ;; Align binary ops with the expression.
                  (mpascal-indent-of last-token))

                 (last-token
                  ;; Indent in from the expression.
                  (mpascal-indent-of last-token mpascal-indent-level))

                 ;; Indent in from the statement's keyword.
                 ((mpascal-indent-of token mpascal-indent-level)))))

        ;; A delimited case statement indents the label according to
        ;; a special rule.
        ('case
         (throw 'done
                (if stmt-start
                    ;; We are not actually indenting to the case statement,
                    ;; but are within a label expression.
                    (mpascal-stmt-line-indent-of
                     stmt-start mpascal-indent-level)
                  ;; Indent from the case keyword.
                  (mpascal-stmt-line-indent-of
                   token mpascal-case-label-indent))))

        ;; Body expression statements are enclosing. Indent from the
        ;; statement's keyword, unless we have a non-block statement following
        ;; it.
        ((mpascal--in mpascal-body-expr-statements)
         (throw 'done (mpascal-stmt-line-indent-of
                       (or stmt-start token) mpascal-indent-level)))

        ;; An else statement is enclosing, but it doesn't have an expression.
        ;; Thus we take into account last-token instead of stmt-start.
        ('else
         (throw 'done (mpascal-stmt-line-indent-of
                       (or last-token token) mpascal-indent-level)))

        ;; We indent relative to an enclosing declaration section,
        ;; unless this is within the a delimited expression
        ;; (bug#36348).
        ((and (guard (not expr-delimited))
              (mpascal--in mpascal-decl-sections))
         (throw 'done (mpascal-indent-of (if last-token last-token token)
                                         mpascal-indent-level)))

        ;; In unit sections we indent right to the left.
        ;; Handle specially the case of "interface", which can be used
        ;; to start either a unit section or an interface definition.
        ('interface ;FIXME: Generalize to all `mpascal-interface-types'?
         (throw 'done
                (let (token-kind)
                  ;; Find the previous non-whitespace token.
                  (while (progn
                           (setq last-token token
                                 token (mpascal-previous-token token)
                                 token-kind (mpascal-token-kind token))
                           (and token
                                (memq token-kind
                                      mpascal-whitespace))))
                  ;; If this token is an equals sign, "interface" is being
                  ;; used to start an interface definition and we should
                  ;; treat it as a composite type; otherwise, we should
                  ;; consider it the start of a unit section.
                  (if (and token (eq token-kind 'equals))
                      (mpascal-line-indent-of last-token
                                              mpascal-indent-level)
                    0))))

        ;; In unit sections we indent right to the left.
        ((mpascal--in mpascal-unit-sections)
         ;; Note: The `interface' case is handled specially above.
         (throw 'done 0))

        ;; A previous terminator means we can stop.
        ((and (mpascal--in mpascal-previous-terminators) token-kind)
         (throw 'done
                (cond ((and last-token
                            (eq 'comma token-kind)
                            (memq from-kind mpascal-binary-ops))
                       ;; Align binary ops with the expression.
                       (mpascal-indent-of last-token))

                      (last-token
                       ;; Indent in from the expression.
                       (mpascal-indent-of last-token mpascal-indent-level))

                      ;; No enclosing expression; use the previous statement's
                      ;; indent.
                      ((mpascal-previous-indent-of token)))))

        ;; A block statement after an expression delimiter has its start
        ;; column as the expression statement. E.g.
        ;;    if (a = b)
        ;;       and (a != c) then begin
        ;;       //...
        ;;    end;
        ;; Remember it for when we encounter the expression statement start.
        ((guard (mpascal-is-block-after-expr-statement token))
         (throw 'done
                (cond (last-token
                       (mpascal-indent-of last-token mpascal-indent-level))

                      (t (+ (mpascal-section-indent-of token)
                            mpascal-indent-level)))))

        ;; Assembly sections always indent in from the asm keyword.
        ('asm
         (throw 'done (mpascal-stmt-line-indent-of token mpascal-indent-level)))

        ;; Stop at an enclosing statement and indent from it.
        ((mpascal--in mpascal-enclosing-statements)
         (throw 'done (mpascal-stmt-line-indent-of
                       (or last-token token) mpascal-indent-level)))

        ;; A class/record declaration is also enclosing.
        ((guard (mpascal-composite-type-start token last-token))
         (throw 'done
                (mpascal-line-indent-of last-token mpascal-indent-level)))

        ;; A ":" we indent relative to its line beginning.  If we are in a
        ;; parameter list, then stop also if we hit a ";".
        ((and 'colon
              (guard (not (or expr-delimited
                              (memq from-kind mpascal-expr-delimiters)
                              equals-encountered
                              (eq from-kind 'equals)))))
         (throw 'done
                (if last-token
                    (mpascal-indent-of last-token mpascal-indent-level)
                  (mpascal-line-indent-of token mpascal-indent-level
                                          'semicolon))))

        ;; If the ":" was not processed above and we have token after the "=",
        ;; then indent from the "=". Ignore :=, however.
        ((and 'colon (guard (and equals-encountered before-equals)))
         (cond
          ;; Ignore binary ops for now. It would do, for example:
          ;;   val := 1 + 2
          ;;          + 3;
          ;; which is good, but also
          ;;   val := Foo
          ;;      (foo, args)
          ;;          + 2;
          ;; which doesn't look right.

          ;; ;; Align binary ops with the before token.
          ;;((memq from-kind mpascal-binary-ops)
          ;;(throw 'done (mpascal-indent-of before-equals 0)))

          ;; Assignments (:=) we skip over to get a normal indent.
          ((eq (mpascal-token-kind last-token) 'equals))

          ;; Otherwise indent in from the equals.
          (t (throw 'done
                    (mpascal-indent-of before-equals mpascal-indent-level)))))

        ;; Remember any "=" we encounter if it has not already been processed.
        ('equals
         (setq equals-encountered token
               before-equals last-token))
        )
      ;; We ran out of tokens. Indent to column 0.
      0)))

(defun mpascal-corrected-indentation ()
  ;; Returns the corrected indentation for the current line.
  (mpascal-save-excursion
    (mpascal-progress-start)
    ;; The caller should make sure we're at the first token on the line.
    (cl-assert (eql (point)
                    (save-excursion
                      (beginning-of-line)
                      (skip-chars-forward mpascal-space-chars)
                      (point))))
    (let* ((token (mpascal-current-token))
           (token-kind (mpascal-token-kind token))
           (indent
            (cond ((eq 'close-group token-kind)
                   ;; Indent to the matching start ( or [.
                   (mpascal-indent-of (mpascal-group-start token)))

                  ((memq token-kind mpascal-unit-statements) 0)

                  ((memq token-kind mpascal-comments)
                   ;; In a comment.
                   (mpascal-comment-indent-of token))

                  ((memq token-kind mpascal-decl-matchers)
                   ;; Use a previous section/routine's indent.
                   (mpascal-section-indent-of token))

                  ((memq token-kind mpascal-match-block-statements)
                   ;; Use the block's indentation.
                   (let ((block-start
                          (mpascal-block-start token 'stop-on-class)))
                     (cond
                      ;; When trailing a body statement, indent to
                      ;; the statement's keyword.
                      ((mpascal-is-block-after-expr-statement block-start)
                       (mpascal-section-indent-of block-start))

                      ;; Otherwise just indent to the block start.
                      ((mpascal-stmt-line-indent-of block-start 0)))))

                  ((eq 'else token-kind)
                   ;; Find the start of the if or case statement.
                   (mpascal-stmt-line-indent-of (mpascal-else-start token) 0))

                  ;; Otherwise indent in from enclosing statement.
                  (t
                   (mpascal-enclosing-indent-of
                    (or token (mpascal-token-at (1- (point)))))))))
      (mpascal-progress-done)
      indent)))

(defun mpascal-indent-line ()
  "Indent the current line according to the current language construct.
If before the indent, the point is moved to the indent."
  (interactive)
  (save-match-data
    (let ((marked-point (point-marker))) ; Maintain our position reliably.
      (beginning-of-line)
      (skip-chars-forward mpascal-space-chars)
      (let ((new-indent (mpascal-corrected-indentation)))
        (if (< marked-point (point))
            ;; If before the indent column, then move to it.
            (set-marker marked-point (point)))
        ;; Advance our marked point after inserted spaces.
        (set-marker-insertion-type marked-point t)
        (indent-line-to new-indent)
        (goto-char marked-point)
        (set-marker marked-point nil)))))

(defvar mpascal-mode-abbrev-table nil
  "Abbrev table in use in Mpascal mode buffers.")
(define-abbrev-table 'mpascal-mode-abbrev-table ())

(defmacro mpascal-ensure-buffer (buffer-var buffer-name)
  ;; Ensures there exists a buffer of the specified name in the specified
  ;; variable.
  `(when (not (buffer-live-p ,buffer-var))
     (setq ,buffer-var (get-buffer-create ,buffer-name))))

(defun mpascal-log-msg (to-buffer the-msg)
  ;; Writes a message to the end of the specified buffer.
  (with-current-buffer to-buffer
    (save-selected-window
      (switch-to-buffer-other-window to-buffer)
      (goto-char (point-max))
      (set-window-point (get-buffer-window to-buffer) (point))
      (insert the-msg))))

;; Debugging helpers:

(defvar mpascal-debug-buffer nil
  "Buffer to write Mpascal mode debug messages to.  Created on demand.")

(defun mpascal-debug-log (format-string &rest args)
  ;; Writes a message to the log buffer.
  (when mpascal-debug
    (mpascal-ensure-buffer mpascal-debug-buffer "*Mpascal Debug Log*")
    (mpascal-log-msg mpascal-debug-buffer
                    (concat (format-time-string "%H:%M:%S ")
                            (apply #'format (cons format-string args))
                            "\n"))))

(defun mpascal-debug-token-string (token)
  (let* ((image (mpascal-token-string token))
         (has-newline (string-match "^\\([^\n]*\\)\n\\(.+\\)?$" image)))
    (when has-newline
       (setq image (concat (match-string 1 image)
                           (if (match-beginning 2) "..."))))
    image))

(defun mpascal-debug-show-current-token ()
  (interactive)
  (let ((token (mpascal-current-token)))
    (mpascal-debug-log "Token: %S %S" token (mpascal-debug-token-string token))))

(defun mpascal-debug-goto-point (p)
  (interactive "NGoto char: ")
  (goto-char p))

(defun mpascal-debug-goto-next-token ()
  (interactive)
  (goto-char (mpascal-token-start (mpascal-next-token (mpascal-current-token)))))

(defun mpascal-debug-goto-previous-token ()
  (interactive)
  (goto-char
   (mpascal-token-start (mpascal-previous-token (mpascal-current-token)))))

(defun mpascal-debug-show-current-string (from to)
  (interactive "r")
  (mpascal-debug-log "String: %S" (buffer-substring from to)))

(defun mpascal-debug-tokenize-region (from to)
  (interactive)
  (mpascal-save-excursion
   (mpascal-progress-start)
   (goto-char from)
   (while (< (point) to)
     (goto-char (mpascal-token-end (mpascal-current-token)))
     (mpascal-step-progress (point) "Tokenizing" mpascal-scanning-progress-step))
   (mpascal-progress-done "Tokenizing done")))

(defun mpascal-debug-tokenize-buffer ()
  (interactive)
  (mpascal-debug-tokenize-region (point-min) (point-max)))

(defun mpascal-debug-tokenize-window ()
  (interactive)
  (mpascal-debug-tokenize-region (window-start) (window-end)))


(defun mpascal-tab ()
  "Indent the region, if Transient Mark mode is on and the region is active.
Otherwise, indent the current line or insert a TAB, depending on the
value of `mpascal-tab-always-indents' and the current line position."
  (interactive)
  (cond ((use-region-p)
         ;; If Transient Mark mode is enabled and the region is active, indent
         ;; the entire region.
         (indent-region (region-beginning) (region-end)))
        ((or mpascal-tab-always-indents
             (save-excursion (skip-chars-backward mpascal-space-chars) (bolp)))
         ;; Otherwise, if we are configured always to indent (regardless of the
         ;; point's position in the line) or we are before the first non-space
         ;; character on the line, indent the line.
         (mpascal-indent-line))
        (t
         ;; Otherwise, insert a tab character.
         (insert "\t"))))

(make-obsolete 'mpascal-tab 'indent-for-tab-command "24.4")

(defun mpascal-is-directory (path)
  ;; True if the specified path is an existing directory.
  (let ((attributes (file-attributes path)))
    (and attributes (car attributes))))

(defun mpascal-is-file (path)
  ;; True if the specified file exists as a file.
  (let ((attributes (file-attributes path)))
    (and attributes (null (car attributes)))))

(defun mpascal-search-directory (unit dir &optional recurse)
  ;; Searches for the unit in the specified directory. If recurse is true, then
  ;; the directory is recursively searched. File name comparison is done in a
  ;; case insensitive manner.
  (when (mpascal-is-directory dir)
    (let ((files (directory-files dir))
          (unit-file (downcase unit)))
      (catch 'done
        ;; Search for the file.
        (dolist (file files)
          (let ((path (concat dir "/" file)))
            (if (and (string= unit-file (downcase file))
                     (mpascal-is-file path))
                (throw 'done path))))

        ;; Not found. Search subdirectories.
        (when recurse
          (dolist (subdir files)
            (unless (member subdir '("." ".."))
              (let ((path (mpascal-search-directory
                           unit (concat dir "/" subdir) recurse)))
                (if path (throw 'done path))))))

        ;; Not found.
        nil))))


(defun mpascal-find-unit-in-directory (unit dir)
  ;; Searches for the unit in the specified directory. If the directory ends
  ;; in \"...\", then it is recursively searched.
  (let ((dir-name dir)
        (recurse nil))
    ;; Check if we need to recursively search the directory.
    (if (string-match "^\\(.+\\)\\.\\.\\.$" dir-name)
        (setq dir-name (match-string 1 dir-name)
              recurse t))
    ;; Ensure the trailing slash is removed.
    (if (string-match "^\\(.+\\)[\\/]$" dir-name)
        (setq dir-name (match-string 1 dir-name)))
    (mpascal-search-directory unit dir-name recurse)))

(defun mpascal-find-unit-file (unit)
  ;; Finds the specified mpascal source file according to `mpascal-search-path'.
  ;; If found, the full path is returned, otherwise nil is returned.
  (catch 'done
    (cond ((null mpascal-search-path)
           (mpascal-find-unit-in-directory unit "."))

          ((stringp mpascal-search-path)
           (mpascal-find-unit-in-directory unit mpascal-search-path))

          ((dolist (dir mpascal-search-path)
             (let ((file (mpascal-find-unit-in-directory unit dir)))
               (if file (throw 'done file))))))
    nil))

(defun mpascal-find-unit (unit)
  "Find the specified Mpascal source file according to `mpascal-search-path'.
If no extension is specified, .pas is assumed.  Creates a buffer for the unit."
  (interactive "sMpascal unit name: ")
  (let* ((unit-file (if (string-match "^\\(.*\\)\\.[a-z]+$" unit)
                        unit
                      (concat unit ".pas")))
         (file (mpascal-find-unit-file unit-file)))
    (if (null file)
        (error "Unit not found: %s" unit-file)
      (find-file file)
      (if (not (derived-mode-p 'mpascal-mode))
          (mpascal-mode)))
    file))

(defun mpascal-find-current-def ()
  "Find the definition of the identifier under the current point."
  (interactive)
  (error "mpascal-find-current-def: Not implemented yet"))

(defun mpascal-find-current-xdef ()
  "Find the definition of the identifier under the current point, searching
in external units if necessary (as listed in the current unit's use clause).
The set of directories to search for a unit is specified by the global variable
`mpascal-search-path'."
  (interactive)
  (error "mpascal-find-current-xdef: Not implemented yet"))

(defun mpascal-find-current-body ()
  "Find the body of the identifier under the current point, assuming
it is a routine."
  (interactive)
  (error "mpascal-find-current-body: Not implemented yet"))

(defun mpascal-fill-comment ()
  "Fill the text of the current comment, according to `fill-column'.
An error is raised if not in a comment."
  (interactive)
  (save-excursion
    (save-restriction
    (let* ((comment (mpascal-current-token))
           (comment-kind (mpascal-token-kind comment)))
      (if (not (memq comment-kind mpascal-comments))
          (error "Not in a comment")
        (let* ((start-comment (mpascal-comment-block-start comment))
               (end-comment (mpascal-comment-block-end comment))
               ;; FIXME: Don't abuse global variables like `comment-end/start'.
               (comment-start (mpascal-token-start start-comment))
               (comment-end (mpascal-token-end end-comment))
               (content-start (mpascal-comment-content-start start-comment))
               (content-indent (mpascal-column-of content-start))
               (content-prefix (make-string content-indent ?\s))
               (content-prefix-re mpascal-leading-spaces-re)
               (p nil)
               (marked-point (point-marker))) ; Maintain our position reliably.
          (when (eq 'comment-single-line comment-kind)
            ;; // style comments need more work.
            (setq content-prefix
                  (let ((comment-indent (mpascal-column-of comment-start)))
                    (concat (make-string comment-indent ?\s) "//"
                            (make-string (- content-indent comment-indent 2)
                                         ?\s)))
                  content-prefix-re (concat mpascal-leading-spaces-re
                                            "//"
                                            mpascal-spaces-re)
                  comment-end (if (mpascal-is-literal-end comment-end)
                                  ;; Don't include the trailing newline.
                                  (1- comment-end)
                                comment-end)))

          ;; Advance our marked point after inserted spaces.
          (set-marker-insertion-type marked-point t)

          ;; Ensure we can modify the buffer
          (goto-char content-start)
          (insert " ")
          (delete-char -1)

          (narrow-to-region content-start comment-end)

          ;; Strip off the comment prefixes
          (setq p (point-min))
          (while (when (< p (point-max))
                   (goto-char p)
                   (re-search-forward content-prefix-re nil t))
            (replace-match "" nil nil)
            (setq p (1+ (point))))

          ;; add an extra line to prevent the fill from doing it for us.
          (goto-char (point-max))
          (insert "\n")

          ;; Fill the comment contents.
          (let ((fill-column (- fill-column content-indent)))
            (fill-region (point-min) (point-max)))

          (goto-char (point-max))
          (delete-char -1)

          ;; Restore comment prefixes.
          (goto-char (point-min))
          (end-of-line)                 ; Don't reset the first line.
          (setq p (point))
          (while (when (< p (point-max))
                   (goto-char p)
                   (re-search-forward "^" nil t))
            (replace-match content-prefix nil nil)
            (setq p (1+ (point))))

          (setq comment-end (point-max))
          (widen)

          ;; Restore our position
          (goto-char marked-point)
          (set-marker marked-point nil)))))))

(defun mpascal-new-comment-line ()
  "If in a // comment, do a newline, indented such that one is still in the
comment block.  If not in a // comment, just does a normal newline."
  (declare
   (obsolete "use comment-indent-new-line with comment-multi-line instead"
             "27.1"))
  (interactive)
  (let ((comment (mpascal-current-token)))
    (if (not (eq 'comment-single-line (mpascal-token-kind comment)))
        ;; Not in a // comment. Just do the normal newline.
        (newline)
      (let* ((start-comment (mpascal-comment-block-start comment))
             (comment-start (mpascal-token-start start-comment))
             (content-start (mpascal-comment-content-start start-comment))
             (prefix
              (concat (make-string (mpascal-column-of comment-start) ?\s) "//"
                      (make-string (- content-start comment-start 2) ?\s))))
        (delete-horizontal-space)
        (insert "\n" prefix)))))

(defun mpascal-match-token (token limit)
  ;; Sets the match region used by (match-string 0) and friends to the token's
  ;; region.  Sets the current point to the end of the token (or limit).
  (set-match-data nil)
  (if token
      (let ((end (min (mpascal-token-end token) limit)))
        (set-match-data (list (mpascal-token-start token) end))
        (goto-char end)
        token)))

(defconst mpascal-font-lock-keywords
  `(("\\_<\\(function\\|pro\\(cedure\\|gram\\)\\)[ \t]+\\([[:alpha:]][[:alnum:]_]*\\)"
     (1 font-lock-keyword-face) (3 font-lock-function-name-face))
    ,(concat "\\_<" (regexp-opt (mapcar #'symbol-name mpascal-keywords))
             "\\_>")))

(defconst mpascal-font-lock-defaults
  '(mpascal-font-lock-keywords
    nil ; Syntactic fontification does apply.
    t	; Don't care about case since we don't use regexps to find tokens.
    nil ; Syntax alists don't apply.
    nil ; Syntax begin movement doesn't apply.
    )
  "Mpascal mode font-lock defaults.  Syntactic fontification is ignored.")

(defconst mpascal--syntax-propertize
  (syntax-propertize-rules
   ;; The syntax-table settings are too coarse and end up treating /* and (/
   ;; as comment starters.  Fix it here by removing the "2" from the syntax
   ;; of the second char of such sequences.
   ("/\\(\\*\\)" (1 ". 3b"))
   ("(\\(/\\)" (1 (prog1 ". 1c" (forward-char -1) nil)))
   ;; Pascal uses '' and "" rather than \' and \" to escape quotes.
   ("''\\|\"\"" (0 (if (save-excursion
                         (nth 3 (syntax-ppss (match-beginning 0))))
                       (string-to-syntax ".")
                     ;; In case of 3 or more quotes in a row, only advance
                     ;; one quote at a time.
                     (forward-char -1)
                     nil)))))

(defvar mpascal-debug-mode-map
  (let ((kmap (make-sparse-keymap)))
    (dolist (binding '(("n" mpascal-debug-goto-next-token)
                       ("p" mpascal-debug-goto-previous-token)
                       ("t" mpascal-debug-show-current-token)
                       ("T" mpascal-debug-tokenize-buffer)
                       ("W" mpascal-debug-tokenize-window)
                       ("g" mpascal-debug-goto-point)
                       ("s" mpascal-debug-show-current-string)))
      (define-key kmap (car binding) (cadr binding)))
    kmap)
  "Keystrokes for Mpascal mode debug commands.")

(defvar mpascal-mode-map
  (let ((kmap (make-sparse-keymap)))
    (dolist (binding
             (list ;; '("\C-cd" mpascal-find-current-def)
                   ;; '("\C-cx" mpascal-find-current-xdef)
                   ;; '("\C-cb" mpascal-find-current-body)
                   '("\C-cu" mpascal-find-unit)
                   '("\M-q" mpascal-fill-comment)
                   ;; '("\M-j" mpascal-new-comment-line)
                   ;; Debug bindings:
                   (list "\C-c\C-d" mpascal-debug-mode-map)))
      (define-key kmap (car binding) (cadr binding)))
    kmap)
  "Keymap used in Mpascal mode.")

(define-obsolete-variable-alias 'delphi-mode-hook 'mpascal-mode-hook "24.4")
;;;###autoload
(define-obsolete-function-alias 'delphi-mode #'mpascal-mode "24.4")
;;;###autoload
(define-derived-mode mpascal-mode prog-mode "Mpascal"
  "Major mode for editing Mpascal code.
\\<mpascal-mode-map>
\\[mpascal-find-unit]\t- Search for a Mpascal source file.
\\[mpascal-fill-comment]\t- Fill the current comment.
\\[mpascal-new-comment-line]\t- If in a // comment, do a new comment line.

\\[indent-region] also works for indenting a whole region.

Customization:

 `mpascal-indent-level'                (default 2)
    Indentation of Mpascal statements with respect to containing block.
 `mpascal-compound-block-indent'       (default 0)
    Extra indentation for blocks in compound statements.
 `mpascal-case-label-indent'           (default 0)
    Extra indentation for case statement labels.
 `mpascal-search-path'                 (default .)
    Directories to search when finding external units.
 `mpascal-verbose'                     (default nil)
    If true then Mpascal token processing progress is reported to the user.

Coloring:

 `mpascal-keyword-face'                (default `font-lock-keyword-face')
    Face used to color Mpascal keywords."

  ;; Buffer locals:
  (setq-local indent-line-function #'mpascal-indent-line)
  (setq-local comment-indent-function #'mpascal-indent-line)
  (setq-local case-fold-search t)
  (setq-local mpascal-progress-last-reported-point nil)
  (setq-local font-lock-defaults mpascal-font-lock-defaults)
  (setq-local tab-always-indent mpascal-tab-always-indents)
  (setq-local syntax-propertize-function mpascal--syntax-propertize)

  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\(?://\\|(\\*\\|{\\)[ \t]*")
  (setq-local comment-end-skip "[ \t]*\\(?:\n\\|\\*)\\|}\\)"))


(defun mpscal-upcase ()
  "Keyword to uperacse"
  (interactive)
  (progn
    (goto-char (point-min))
    (while (not (eobp))
      (let ((faces-at-point (get-char-property (point) 'face)))
	(when (eq 'font-lock-keyword-face faces-at-point)
          (upcase-word 1))
	(forward-char 1)))))


(provide 'mpascal)
;;; mpascal.el ends here
