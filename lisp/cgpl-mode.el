;; CALMA/GPL II mode
;; (require 'cgpl-mode "/home/kenjiro/.emacs.d/lisp/cgpl-mode.el")

(defvar cgpl-mode-hook nil)
(defvar cgpl-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for CALMA/GPL II major mode")

(defconst cgpl-comments
  '(comment-single-line)
  "Tokens that represent comments.")

(defconst cgpl-keywords
  (append
   '(NILADIC MONADIC DYADIC)
   '(LOCAL GLOBAL EXTERNAL)
   '(LOGICAL CHAR INTEGER REAL)
   '(PROCEDURE FUNCTION ENDSUB)
   '(AND OR NOT)
   '(GOTO WHILE UNTIL DO ENDDO IF THEN ELSE ELIF ENDIF SWITCH OF CASE OUT ENDSWITCH))
  "GPL II directives.")

(defconst cgpl-font-lock-keywords
  (list (concat "\\_<" (regexp-opt (mapcar #'symbol-name cgpl-keywords))
		"\\_>")))

(defconst cgpl-font-lock-defaults
  '(cgpl-font-lock-keywords
    nil ; Syntactic fontification does apply.
    nil ; Don't care about case since we don't use regexps to find tokens.
    nil ; Syntax alists don't apply.
    nil ; Syntax begin movement doesn't apply.
    ))
;; I'd probably put in a default that you want, as opposed to nil
(defvar cgpl-tab-width 2 "Width of a tab for CGPL mode")

;;;### autoload
(add-to-list 'auto-mode-alist '("\\.GS\\'" . cgpl-mode))

(define-derived-mode cgpl-mode prog-mode "CALMA/GPL II"
  "Major mode for CALMA/GPL II source files"
  ;; (interactive)
  ;; (kill-all-local-variables)
  ;; ;; (set-syntax-table cgpl-mode-syntax-table)
  ;; (use-local-map cgpl-mode-map)
  ;; (setq major-mode 'cgpl-mode)
  ;; (setq mode-name )
  ;; (run-hooks 'cgpl-mode-hook)

  ;; when there's an override, use it
  ;; otherwise it gets the default value
  (when cgpl-tab-width
    (setq tab-width cgpl-tab-width))
  
  ;; for comments
  ;; overriding these vars gets you what (I think) you want
  ;; they're made buffer local when you set them
  (setq comment-start "#")
  (setq comment-end "")
  
  (modify-syntax-entry ?| "< b" cgpl-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" cgpl-mode-syntax-table)
  (modify-syntax-entry ?` "<" cgpl-mode-syntax-table)
  (modify-syntax-entry ?' ">" cgpl-mode-syntax-table)
  ;; Buffer locals:
  
  (setq-local font-lock-defaults cgpl-font-lock-defaults)
  
  (setq-local comment-start "| ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(?://\\|(\\*\\|{\\)[ \t]*")
  (setq-local comment-end-skip "[ \t]*\\(?:\n\\|\\*)\\|}\\)")  
  )

(provide 'cgpl-mode)

