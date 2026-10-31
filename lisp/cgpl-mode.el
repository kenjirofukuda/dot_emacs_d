;;; cgpl-mode.el --- Major mode for CALMA/GPL II source files -*- lexical-binding: t; -*-

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

(defvar cgpl-tab-width 2 "Width of a tab for CGPL mode")

;; ========================================================================
;; 🚨 【完全解決】右流れを起こすSMIEを全廃し、rx構文ベースのインデントを実装
;; ========================================================================
(defun cgpl-indent-line ()
  "GPL II のための行ベースの精密インデント関数。"
  (interactive)
  (let ((indent-offset cgpl-tab-width)
        (current-indent 0))
    (save-excursion
      ;; 1. 1行上の行頭へ移動してベースのインデント量を取得
      (forward-line -1)
      (back-to-indentation)
      (setq current-indent (current-indentation))

      ;; 2. 直前の行の「末尾（コメントの手前）」にブロック開始キーワードがあるかrx構文で判定
      ;; 行末のインラインコメント「| ...」や空白を安全にスキップして判定します
      (when (looking-at (rx (and (zero-or-more not-newline)
                                 word-start
                                 (or "THEN" "DO" "ELSE" "ELIF" "PROCEDURE" "FUNCTION" "OF" ":")
                                 word-end
                                 (zero-or-more (any " \t"))
                                 (zero-or-more (and "|" (zero-or-more not-newline)))
                                 eol)))
        ;; マッチしたらインデントを2マス深くする
        (setq current-indent (+ current-indent indent-offset))))

    ;; 3. 現在の行のキーワード（閉じタグ）自体を入力した時の、左への巻き戻し判定
    (back-to-indentation)
    (when (looking-at (rx (and word-start
                               (or "ENDSUB" "ELSE" "ELIF" "UNTIL" "ENDDO" "ENDIF" "ENDSWITCH" "CASE" "OUT")
                               word-end)))
      ;; 閉じタグ自体は2マス左に戻す（ただし0未満にはしない）
      (setq current-indent (max 0 (- current-indent indent-offset))))

    ;; 現在の行に確定したインデント量を適用
    (indent-line-to current-indent)))

;;;### autoload
(add-to-list 'auto-mode-alist '("\\.GS\\'" . cgpl-mode))

(define-derived-mode cgpl-mode prog-mode "CALMA/GPL II"
  "Major mode for CALMA/GPL II source files"

  (when cgpl-tab-width
    (setq tab-width cgpl-tab-width))

  ;; 🚨 SMIEを完全にパージし、独自のインデント関数をセット
  (setq-local indent-line-function #'cgpl-indent-line)

  ;; ========================================================================
  ;; コメント・構文テーブル定義（カラーライズを100%維持）
  ;; ========================================================================
  (modify-syntax-entry ?| "< b" cgpl-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" cgpl-mode-syntax-table)
  (modify-syntax-entry ?` "<" cgpl-mode-syntax-table)
  (modify-syntax-entry ?' ">" cgpl-mode-syntax-table)

  (setq-local font-lock-defaults cgpl-font-lock-defaults)

  (setq-local comment-start "| ")
  (setq-local comment-end "")

  (setq-local comment-start-skip
              (rx (and (or "|" "`") (zero-or-more (any " \t")))))
  (setq-local comment-end-skip
              (rx (and (zero-or-more (any " \t")) (or "\n" "'"))))
  )

(provide 'cgpl-mode)
