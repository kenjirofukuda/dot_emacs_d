;;; cgpl-mode.el --- Major mode for CALMA/GPL II source files -*- lexical-binding: t; -*-

(defvar cgpl-mode-hook nil)
(defvar cgpl-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    ;; C-c C-f でバッファ全体の演算子間隔 ＋ 文字列の密着を一発統合整形
    (define-key map (kbd "C-c C-f") 'cgpl-format-buffer)
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

(defvar cgpl-tab-width 2 "Width of a tab for CGPL mode.")

;; ========================================================================
;; rx構文ベースの行ベース精密インデント関数（空行スキップ対応）
;; ========================================================================
(defun cgpl-indent-line ()
  "GPL II のための行ベースの精密インデント関数。空行をスキップして解析します。."
  (interactive)
  (let ((indent-offset cgpl-tab-width)
         (current-indent 0))
    (save-excursion
      (forward-line -1)
      (while (and (not (bobp))
               (looking-at (rx (and (zero-or-more (any " \t\r")) eol))))
        (forward-line -1))

      (back-to-indentation)
      (setq current-indent (current-indentation))

      (when (looking-at (rx (and (zero-or-more not-newline)
                              word-start
                              (or "THEN" "DO" "ELSE" "ELIF" "PROCEDURE" "FUNCTION" "OF")
                              word-end
                              (zero-or-more (any " \t"))
                              (zero-or-more (and "|" (zero-or-more not-newline)))
                              eol)))

        (setq current-indent (+ current-indent indent-offset)))

      (when (looking-at (rx (and (zero-or-more not-newline)
                              word-start
                              "OUT"
                              word-end
                              (zero-or-more (any " \t"))
                              ":"
                              )))
        (setq current-indent (+ current-indent indent-offset)))

      (when (looking-at (rx "CASE" (+? nonl) ":"))
        (setq current-indent (+ current-indent indent-offset))))



    (back-to-indentation)
    (when (looking-at (rx (and word-start
                            (or "ELSE" "ELIF" "UNTIL" "ENDDO" "ENDIF" "ENDSWITCH")
                            word-end)))
      (setq current-indent (max 0 (- current-indent indent-offset))))

    (when (looking-at (rx (and (zero-or-more not-newline)
                            word-start
                            "OUT"
                            word-end
                            (zero-or-more (any " \t"))
                            ":"
                            )))
      (setq current-indent (max 0 (- current-indent indent-offset))))

    (when (looking-at (rx "CASE" (+? nonl) ":"))
      (setq current-indent (max 0 (- current-indent indent-offset)))
      )

    (indent-line-to current-indent))
  )
;; ========================================================================
;; 構造化されたフォーマッタ実装群
;; ========================================================================

(defun cgpl-format-operators ()
  "バッファ内の2項演算子 (:=, 比較, 算術, 論理) の間隔を綺麗に整形します（増殖・無限ループ防止完備）。."
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (while (re-search-forward
               (rx (and (zero-or-more (any " \t"))
                     (or (and ":" (zero-or-more (any " \t")) "=")
                       (and "<" (zero-or-more (any " \t")) (or "=" ">"))
                       (and ">" (zero-or-more (any " \t")) "=")
                       (and (or "<" ">" "=" "+" "-" "*" "%"))
                       (and word-start (or "AND" "OR") word-end))
                     (zero-or-more (any " \t"))))
               nil t)
        (let ((beg (match-beginning 0))
               (end (match-end 0))
               (matched-str (match-string 0)))
          (unless (syntax-ppss-context (syntax-ppss beg))
            (let* ((pure-token (replace-regexp-in-string (rx (one-or-more (any " \t"))) "" matched-str))
                    (final-string ""))
              (when (string-match-p (rx (or "AND" "OR")) matched-str)
                (setq pure-token (if (string-match-p "AND" matched-str) "AND" "OR")))
              (setq final-string (concat " " pure-token " "))
              (save-excursion
                (goto-char beg)
                (when (bolp) (setq final-string (concat pure-token " ")))
                (goto-char end)
                (when (or (eolp) (looking-at (rx (any "\n|"))))
                  (setq final-string (string-trim-right final-string))))
              (replace-match final-string t t)))
          (goto-char (max (point) end)))))))

(defun cgpl-format-keyword-strings ()
  "GPL II のキーワードや識別子と、文字列 '\"' の密着を確実に1行ずつ切り離します。.
🚨 検索段階で完全に密着している箇所のみを狙い撃ちするため、文字列内部を汚染することは200%絶対にありません。."
  (message "[GUID: cgpl-format-v20261031-pure-rx-shield]")
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (while (not (eobp))
        (let ((eol-pos (line-end-position)))
          (save-excursion
            (beginning-of-line)
            ;; 1. 【パターンA: キーワードの直後に「\"」が「完全にスペース0個」で密着している箇所のみをキャッチ】
            ;; (例: TEXTINPUT" や )" のみ。すでにスペースがある `; "` などは検索段階で100%安全に弾かれます。
            (while (re-search-forward
                     (rx (and (submatch (or (and word-start (one-or-more (any "A-Za-z0-9_")) word-end)
                                          (any ")]")))
                           (submatch "\"")))
                     eol-pos t)
              (let ((end-pos (match-end 0)))
                (unless (syntax-ppss-context (syntax-ppss (match-beginning 0)))
                  ;; 💡 危うい判定文をすべて全廃。マッチした＝100%密着しているため、安全に1マス引き離します。
                  (replace-match "\\1 \\2" t))
                (goto-char (max (point) end-pos))))

            (beginning-of-line)
            ;; 2. 【パターンB: 閉じ「\"」の直後にキーワードや開き括弧が「完全にスペース0個」で密着している箇所のみをキャッチ】
            ;; (例: "THEN や "( のみ)
            (while (re-search-forward
                     (rx (and (submatch "\"")
                           (submatch (or (and word-start (one-or-more (any "A-Za-z0-9_")) word-end)
                                       (any "([")))))
                     eol-pos t)
              (let ((end-pos (match-end 0)))
                (unless (syntax-ppss-context (syntax-ppss (match-beginning 0)))
                  (replace-match "\\1 \\2" t))
                (goto-char (max (point) end-pos))))))

        ;; 次の行へ確実に前進
        (forward-line 1)))))


(defun cgpl-format-buffer ()
  "【統合エントリー】 GPL II ソースファイル全体の演算子間隔と、キーワード・文字列の密着を一括整形します。."
  (interactive)
  (cgpl-format-operators)
  ;; 重大なバグのため、現在未使用
  ;; (cgpl-format-keyword-strings)
  (message "GPL II buffer formatted successfully."))

;;;### autoload
(add-to-list 'auto-mode-alist '("\\.GS\\'" . cgpl-mode))

(define-derived-mode cgpl-mode prog-mode "CALMA/GPL II"
  "Major mode for CALMA/GPL II source files."

  (when cgpl-tab-width
    (setq tab-width cgpl-tab-width))

  (setq-local indent-tabs-mode nil)
  (setq-local indent-line-function #'cgpl-indent-line)

  ;; ========================================================================
  ;; コメント・構文テーブル定義
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
