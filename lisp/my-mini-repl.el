(defvar my-mini-repl-prompt "REPL> "
  "REPL のプロンプト文字列。")

(defvar my-mini-repl-history nil
  "REPL の入力履歴。")

(defvar-local my-mini-repl-history-index -1
  "現在の履歴インデックス。")

(defvar my-mini-repl-map
  (let ((map (make-sparse-keymap)))
    ;; 完全に純粋なキーマップとして定義（comint の依存を排除）
    (define-key map (kbd "RET") 'my-mini-repl-confirm)
    (define-key map (kbd "M-p") 'my-mini-repl-prev)
    (define-key map (kbd "M-n") 'my-mini-repl-next)
    (define-key map (kbd "C-d") 'my-mini-repl-exit-or-delete)
    map)
  "My Mini REPL のキーマップ")

(define-derived-mode my-mini-repl-mode fundamental-mode "Mini-REPL"
  "ielm と全く同じ操作感を持つ、プロセスレスの完全制御型 REPL。"
  (use-local-map my-mini-repl-map)
  (setq-local buffer-read-only nil))

(defun my-mini-repl-find-prompt-start ()
  "最終プロンプトの開始位置を返す。"
  (save-excursion
    (goto-char (point-max))
    (forward-line 0)
    (if (looking-at (concat "^" (regexp-quote my-mini-repl-prompt)))
        (point)
      (point-at-bol))))

(defun my-mini-repl-find-input-start ()
  "最終プロンプトの直後（ユーザーの入力開始位置）を返す。"
  (save-excursion
    (goto-char (point-max))
    (forward-line 0)
    (if (looking-at (concat "^" (regexp-quote my-mini-repl-prompt)))
        (match-end 0)
      (point-max))))

(defun my-mini-repl-confirm ()
  "Enterキーの処理。過去の行なら最下部にコピー、最下部ならエコーを実行。"
  (interactive)
  (let ((input-start (my-mini-repl-find-input-start)))
    (if (< (point) (my-mini-repl-find-prompt-start))
        ;; 過去の行で Enter を押したら、その行の文字列を最下部にコピー
        (let ((current-line-str (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
          (when (string-match (concat "^" (regexp-quote my-mini-repl-prompt)) current-line-str)
            (setq current-line-str (substring current-line-str (match-end 0))))
          (goto-char (point-max))
          (let ((inhibit-read-only t))
            (delete-region input-start (point-max))
            (insert current-line-str)))

      ;; 最下部の入力欄にいる場合は、エコー処理を実行
      (let ((input-str (buffer-substring-no-properties input-start (point-max))))
        (unless (string-empty-p (string-trim input-str))
          (add-to-history 'my-mini-repl-history input-str))

        (let ((inhibit-read-only t))
          ;; 1. 過去ログの保護：これまでのバッファ全体を完全に読み取り専用にロック
          (put-text-property (point-min) (point-max) 'read-only t)

          ;; 2. エコーバックを出力
          (goto-char (point-max))
          (insert "\n=> " input-str)

          ;; 3. 新しいプロンプトを出力
          (insert "\n" my-mini-repl-prompt)

          ;; 4. プロンプト自体を保護しつつ、その直後の入力欄へ属性が伝染するのを防ぐ (rear-nonsticky)
          (let ((p-start (- (point) (length my-mini-repl-prompt)))
                (p-end (point)))
            (put-text-property p-start p-end 'read-only t)
            (put-text-property p-start p-end 'rear-nonsticky '(read-only))))

        (goto-char (point-max))
        (setq my-mini-repl-history-index -1)))))

(defun my-mini-repl-exit-or-delete (arg)
  "C-d の処理。未入力なら即座に終了、文字があれば1文字削除。"
  (interactive "p")
  (let ((input-start (my-mini-repl-find-input-start)))
    (if (and (eobp) (= (point) input-start))
        (progn
          (message "REPL を終了しました。")
          (kill-buffer (current-buffer))
          (unless (one-window-p)
            (delete-window)))
      (delete-char arg))))

(defun my-mini-repl-prev ()
  "M-p: 一つ前の履歴を呼び出す。"
  (interactive)
  (if (< (point) (my-mini-repl-find-prompt-start))
      (message "Text is read-only")
    (when my-mini-repl-history
      (setq my-mini-repl-history-index (1+ my-mini-repl-history-index))
      (when (>= my-mini-repl-history-index (length my-mini-repl-history))
        (setq my-mini-repl-history-index (1- (length my-mini-repl-history))))
      (let ((inhibit-read-only t)
            (input-start (my-mini-repl-find-input-start)))
        (delete-region input-start (point-max))
        (insert (nth my-mini-repl-history-index my-mini-repl-history))))))

(defun my-mini-repl-next ()
  "M-n: 一つ後の履歴を呼び出す。"
  (interactive)
  (if (< (point) (my-mini-repl-find-prompt-start))
      (message "Text is read-only")
    (let ((input-start (my-mini-repl-find-input-start)))
      (if (< my-mini-repl-history-index 0)
          (message "これ以上新しい履歴はありません")
        (setq my-mini-repl-history-index (1- my-mini-repl-history-index))
        (let ((inhibit-read-only t))
          (delete-region input-start (point-max))
          (if (< my-mini-repl-history-index 0)
              (setq my-mini-repl-history-index -1)
            (insert (nth my-mini-repl-history-index my-mini-repl-history))))))))

;;;###autoload
(defun my-mini-repl ()
  "簡易 REPL バッファを起動します。"
  (interactive)
  (let ((buf (get-buffer-create "*My Mini REPL*")))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (unless (eq major-mode 'my-mini-repl-mode)
        (my-mini-repl-mode)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "=== My Mini REPL (M-p/M-n:履歴, C-d:終了, Enter:エコー) ===\n")
          (insert my-mini-repl-prompt)
          ;; 初回プロンプトの保護と伝染防止
          (let ((p-start (- (point) (length my-mini-repl-prompt)))
                (p-end (point)))
            (put-text-property p-start p-end 'read-only t)
            (put-text-property p-start p-end 'rear-nonsticky '(read-only))))
        (goto-char (point-max))
        (setq my-mini-repl-history-index -1)))))
