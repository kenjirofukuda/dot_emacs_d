(defun kf:dired-buffers ()
  (loop for buf in (buffer-list)
        for m = (with-current-buffer buf major-mode)
        when (eq m 'dired-mode)
        collect (buffer-name buf)))

(defun kf:close-dired-buffers ()
  (interactive)
  (dolist (buff (kf:dired-buffers))
    (kill-buffer buff)))

(defun kf:saved-file-buffers ()
  (seq-filter (lambda (b)
  		(and (buffer-file-name b)
  		     (not (buffer-modified-p b))))
  	      (buffer-list)))

(defun kf:close-saved-buffers ()
  (interactive)
  (dolist (buff (kf:saved-file-buffers))
    (kill-buffer buff)))

(defun kf:font-family-installed-p (fontname)
  ;; フォントがインストールされているかを調べる
  (seq-some (lambda (elt)
              (string-prefix-p fontname (downcase elt)))
            (font-family-list)))

;; シェルコマンドの結果を挿入
(defun kf:insert-shell-command (start end command)
  "シェルコマンドの結果を挿入"
  (interactive (let (string)
		 (setq string (read-shell-command "Shell command on region(and insert): "))
		 (list (point) (point) string)))
  (let ((output-buffer t)
	(replace t))
    (shell-command-on-region start end command output-buffer replace)))


;; Generated: 2026-10-11T09:35:00Z
;; [GUID: fcb72a8c-9c71-4683-af2d-862d51b32d01]
(defun kf:vterm-opend ()
  (seq-find (lambda (b) (string= "*vterm*" (buffer-name b)))
    	    (buffer-list)))

(defun kf:vterm-cd (dir)
  (interactive)
  (with-current-buffer "*vterm*"
    (vterm-send-string (concat "cd " dir))
    (vterm-send-return)))

(defun kf:vterm-visit (dir)
  (interactive)
  (kf:vterm-cd dir)
  (switch-to-buffer-other-window "*vterm*"))

(defun kf:vterm-quit-wait ()
  "vterm に C-d を送信し、履歴が書き込まれてプロセスが終了するまで同期的に待機します。"
  (interactive)
  (let ((buf (kf:vterm-opend)))
    (when buf
      (let ((proc (get-buffer-process buf)))
        (when (and proc (process-live-p proc))
          ;; 1. vterm バッファを通じて bash に明確に C-d を送り、正規の終了（履歴書き込み）を促す
          (with-current-buffer buf
            (vterm-send-C-d))
          ;; 2. bash が終了処理（.bash_history への書き込み）を完遂して
          ;;    プロセスが完全に消滅するまで、最大2秒間（0.05秒刻みで）同期的に待機する
          (let ((counter 0))
            (while (and (process-live-p proc) (< counter 40))
              (sit-for 0.05)
              (setq counter (1+ counter))))
          ;; 3. プロセスが安全に消滅したことを確認してからバッファをクリーンアップする
          (unless (process-live-p proc)
            (kill-buffer buf)))))))

(defun kf:quit-emacs ()
  (interactive)
  (kf:close-saved-buffers)
  (kf:close-dired-buffers)
  ;; 履歴を保存させるための同期的終了シーケンスを最初に呼ぶ
  (kf:vterm-quit-wait)
  (save-buffers-kill-terminal)
  t)

(defun kf:open-file-thing ()
  "カーソル行のファイルを開く"
  (interactive)
  (let* ((url-part (kf:get-org-url-part-in-caption))
  	 (filename (or (thing-at-point 'filename t)
  		       (and url-part
  			    (string-match-p "^file:///" url-part))))
  	 (url (or (thing-at-point 'url t)
  		  url-part)))
    (when url
      (cond ((string-match-p "^file:///" url)
             (eww-browse-url url)
             (message "eww: %s" url))
            (t (message "chrome: %s" url)
               (browse-url-chrome url))))
    (when (and filename (file-exists-p filename))
      (cond ((string-match-p "\\.wav$" filename)
             (play-sound-file filename))
            (t
             (find-file-other-window filename))))))
(global-set-key (kbd "\C-c <f3>") 'kf:open-file-thing)


(defun kf:ensure-load-file (file)
  "ファイルが存在すればロードする"
  (let ((full-path (expand-file-name file)))
    (when (file-exists-p full-path)
      (load-file full-path))))


;; ウィンドウのバッファを固定する
;; Similar to: http://stackoverflow.com/questions/43765/pin-emacs-buffers-to-windows-for-cscope/65992#65992

(defun kf:pin-buffer ()
  "Pin buffer to current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "pinned buffer" "un-pinned buffer")))

;; ファイルが適切なソースコードか?
(defun kf:valid-project-file (path)
  "ファイルが適切なソースコードか?"
  (not (or (string-match-p ".*/\\.ccls-cache/.*" path)
	   (string-match-p ".*/\\.git/.*" path))))

(defun kf:duplicate-line ()
  (interactive)
  (duplicate-line)
  (forward-line))

;; Bind C-c j to duplicate-line
(global-set-key (kbd "C-c j") 'kf:duplicate-line)

(defun kf:get-org-url-part-in-caption ()
    ;;; (interactive)
  (defun point-in-range-p (range)
    (and (>= (current-column) (car range)) (< (current-column) (cdr range))))
  (let* ((thing (thing-at-point 'line))
  	 (matched (string-match  "\\[\\[\\(.+\\)\\]\\[\\(.+\\)\\]\\]" thing)))
    (if matched
  	(let ((url-range (cons (match-beginning 1) (match-end 1)) )
  	      (caption-range (cons (match-beginning 2) (match-end 2))))
  	  (if (point-in-range-p caption-range)
  	      ;;(list url-range caption-range)
  	      (substring thing (car url-range) (cdr url-range)))))))

;;; === kf-locate-module.el ===
;; Generated: 2026-10-13T06:27:00Z
;; GUID: 4a2b9c3d-7e1f-4a8b-9c2d-5e6f7a8b9c0d
(defun kf:locate (search-string)
  "仮想コンソールを開くことなく、locateコマンドを非同期に実行してファイルを高速検索します。
結果はミニバッファで一行ずつ選択（completing-read）でき、選択したファイルを即座に開きます。"
  (interactive "sLocate file: ")
  (if (string-empty-p search-string)
      (user-error "検索クエリが空です")
    (let ((default-directory "/")
	  (output-buffer (generate-new-buffer " kf-locate-output"))
	  (exit-code nil))
      (unwind-protect
	  (progn
	    ;; 同期または非同期でlocateコマンドの出力を一時バッファに格納
	    ;; 高速なインデックス検索のため、通常のファイル走査より著しく高速です
	    (setq exit-code (call-process "locate" nil output-buffer nil search-string))
	    (if (not (eq exit-code 0))
		(message "locateコマンドがエラーを返しました（終了コード: %s）" exit-code)
	      (with-current-buffer output-buffer
		(let ((lines nil))
		  (goto-char (point-min))
		  (while (not (eobp))
		    (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
		      (unless (string-empty-p line)
			(push line lines)))
		    (forward-line 1))
		  (if (null lines)
		      (message "マッチするファイルは見つかりませんでした: %s" search-string)
		    ;; 補完インターフェース（Vertico, Selectrum, 又は標準のcompleting-read）で選択
		    (let ((chosen-file (completing-read "Select file: " (nreverse lines) nil t)))
		      (when (and chosen-file (file-exists-p chosen-file))
			(find-file chosen-file))))))))
	;; 一時バッファを確実にクリーンアップ
	(when (buffer-live-p output-buffer)
	  (kill-buffer output-buffer))))))
(defun kf:updatedb ()
  "仮想コンソールを開くことなく、locateコマンドのデータベース(updatedb)を非同期プロセスで更新します。
一般ユーザー環境での実行を想定し、必要に応じてpkexecまたはsudo経由で実行します。
Debian環境でpolkitが設定されている場合は、グラフィカル、またはミニバッファでのパスワード要求が発生します。"
  (interactive)
  (message "locate データベース(updatedb)を非同期で更新中...")
  (let* ((process-name "kf-updatedb-process")
	 (buffer-name " kf-updatedb-log")
	 ;; Debian 13環境で特権昇格を伴うupdatedbを非同期実行するためのコマンド配列
	 ;; パスワード要求を処理するため、環境に応じて適切なラッパーを選択してください
	 (proc (start-process process-name buffer-name "pkexec" "updatedb")))
    (set-process-sentinel proc
			  (lambda (process event)
			    (when (string-match-p "finished" event)
			      (message "locate データベース(updatedb)の更新が正常に完了しました。")
			      (when (get-buffer " kf-updatedb-log")
				(kill-buffer " kf-updatedb-log")))))))


(defvar kf:last-begin nil "Last used begin string.")
(defvar kf:last-end nil "Last used end string.")

(defun kf:insert-begin-end (begin end)
  "Insert BEGIN before and END after the region."
  (interactive
   (list (read-string "Begin: " kf:last-begin)
         (read-string "End: " kf:last-end)))
  (setq kf:last-begin begin
        kf:last-end end)
  (save-excursion
    (let ((start (region-beginning))
          (e (region-end)))
      (goto-char e)
      (if (= (char-before) ?\n)
          (insert end "\n")
        (insert "\n" end "\n"))
      (goto-char start)
      (insert begin "\n"))))

;; GUID: kf990e5f-9e2c-41f6-b8c3-a3d2f5b6c8fd
;; ========================================================================
;; pandoc外部ツールおよび一時ファイル連携による確実なMarkdown→Org変換ペースト
;; ========================================================================

(defun kf:paste-markdown-as-org ()
  "クリップボードのMarkdownを、外部ツールpandocを利用して確実にOrg文法に変換して挿入します。"
  (interactive)
  (let* (;; 1. システムに応じた一時ディレクトリ内のユニークなファイルパスを確定
         ;; 質問者様の構成済み一時領域（my-temporary-dir）を活用します
         (temp-dir (file-name-as-directory my-temporary-dir))
         (md-file (concat temp-dir "clipboard_input.md"))
         (org-file (concat temp-dir "clipboard_output.org"))

         ;; 2. 環境に存在する最適なクリップボード取得コマンドを確定
         (clip-cmd (cond
                    ((executable-find "wl-paste") "wl-paste -t text/plain")
                    ((executable-find "xclip") "xclip -selection clipboard -o -t UTF8_STRING")
                    (t nil))))

    ;; 事前に一時ディレクトリの存在を保証
    (make-directory temp-dir t)

    (if (and clip-cmd (executable-find "pandoc"))
        ;; 3. クリップボードから生データをUTF-8テキストとして一時ファイルへ確実に書き出し
        (let ((coding-system-for-read 'utf-8)
              (coding-system-for-write 'utf-8)
              (raw-markdown (shell-command-to-string clip-cmd)))

          (if (and raw-markdown (not (string-empty-p raw-markdown)))
              (progn
                ;; Markdownの生データをファイルへ書き出し
                (with-temp-file md-file
                  (insert raw-markdown))

                ;; 4. pandoc外部プロセスを呼び出し、MarkdownからOrg-modeへ超高精度に変換
                ;; --wrap=none を付加することで、日本語の単語境界で意図しない改行が挟まるのを防ぎます
                (shell-command (format "pandoc -f markdown -t org %s -o %s --wrap=none"
                                       (shell-quote-argument md-file)
                                       (shell-quote-argument org-file)))

                ;; 5. 変換されたOrgファイルをマルチバイトを維持したままバッファへ流し込み
                (if (file-exists-p org-file)
                    (let ((converted-content (with-temp-buffer
                                               (insert-file-contents org-file)
                                               (buffer-string))))

                      ;; 現在開いているカレントバッファのカーソル位置へダイレクト挿入
                      (insert converted-content)
                      (message "pandocを介した高精度なOrg文法変換ペーストが成功しました。")

                      ;; 使用済みの一時ファイルを安全にクリーンアップ
                      (ignore-errors
                        (delete-file md-file)
                        (delete-file org-file)))
                  (message "エラー：pandocによるOrgファイルの生成に失敗しました。")))
            (message "エラー：クリップボードから有効なテキストデータを取得できませんでした。")))
      (message "エラー：システムに xclip/wl-clipboard または pandoc が見つかりません。"))))

(message "kf-command loaded...")
(provide 'kf-command)
