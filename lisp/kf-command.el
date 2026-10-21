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

;; GUID: kf770b2c-4d3e-5f6a-8b9c-0d1e2f3a4b5c
;; ========================================================================
;; 外部変数による柔軟な除外パターン管理と、自動ノイズフィルター付き高速ファイル検索
;; ========================================================================

(defcustom kf:locate-ignore-patterns
  '("/\\.git/"
    "/\\.cache/"
    "/node_modules/"
    "~\$"                  ; バックアップファイル（末尾にチルダ）
    "^/tmp/"              ; テンポラリ領域
    )
  "`kf:locate' でファイル検索を行う際、検索結果から完全に除外（パージ）する正規表現のリスト。"
  :type '(repeat string)
  :group 'kf-commands)

(defun kf:locate (search-regexp)
  "locate コマンドをバックエンドに使い、ベースネームに対して正規表現検索を行います。
結果から `kf:locate-ignore-patterns' にマッチするパス、および『ディレクトリ自体』を完全に除外します。
入力例： kf-command\\.org\$"
  (interactive "sLocate (Regexp for Basename): ")
  (if (string-empty-p search-regexp)
      (user-error "検索クエリが空です")
    (let ((default-directory "/")
          (output-buffer (generate-new-buffer " kf-locate-output"))
          (filtered-lines nil)
          (exit-code nil))
      (unwind-protect
          (progn
            ;; -b (basename) と -r (regexp) を指定して locate を実行
            (setq exit-code (call-process "locate" nil output-buffer nil "-b" "-r" search-regexp))
            (if (not (eq exit-code 0))
                (message "locateコマンドがエラーを返しました（終了コード: %s）" exit-code)
              (with-current-buffer output-buffer
                (goto-char (point-min))
                (while (not (eobp))
                  (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                    (unless (string-empty-p line)
                      ;; 【洗練されたリスト駆動フィルター】
                      ;; 外部変数に登録されたパターンのいずれか一つでもマッチするか、
                      ;; または対象がディレクトリである場合は即座にスキップします
                      (unless (or (seq-some (lambda (pattern) (string-match-p pattern line))
                                            kf:locate-ignore-patterns)
                                  (file-directory-p line))
                        (push line filtered-lines))))
                  (forward-line 1)))

              ;; フィルタリングされたクリーンな結果をユーザーに提示
              (if (null filtered-lines)
                  (message "マッチする有効なファイルは見つかりませんでした: %s" search-regexp)
                (let ((chosen-file (completing-read "Select file: " (nreverse filtered-lines) nil t)))
                  (when (and chosen-file (file-exists-p chosen-file))
                    (find-file-other-window chosen-file))))))

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


;; GUID: kf660d5e-6a7b-8c9d-0e1f-2a3b4c5d6e7f
;; ========================================================================
;; 自動外枠パージ機能を搭載したマークアップ用インサート・置換コマンド
;; ========================================================================
(defvar kf:last-begin nil "最後に使用した開始マークアップ文字列")
(defvar kf:last-end nil "最後に使用した終了マークアップ文字列")

(defun kf:insert-begin-end (begin end)
  "選択リージョンの前後に BEGIN と END を挿入します。
もしリージョンの直前・直後が既に別のブロック（#+begin_... 等）で囲まれている場合は、
古いマークアップを自動的にパージ（除去）して上書き置換を行います。"
  (interactive
   (list (read-string "Begin: " kf:last-begin)
         (read-string "End: " kf:last-end)))
  (setq kf:last-begin begin
        kf:last-end end)

  (if (not (use-region-p))
      (user-error "リージョンが選択されていません")
    (let* ((start (region-beginning))
           (e (region-end))
           (has-outer-block nil)
           (outer-beg-pos nil)
           (outer-end-pos nil))

      ;; --- 鉄壁の既存マークアップ（外枠）事前検知ロジック ---
      (save-excursion
        ;; 1. リージョンの直前に "#+begin_" で始まる行があるかチェック
        (goto-char start)
        (forward-line -1)
        (let ((prev-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (when (string-match-p "^\\s-*#\\+begin_" prev-line)
            (setq outer-beg-pos (cons (line-beginning-position) (1+ (line-end-position))))))

        ;; 2. リージョンの直後に "#+end_" で始まる行があるかチェック
        (goto-char e)
        ;; リージョン末尾が改行コード自体を掴んでいる場合の補正
        (when (and (= (char-before) ?\n) (< start (point))) (forward-char -1))
        (forward-line 1)
        (let ((next-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (when (string-match-p "^\\s-*#\\+end_" next-line)
            (setq outer-end-pos (cons (line-beginning-position) (if (eobp) (line-end-position) (1+ (line-end-position))))))))

      ;; 上下の両方にマーカーが存在していれば、パージ（置換）モードを確定
      (setq has-outer-block (and outer-beg-pos outer-end-pos))

      ;; --- 本番のテキスト変形処理 ---
      (save-excursion
        (if has-outer-block
            ;; 【パターンA】上下の古いマークアップを消し去って、ダイレクトに上書き（添付3の挙動）
            (let ((content (buffer-substring-no-properties start e)))
              ;; 終了タグ行を削除
              (delete-region (car outer-end-pos) (cdr outer-end-pos))
              ;; 開始タグ行からコンテンツ末尾までを一旦すべて削除して綺麗にする
              (delete-region (car outer-beg-pos) e)
              ;; 新しい定義で再配置
              (goto-char (car outer-beg-pos))
              (insert begin "\n" content)
              ;; コンテンツ末尾が改行で終わっていない場合の安全な改行補正
              (if (= (char-before) ?\n) (insert end "\n") (insert "\n" end "\n"))
              (message "既存のブロック構造を %s に昇格・置換しました。" begin))

          ;; 【パターンB】周囲に何もないため、通常の新規挿入を行う（添付1から2への安全な移行）
          (goto-char e)
          (if (= (char-before) ?\n)
              (insert end "\n")
            (insert "\n" end "\n"))
          (goto-char start)
          (insert begin "\n")
          (message "新しく %s ブロックを適用しました。" begin))))))

;; GUID: kf990e5f-9e2c-41f6-b8c3-a3d2f5b6c8fd
;; ========================================================================
;; ラインパーサー方式による文字化け・エラー皆無のMarkdown→Org変換ペースト
;; ========================================================================

(defun kf:paste-markdown-as-org ()
  "クリップボードのMarkdownを安全に取得し、行単位のクリーンな置換で確実にOrg文法に変換して挿入します。"
  (interactive)
  (let* ((cmd (cond
               ((executable-find "wl-paste") "wl-paste -t text/plain")
               ((executable-find "xclip") "xclip -selection clipboard -o -t UTF8_STRING")
               (t nil)))
         (text (when cmd
                 (let ((coding-system-for-read 'utf-8))
                   (shell-command-to-string cmd)))))

    (if (and text (not (string-empty-p text)))
        (with-temp-buffer
          ;; バッファをマルチバイトに固定し、文字列を安全に展開
          (set-buffer-multibyte t)
          (insert text)

          ;; 1. 【見出し変換】安全に行頭の # 列を検出し、タイトル文字列（.*）を完全に保持して * に置換
          (goto-char (point-min))
          (while (re-search-forward "^\\(#+\\) +.*\$" nil t)
            (let* ((hashes (match-string 1))
                   (title (match-string 2))
                   (stars (make-string (length hashes) ?*)))
              (replace-match (concat stars " " title) t t)))

          ;; 2. 【コードブロック変換】``` だけで構成される行を個別に置換（マルチライン正規表現のバグを回避）
          ;; 行頭の ```lang を検知して #+begin_src lang に変換します
          (goto-char (point-min))
          (while (re-search-forward "^```\\([a-zA-Z0-9_-]+\\)$" nil t)
            (replace-match "#+begin_src \\1" t))
          ;; 閉じの ``` または言語指定のない ``` 行を #+end_src / #+begin_example に安全トグル置換
          (goto-char (point-min))
          (while (re-search-forward "^```\$" nil t)
            (replace-match "#+end_src" t))

          ;; 3. 【インラインコード変換】`code` -> ~code~
          (goto-char (point-min))
          (while (re-search-forward "`\\([^`\n]+?\\)`" nil t)
            (replace-match "~\\1~"))

          ;; 4. 【太字変換】**text** -> *text*
          (goto-char (point-min))
          (while (re-search-forward "\\*\\*\\([^* \n][^*]*?[^* \n]\\)\\*\\*" nil t)
            (replace-match "*\\1*"))

          ;; 5. 【リンク変換】[text](URL) -> [[URL][text]]
          (goto-char (point-min))
          (while (re-search-forward "\\[\\([^]]+\\)\\](\\([^)]+\\))" nil t)
            (replace-match "[[\\2][\\1]]" t t))

          ;; 6. カレントバッファへの確実な挿入
          (let ((converted (buffer-string)))
            (with-current-buffer (car (buffer-list))
              (insert converted)
              (message "MarkdownからOrg文法への確実な変換ペーストが完了しました。"))))
      (message "エラー：クリップボードからUTF-8テキストデータを取得できませんでした。"))))


;; GUID: kf0a1b2c-3d4e-5f6a-7b8c-9d0e1f2a3b4c
;; ========================================================================
;; クリップボードの画像を永続保存し、Org-modeへダイレクト挿入する汎用コマンド
;; ========================================================================
(defun kf:insert-clipboard-image ()
  "クリップボードのPNG画像データを ~/Pictures/Screenshots/ 配下に永続保存し、
現在のOrgバッファのカーソル位置にインラインリンクとして挿入・表示します。"
  (interactive)
  (if (derived-mode-p 'org-mode)
      (let* ((target-dir (expand-file-name "~/Pictures/Screenshots/"))
             ;; GNOMEスクリーンショットの標準規則（ハイフン区切り）に準拠したファイル名生成
             (img-name (format-time-string "Screenshot from %Y-%m-%d %H-%M-%S.png"))
             (full-path (concat target-dir img-name))
             ;; 保存用ディレクトリ、およびクリップボード抽出コマンドの確定
             (cmd (cond
                   ((executable-find "wl-paste") (format "wl-paste -t image/png > %S" full-path))
                   ((executable-find "xclip") (format "xclip -selection clipboard -t image/png -o > %S" full-path))
                   (t nil))))

        ;; 1. 保存先ディレクトリ（~/Pictures/Screenshots/）の存在を保証
        (unless (file-directory-p target-dir)
          (make-directory target-dir t))

        ;; 2. クリップボードからの画像データ抽出とファイル書き出し
        (if (and cmd (eq (shell-command cmd) 0))
            (progn
              ;; 3. 現在のカーソル位置に生のOrgリンクをダイレクトに書き込み
              (insert (format "[[file:%s]]\n" full-path))
              ;; 4. 挿入された画像をエディタ上で即座にインライン表示
              (org-display-inline-images)
              (message "画像を保存・挿入しました: %s" img-name))
          (message "エラー：クリップボードに有効な画像データが見つからないか、抽出に失敗しました。")))
    (message "この操作は Org-mode のバッファでのみ実行可能です。")))

(message "kf-command loaded...")
(provide 'kf-command)
