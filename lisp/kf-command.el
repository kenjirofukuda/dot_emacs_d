;; シェルコマンドの結果を挿入
(defun kf:insert-shell-command (start end command)
  "シェルコマンドの結果を挿入"
  (interactive (let (string)
		 (setq string (read-shell-command "Shell command on region(and insert): "))
		 (list (point) (point) string)))
  (let ((output-buffer t)
	(replace t))
    (shell-command-on-region start end command output-buffer replace)))

(defun kf:open-file-thing ()
  "カーソル行のファイルを開く"
  (interactive)
  (let ((filename (thing-at-point 'filename t))
	(url (thing-at-point 'url t)))
    ;; (message "filename: %s" filename)
    ;; (message "     url: %s" url)
    (when url
      (message url)
      (browse-url-chrome url))
    (when (and filename (file-exists-p filename))
      (find-file filename))
    ))
(global-set-key (kbd "\C-c <f3>") 'kf:open-file-thing)

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

(message "kf-command loaded...")
(provide 'kf-command)
