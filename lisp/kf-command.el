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

(message "kf-command loaded...")
(provide 'kf-command)
