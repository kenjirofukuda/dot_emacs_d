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

(defun kf:vterm-quit ()
  (interactive)
  (if (kf:vterm-opend)
      (with-current-buffer "*vterm*"
  	(vterm-send-C-d))))

(defun kf:quit-emacs ()
  (interactive)
  (kf:close-saved-buffers)
  (kf:close-dired-buffers)
  (kf:vterm-quit)
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

(message "kf-command loaded...")
(provide 'kf-command)
