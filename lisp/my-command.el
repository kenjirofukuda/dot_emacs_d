
(current-buffer)

(get-buffer "*scratch*")

(get-buffer-create "kenjirofukuda")

(progn
  (set-buffer (get-buffer "kenjirofukuda"))
  (current-buffer))

(progn
  (set-buffer "kenjirofukuda")
  (current-buffer))

(setq my-org-files '("Kenjiro1.org" "Kenjiro2.org" "Kenjiro3.org"))

(dolist (org-file my-org-files)
  (message "File: %s" org-file))


(dolist (org-file my-org-files)
  (let ((file-path (expand-file-name org-file "~/Nextcloud/")))
    (message "File: %s" file-path)))

(dolist (org-file my-org-files)
  (let ((file-path (expand-file-name org-file "~/Nextcloud/")))
    (with-current-buffer (or (get-file-buffer file-path)
                             (find-file-noselect file-path))
      (message "File: %s" (buffer-file-name)))))

       
(with-current-buffer "my-command.org"
  (save-excursion
    (thing-at-point 'word)))

(with-current-buffer "my-command.org"
  (save-excursion
    (thing-at-point 'sentence t)))

(with-current-buffer "my-command.org"
  (save-excursion
    (thing-at-point 'sexp t)))


;; これは実在するワードなのでエラーを起こさない
(with-current-buffer "my-command.org"
  (save-excursion
    (goto-char (point-min))
    (search-forward "thing")))


;; これは実在 !!!しない!!! ワードなのでエラーを起こす
(with-current-buffer "my-command.org"
  (save-excursion
    (goto-char (point-min))
    (search-forward "thing2")))

;; これは実在 !!!しない!!! ワードだが
;; オプションパラメータの2番目に t を渡すことで回避できる
;; ただし、戻り値は nil
(with-current-buffer "my-command.org"
  (save-excursion
    (goto-char (point-min))
    (search-forward "thing2" nil t)))


;; 現在を末尾から検索し、行を取得する
(with-current-buffer "my-command.org"
  (save-excursion
    (goto-char (point-max))
    (search-backward "現在")
    (thing-at-point 'sentence t)))


;;; パス名
(file-name-directory "~/home/kenjiro/Documents/gnustep.tar.gz")
;; => "~/home/kenjiro/Documents/"

(file-name-nondirectory "~/home/kenjiro/Documents/gnustep.tar.gz")
;; => "gnustep.tar.gz"

(file-name-extension "~/home/kenjiro/Documents/gnustep.tar.gz")
;; => "gz"

(file-name-sans-extension "~/home/kenjiro/Documents/gnustep.tar.gz")
;; => "~/home/kenjiro/Documents/gnustep.tar"

(file-name-base "~/home/kenjiro/Documents/gnustep.tar.gz")
;; => "gnustep.tar"

(file-name-as-directory "~/home/kenjiro/Documents/gnustep.tar.gz")
;; => "~/home/kenjiro/Documents/gnustep.tar.gz/"

;;; パス名の解決

(file-name-absolute-p "~/home/kenjiro/Documents/gnustep.tar.gz")
;; => t

(file-name-absolute-p "gnustep.tar.gz")
;; => nil

(file-name-absolute-p "Documents/gnustep.tar.gz")
;; => nil

(file-name-absolute-p "./Documents/gnustep.tar.gz")
;; => nil

(file-relative-name (buffer-file-name) "~/Nextcloud")
;; => "../.emacs.d/lisp/mycommand.el"

(file-relative-name (buffer-file-name) "~/.emacs.d")
;; => "lisp/mycommand.el"

(expand-file-name "my-command.org")
;; => "/home/kenjiro/.emacs.d/lisp/my-command.org"

;; そのままでは変数展開はしてくれない
(expand-file-name "$HOME/.emacs.d")
;; => "/home/kenjiro/.emacs.d/lisp/$HOME/my-command.org"

;; 変数展開はこちらを使うべき
(substitute-in-file-name "$HOME/.emacs.d")
;; => "/home/kenjiro/.emacs.d"

(defun my-format ()
  (interactive)
  (insert (shell-command-to-string "pwd")))

(defun my-pathname ()
  "Insert current buffer's path name"
  (interactive)
  (insert (buffer-file-name (current-buffer))))

(defun my:shell-command-on-region-and-insert (start end command)
  "Shell command in region and insert"
  (interactive (let (string)
                 (unless (region-active-p)
                   (user-error "The mark is not set now, so there is no region"))
                 (setq string (read-shell-command "Shell command on region(and insert): "))
                 (list (region-beginning) (region-end) string))
               )
  (let ((output-buffer t)
        (replace t))
    (shell-command-on-region start end command output-buffer replace)))


(require 'color)
(progn
  (let ((block-back-color
         (color-darken-name
          (face-attribute 'default :background)
          15)))
    
    (set-face-attribute 'org-block-begin-line nil
                        :foreground "#000000"
                        :background block-back-color)
    
    (set-face-attribute 'org-block-end-line nil
                        :foreground "#000000"
                        :background block-back-color)
    
    (set-face-attribute 'org-block nil
                        :background block-back-color))
  )


;; (directory-files DIRECTORY &optional FULL MATCH NOSORT COUNT)
;; ベース名のみの取得
(directory-files "~/.emacs.d/lisp/")

;; フルパスで取得
(directory-files "~/.emacs.d/lisp/" t)

;; .el のみ取得
(directory-files "~/.emacs.d/lisp/" t ".el")

;; ソートしない
(directory-files "~/.emacs.d/lisp/" t "" t)

;; ３つまで
(directory-files "~/.emacs.d/lisp/" t "" nil 3)

;; (directory-files-recursively
;;   DIR
;;   REGEXP
;;   &optional INCLUDE-DIRECTORIES
;;             PREDICATE
;;             FOLLOW-SYMLINKS)

;; PREDICATE には lambda 関数を渡すことができる

;;
(directory-files-recursively "~/.emacs.d/" "\\.el$")

;; gnustep プロジェクトの中で、純粋な ソースコードヘッダーファイルを取得
(directory-files-recursively
 "~/Documents/github/kenjirofukuda/gdsfeel-gnustep/" ".[mhc]$"
 nil
 (lambda (path) ;; ccls キャシュを除外
   (not (or (string-match-p ".*/\\.ccls-cache/.*" path)
            (string-match-p ".*/\\.git/.*" path)))))

;; 転送先がディレクトリの場合 / がないとエラー
(copy-file "~/.emacs.d/init.el" "/tmp")

(copy-file "~/.emacs.d/init.el" "/tmp/")
(directory-files "/tmp" nil "\\.el$")

;; 2回目はすでに存在するのでエラー
(copy-file "~/.emacs.d/init.el" "/tmp/")

;; 上書きには t 指定が必要
(copy-file "~/.emacs.d/init.el" "/tmp/" t)

;; (copy-directory DIRECTORY NEWNAME
;;    &optional KEEP-TIME
;;              PARENTS
;;              COPY-CONTENTS

;; copy-file 同様 / で終わらないとエラー
(copy-directory "~/.emacs.d/lisp" "/tmp")

;; コピー可能
(copy-directory "~/.emacs.d/lisp" "/tmp/")
(directory-files "/tmp" nil "^lisp$")

(copy-directory "~/.emacs.d/eshell" "/tmp/lisp")
;; Debugger entered--Lisp error: (file-already-exists "File exists" "/tmp/lisp")

(copy-directory "~/.emacs.d/eshell" "/tmp/lisp/")
;; /tmp/lisp/eshell

;; この２つは今ひとつ理解できなかった
(copy-directory "~/.emacs.d/eshell" "/tmp/lisp" t t nil)
;; ("/tmp/lisp/history" "/tmp/lisp/lastdir")
(copy-directory "~/.emacs.d/eshell" "/tmp/lisp" t t t)


(file-exists-p "~/.emacs.d/eshell")

(directory-files-recursively "~/.emacs.d/eshell" "" )


(provide 'my-command)
