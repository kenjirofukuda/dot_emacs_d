;;;

;;; Code:

(with-current-buffer "*scratch*"
  (insert ";;; loading Experminal init.el\n\n"))

(when (file-directory-p "~/.emacs.d/lisp")
  (push (expand-file-name "~/.emacs.d/lisp") load-path))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 8 1024 1024)) ;; 8mb

;; パッケージアーカイブ
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(eval-and-compile
  (require 'package)
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
		       ("melpa" . "https://melpa.org/packages/")
		       ("melpa-stable" . "https://mstable.elpa.org/packages/")
		       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))

  (require 'use-package)
  (setq use-package-always-ensure t))

;; パッケージ自動アップデート
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "20:00"))

;; 基本関数
(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(defun safe-system-type-string ()
  (replace-in-string "/" "_" (prin1-to-string system-type)))

(defun safe-host-name-string ()
  (replace-in-string "/" "_" (downcase (system-name))))

(defun sys-base-name (prefix suffix)
  (concat prefix "-" (safe-system-type-string) suffix ))

(defun host-base-name (prefix suffix)
  (concat prefix "-" (safe-host-name-string) suffix ))

;; 外部プロセス更新ファイルのバッファ同期
;; https://takaxp.github.io/init.html#orgc2257142
(when (require 'auto-save-buffers nil t)

  (defun my-ox-hugo-auto-saving-p ()
    (when (eq major-mode 'org-mode)
      (or (bound-and-true-p org-capture-mode) ;; when activating org-capture
	  (and (fboundp 'org-entry-get)
	       (equal "" (org-entry-get (point) "EXPORT_FILE_NAME"))))))

  (defun my-auto-save-buffers ()
    (cond ((memq major-mode '(undo-tree-visualizer-mode diff-mode)) nil)
	  ((string-match "Org Src" (buffer-name)) nil)
	  ((let ((pt (point)))
	     (and (string-match ".gpg" (buffer-name))
		  (not (eq pt 1))
		  (string-match (buffer-substring (- pt 1) pt) " "))) nil) ;; .gpg で半角スペースの後ろのブリッツでは自動保存しない．FIXME 半角スペース+行末
	  ((my-ox-hugo-auto-saving-p) nil)
	  (t
	   (auto-save-buffers))))

  (run-with-idle-timer 1.6 t #'my-auto-save-buffers))

(unless noninteractive
  (global-auto-revert-mode 1)
  ;; revert されるのが org バッファのとき，自動的にドロワをたたむ
  ;; カーソルが (point-max) に移動してしまう場合は非推奨
  (with-eval-after-load "org"
    (defun my-org-hide-drawers-all ()
      (when (eq major-mode 'org-mode)
	(org-cycle-hide-drawers 'all)))
    (add-hook 'after-revert-hook 'my-org-hide-drawers-all)))

;; 最近訪れたファイル対応
(defun recentf-base-name ()
  (host-base-name "recentf" ".dat"))

(defun recentf-file-name ()
  (expand-file-name (locate-user-emacs-file (recentf-base-name))))

(setq recentf-save-file (recentf-file-name))

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(defun options-base-name ()
  (sys-base-name "options-experminal" ".el"))

(defun options-file-name ()
  (expand-file-name (locate-user-emacs-file (options-base-name))))

(setq custom-file (options-file-name))
(if (file-exists-p custom-file)
    (load custom-file t nil nil))

;; 拡張選択範囲
(use-package expand-region
  :ensure t)
(global-set-key (kbd "C-q") 'er/expand-region)

;; コード補完
(use-package company
  :ensure t)

;; Magit 設定
(use-package magit
  :ensure t
  :pin melpa)

;; https://joppot.info/posts/f3007a42-5ba2-4060-90d4-496697413cf9
(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (unless (window-system) (diff-hl-margin-mode))
  :custom-face
  (diff-hl-change ((t (:background "#8adf80"))))
  (diff-hl-delete ((t (:background "#ff8f88"))))
  (diff-hl-insert ((t (:background "#bfc9ff"))))
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; Lisp ファミリ基本設定
(show-paren-mode t)
(use-package paredit
  :ensure t
  :commands enable-paredit-mode
  :hook ((emacs-lisp-mode
	  org-mode)
	 . enable-paredit-mode))

(use-package smartparens :ensure t)
(use-package rainbow-delimiters :ensure t)

;; emacs lisp
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
;; (add-hook 'emacs-lisp-mode-hook 'company-mode)
;; (add-hook 'emacs-lisp-mode-hook 'subword-mode)
;; (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)

(eval-after-load 'inferior-emacs-lisp-mode
  '(progn
     (add-hook 'ielm-mode-hook 'paredit-mode)
     (define-key paredit-mode-map (kbd "RET") nil)
     (define-key paredit-mode-map (kbd "C-j") 'paredit-newline)))

(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)))

;; <s <tab> でブロック文のテンプレートを挿入
(require 'org-tempo)

;; デフォルトのbabelではシェルは禁止されているの使えるようにする
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (shell . t)
   ))

(setq browse-url-browser-function 'eww-browse-url)

;; https://systemcrafters.net/emacs-shorts/pomodoro-timer/
(setq org-clock-sound "/usr/share/sounds/sound-icons/xylofon.wav")
;; C-c C-x ;
(org-timer-set-timer 25)

;; カスタムコマンドのロード
(require 'kf-command)
(recentf-open-files)
(global-display-line-numbers-mode nil)
