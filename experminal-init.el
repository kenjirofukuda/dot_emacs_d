;;;

;;; Code:

(with-current-buffer "*scratch*"
  (insert ";;; loading Experminal init.el"))

(when (file-directory-p "~/.emacs.d/lisp")
  (push (expand-file-name "~/.emacs.d/lisp") load-path))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 8 1024 1024)) ;; 1mb

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

;; Magit 設定
(use-package magit
    :ensure t
    :pin melpa)

(use-package git-gutter
  :init
  (progn
    (global-git-gutter-mode t)
    (git-gutter:linum-setup))
  :bind
  (("C-x C-g" . git-gutter-mode))
  :config
  (progn
    (custom-set-variables
     '(git-gutter:modified-sign "  ")
     '(git-gutter:added-sign "++")
     '(git-gutter:deleted-sign "--"))
    (set-face-background 'git-gutter:modified "purple")
    (set-face-foreground 'git-gutter:added "green")
    (set-face-foreground 'git-gutter:deleted "red")))
