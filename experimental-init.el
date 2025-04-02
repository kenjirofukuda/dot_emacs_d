;;; Code:

(with-current-buffer "*scratch*"
  (insert ";;; loading Experimental init.el\n")
  (insert ";;; Emacs " emacs-version "\n")
  (insert "\n")
  (insert "\n"))

(when (file-directory-p "~/.emacs.d/lisp")
  (push (expand-file-name "~/.emacs.d/lisp") load-path))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 80 1024 1024)) ;; 8mb
;; カスタムコマンドのロード
(require 'kf-command)
(require 'mpascal)

(add-to-list 'auto-mode-alist
             '("\\.\\(p\\|pas\\|dpr\\|dpk\\)\\'" . mpascal-mode))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode -1)

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

;; Editor Config
;; https://editorconfig.org/
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; より本物に近いターミナルエミュレータ
(use-package vterm
  :unless (eq system-type 'windows-nt)
  :ensure t)

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
(setq make-backup-files nil)
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
  (sys-base-name "options-experimental" ".el"))

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

;; ミニバッファーの改善
;; https://github.com/minad/vertico
(use-package vertico
  :ensure t
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil)
  (vertico-mode 1))

;; https://github.com/minad/marginalia
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

;; https://github.com/minad/consult
(use-package consult
  :ensure t)

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

;; https://github.com/oantolin/embark/
(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; https://github.com/minad/corfu
(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

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

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (add-hook 'markdown-mode-hook #'turn-off-auto-fill)
  (add-hook 'markdown-mode-hook #'turn-on-visual-line-mode))

(unless (version< emacs-version "29.1")
  (use-package word-wrap-mode
    :hook (visual-line-mode . word-wrap-whitespace-mode)
    :config
    (add-to-list 'word-wrap-whitespace-characters ?\])))

;; (use-package visual-fill-column
;;   :hook (visual-line-mode . visual-fill-column-mode)
;;   :init
;;   (setq visual-line-fringe-indicators '(left-curly-arrow nil))
;;   :config
;;   (setq visual-fill-column-width 120))

(use-package adaptive-wrap
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

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
   (ruby . t)
   (python . t)
   ))

(setq browse-url-browser-function 'eww-browse-url)

;; https://systemcrafters.net/emacs-shorts/pomodoro-timer/
(setq org-clock-sound "/usr/share/sounds/sound-icons/xylofon.wav")
;; C-c C-x ;
(org-timer-set-timer 25)

;; キーストローク表示
;; https://github.com/tarsius/keycast
(use-package keycast
  :ensure t)

;; 日本語の範囲をNotoフォントに設定
(when (display-graphic-p)
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Noto Serif CJK JP"))
  ;; (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Noto Sans CJK JP"))
  )

(use-package eglot
  :ensure t
  :hook
  (c-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  (objc-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '((c-mode c++-mode objc-mode) "clangd")))

;; 構文解析エンジン Tree sitter
(unless (version< emacs-version "29.0")
  (use-package treesit-auto
    :ensure t
    :config
    (setq treesit-auto-install t)
    (global-treesit-auto-mode)))

;; フレームの回転
(use-package transpose-frame
  :ensure t)

;; C-s <araow keys> でウィンドウのサイズをマウスを使わずに調節する
;;
;; Window Resize
;; https://www.emacswiki.org/emacs/WindowResize
;;
(defun win-resize-top-or-bot ()
  "Figure out if the current window is on top, bottom or in the
middle"
  (let* ((win-edges (window-edges))
         (this-window-y-min (nth 1 win-edges))
         (this-window-y-max (nth 3 win-edges))
         (fr-height (frame-height)))
    (cond
     ((eq 0 this-window-y-min) "top")
     ((eq (- fr-height 1) this-window-y-max) "bot")
     (t "mid"))))

(defun win-resize-left-or-right ()
  "Figure out if the current window is to the left, right or in the
middle"
  (let* ((win-edges (window-edges))
         (this-window-x-min (nth 0 win-edges))
         (this-window-x-max (nth 2 win-edges))
         (fr-width (frame-width)))
    (cond
     ((eq 0 this-window-x-min) "left")
     ((eq (+ fr-width 4) this-window-x-max) "right")
     (t "mid"))))

(defun win-resize-enlarge-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window -1))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window 1))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window -1))
   (t (message "nil"))))

(defun win-resize-minimize-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window 1))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window -1))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window 1))
   (t (message "nil"))))

(defun win-resize-enlarge-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally -1))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally -1))))

(defun win-resize-minimize-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally -1))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally 1))))

;; replace all C-M-(down|up|left|right) to
;;             C-s-(down|up|left|right)
;; reason for confilict to paredit
(global-set-key [C-s-down] 'win-resize-minimize-vert)
(global-set-key [C-s-up] 'win-resize-enlarge-vert)
(global-set-key [C-s-left] 'win-resize-minimize-horiz)
(global-set-key [C-s-right] 'win-resize-enlarge-horiz)
(global-set-key [C-s-up] 'win-resize-enlarge-horiz)
(global-set-key [C-s-down] 'win-resize-minimize-horiz)
(global-set-key [C-s-left] 'win-resize-enlarge-vert)
(global-set-key [C-s-right] 'win-resize-minimize-vert)

(use-package sudo-edit
  :ensure t
  :pin melpa)

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package all-the-icons
  :if (display-graphic-p))

(when (display-graphic-p)
  (use-package nerd-icons)
  (unless (kf:font-family-installed-p "all-the-icons")
    (all-the-icons-install-fonts))
  (unless (kf:font-family-installed-p "symbols nerd")
    (nerd-icons-install-fonts)))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package astyle
  :ensure t
  :when (executable-find "astyle"))

(use-package cmake-mode
  :ensure t)
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

;; https://qiita.com/tadsan/items/df73c711f921708facdc
(setq-default show-trailing-whitespace t)
(defun my/disable-trailing-mode-hook ()
  "Disable show tail whitespace."
  (setq show-trailing-whitespace nil))

(defvar my/disable-trailing-modes
  '(comint-mode
    eshell-mode
    eww-mode
    term-mode
    vterm-mode
    twittering-mode))

(mapc
 (lambda (mode)
   (add-hook (intern (concat (symbol-name mode) "-hook"))
             'my/disable-trailing-mode-hook))
 my/disable-trailing-modes)

(use-package doom-modeline
  :if (and (display-graphic-p) (not (eq system-type 'haiku)))
  :init
  (doom-modeline-mode +1))

;; TODO この対策は一時的なものであるため、通常のoptionsに戻すのを忘れないこと
(when (and (display-graphic-p) (kf:font-family-installed-p "iosevka"))
  (let ((font-height (if (< (display-pixel-height) 900) 100 130))
        (custom-set-faces
         ;; custom-set-faces was added by Custom.
         ;; If you edit it by hand, you could mess it up, so be careful.
         ;; Your init file should contain only one such instance.
         ;; If there is more than one, they won't work right.
         ;;
         '(default ((t (:family "Iosevka" :foundry "UKWN" :slant normal :weight regular :height font-height :width normal))))
         ))))

;; デスクトップ環境の保存
;; https://www.gnu.org/software/emacs//manual/html_node/emacs/Saving-Emacs-Sessions.html
(desktop-save-mode (if (display-graphic-p) +1 -1))
;; ホスト名を付加してファイル名衝突回避
(setq desktop-base-file-name (concat "." (safe-host-name-string) "-emacs.desktop"))

(use-package ef-themes
  :ensure t
  :config
  (ef-themes-select 'ef-cyprus))

;; https://agel.readthedocs.io/en/latest/index.html
(use-package ag
  :ensure t)

(use-package projectile
  :config
  (defun projectile-project-find-function (dir)
    (let* ((root (projectile-project-root dir)))
      (and root (cons 'transient root))))
  (with-eval-after-load 'project
    (add-to-list 'project-find-functions 'projectile-project-find-function))
  )

;; (use-package eglot
;;   :bind (:map eglot-mode-map
;;               ("C-c C-d" . eglot-help-at-point)
;;               ("C-c C-r" . eglot-code-actions))
;;   :hook
;;   ((c-mode-common . eglot-ensure))
;;   )

(defun kf:c-mode-init ()
  (c-toggle-auto-newline 1)
  (setq c-hanging-braces-alist '((statement-open before after)
                                 (substatement-open before after) ))
  )
(defun kf:objc-mode-init ()
  (c-toggle-auto-newline 1)
  (setq c-hanging-braces-alist '((statement before after)
                                 (statement-open before after)
                                 (substatement-open before after) ))
  )
(add-hook 'c-mode-hook 'kf:c-mode-init)
(add-hook 'objc-mode-hook 'kf:objc-mode-init)

;; https://qiita.com/fujimisakari/items/a6ff082f0e8eddc09511
;; .hファイルもobjc-modeで開くけるようにする
(add-to-list 'magic-mode-alist
             `(,(lambda ()
                  (and (string= (file-name-extension buffer-file-name) "h")
                       (re-search-forward "@\\<interface\\>"
                                          magic-mode-regexp-match-limit t)))
               . objc-mode))

(use-package clang-format
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.gsmarkup$" . xml-mode))

;; NeoTree
;; https://tsuu32.hatenablog.com/entry/2020/08/19/004306
(use-package neotree
  :ensure t
  :config
  (setq neo-show-hidden-files t) ; dot-fileも表示する
  (setq neo-theme 'icons) )

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Haiku build system
(kf:ensure-load-file "~/.emacs.d/lisp/jam-mode.el")

(use-package multiple-cursors
  :ensure t)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

(use-package org-transclusion
  :ensure t
  :after org)
(define-key global-map (kbd "C-c <f12>") #'org-transclusion-add)
(define-key global-map (kbd "C-c t") #'org-transclusion-mode)

(use-package yasnippet)
(use-package yasnippet-snippets)

;; https://github.com/roswell/roswell
(kf:ensure-load-file "~/.roswell/helper.el")

;; https://github.com/rversteegen/fb-mode
(kf:ensure-load-file "~/.emacs.d/lisp/fb-mode.el")

(use-package d-mode
  :ensure t)

(recentf-open-files)
