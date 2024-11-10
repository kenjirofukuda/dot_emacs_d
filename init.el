;;; init

;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!

(push (expand-file-name "~/.emacs.d/lisp") load-path)

;;(load-file (expand-file-name "~/.emacs.d/lisp/initchart.el"))

;; Load the library
(require 'initchart)

;; Measure the execution time of a specified function for every call.
;; Optionally, you might give a parameter name of the function you specified to
;; record what value is passed to the function.
(initchart-record-execution-time-of load file)
(initchart-record-execution-time-of require feature)


;;; Code:
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 8 1024 1024)) ;; 1mb
(setq lsp-idle-delay 0.5)
(setq lsp-log-io nil)


;; You will most likely need to adjust this font size for your system!
(defvar efs/default-font-size 120)
(defvar efs/default-variable-font-size 120)

;; Make frame transparency overridable
(defvar efs/frame-transparency '(95 . 95))

(defun no-junk-please-were-unixish ()
  (let ((coding-str (symbol-name buffer-file-coding-system)))
    (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
      (set-buffer-file-coding-system 'unix))))

(add-hook 'find-file-hook 'no-junk-please-were-unixish)

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
  (setq use-package-always-ensure t)
  
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t :when (display-graphic-p))
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
    ;;</leaf-install-code>


;; ** Automatic Package Updates

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "20:00"))


;;* Keep Folders Clean
;; TODO

;; * Basic UI Configuration

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)


;; Set frame transparency
(when (display-graphic-p)
  (set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
  (add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
		            eww-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
		neotree-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; ** Font Configuration

;; I am using the [[https://github.com/tonsky/FiraCode][Fira Code]] and [[https://fonts.google.com/specimen/Cantarell][Cantarell]] fonts for this configuration which will more than likely need to be installed on your machine.  Both can usually be found in the various Linux distro package managers or downloaded from the links above.

;; #+begin_src emacs-lisp

(when (display-graphic-p)
  (set-face-attribute 'default nil :font "Fira Code Retina" :height efs/default-font-size)
  (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height efs/default-font-size)
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height efs/default-variable-font-size :weight 'regular))

;; #+end_src


;;* Keybinding Configuration
;; TODO but i am not use evil

;;* UI Configuration

;;** Command Log Mode


;;** Color Theme
;; TODO switch to doom?

;; ** Better Modeline

;; [[https://github.com/seagle0128/doom-modeline][doom-modeline]] is a very attractive and rich (yet still minimal) mode line configuration for Emacs.  The default configuration is quite good but you can check out the [[https://github.com/seagle0128/doom-modeline#customize][configuration options]] for more things you can enable or disable.

;; *NOTE:* The first time you load your configuration on a new machine, you'll need to run `M-x all-the-icons-install-fonts` so that mode line icons display correctly.

;; #+begin_src emacs-lisp

(when (display-graphic-p)
  (use-package all-the-icons))

;; #+end_src


;; ** Which Key

;; [[https://github.com/justbur/emacs-which-key][which-key]] is a useful UI panel that appears when you start pressing any key binding in Emacs to offer you all possible completions for the prefix.  For example, if you press =C-c= (hold control and press the letter =c=), a panel will appear at the bottom of the frame displaying all of the bindings under that prefix and which command they run.  This is very useful for learning the possible key bindings in the mode of your current buffer.

;; #+begin_src emacs-lisp

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; #+end_src

;; ** Text Scaling

;; This is an example of using [[https://github.com/abo-abo/hydra][Hydra]] to design a transient key binding for quickly adjusting the scale of the text on screen.  We define a hydra that is bound to =C-s t s= and, once activated, =j= and =k= increase and decrease the text scale.  You can press any other key (or =f= specifically) to exit the transient key map.

;; #+begin_src emacs-lisp

(when (display-graphic-p)
  (use-package hydra
    :defer t)
  (defhydra hydra-text-scale (:timeout 4)
    "scale text"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("f" nil "finished" :exit t)))

;; (efs/leader-keys
;;  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; #+end_src



;; * Org Mode

;; [[https://orgmode.org/][Org Mode]] is one of the hallmark features of Emacs.  It is a rich document editor, project planner, task and time tracker, blogging engine, and literate coding utility all wrapped up in one package.

;; ** Better Font Faces

;; The =efs/org-font-setup= function configures various text faces to tweak the sizes of headings and use variable width fonts in most cases so that it looks more like we're editing a document in =org-mode=.  We switch back to fixed width (monospace) fonts for code blocks and tables so that they display correctly.

;; #+begin_src emacs-lisp

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  ;; STOP: hyphen replacement
  ;; (font-lock-add-keywords 'org-mode
  ;;                         '(("^ *\\([-]\\) "
  ;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

;; #+end_src


;; ** Basic Config

;; This section contains the basic configuration for =org-mode= plus the configuration for Org agendas and capture templates.  There's a lot to unpack in here so I'd recommend watching the videos for [[https://youtu.be/VcgjTEa0kU4][Part 5]] and [[https://youtu.be/PNE-mgkZ6HM][Part 6]] for a full explanation.

;; #+begin_src emacs-lisp

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
        '("~/Projects/Code/emacs-from-scratch/OrgFiles/Tasks.org"
          "~/Projects/Code/emacs-from-scratch/OrgFiles/Habits.org"
          "~/Projects/Code/emacs-from-scratch/OrgFiles/Birthdays.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
	'(("Archive.org" :maxlevel . 1)
          ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
	'((:startgroup)
					; Put mutually exclusive tags here
          (:endgroup)
          ("@errand" . ?E)
          ("@home" . ?H)
          ("@work" . ?W)
          ("agenda" . ?a)
          ("planning" . ?p)
          ("publish" . ?P)
          ("batch" . ?b)
          ("note" . ?n)
          ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
	'(("d" "Dashboard"
	   ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

	  ("n" "Next Tasks"
	   ((todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))))

	  ("W" "Work Tasks" tags-todo "+work-email")

	  ;; Low-effort next actions
	  ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
	   ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))

	  ("w" "Workflow Status"
	   ((todo "WAIT"
		  ((org-agenda-overriding-header "Waiting on External")
		   (org-agenda-files org-agenda-files)))
            (todo "REVIEW"
		  ((org-agenda-overriding-header "In Review")
		   (org-agenda-files org-agenda-files)))
            (todo "PLAN"
		  ((org-agenda-overriding-header "In Planning")
		   (org-agenda-todo-list-sublevels nil)
		   (org-agenda-files org-agenda-files)))
            (todo "BACKLOG"
		  ((org-agenda-overriding-header "Project Backlog")
		   (org-agenda-todo-list-sublevels nil)
		   (org-agenda-files org-agenda-files)))
            (todo "READY"
		  ((org-agenda-overriding-header "Ready for Work")
		   (org-agenda-files org-agenda-files)))
            (todo "ACTIVE"
		  ((org-agenda-overriding-header "Active Projects")
		   (org-agenda-files org-agenda-files)))
            (todo "COMPLETED"
		  ((org-agenda-overriding-header "Completed Projects")
		   (org-agenda-files org-agenda-files)))
            (todo "CANC"
		  ((org-agenda-overriding-header "Cancelled Projects")
		   (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
	`(("t" "Tasks / Projects")
          ("tt" "Task" entry (file+olp "~/Projects/Code/emacs-from-scratch/OrgFiles/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ("j" "Journal Entries")
          ("jj" "Journal" entry
           (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
          ("jm" "Meeting" entry
           (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

          ("w" "Workflows")
          ("we" "Checking Email" entry (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

          ("m" "Metrics Capture")
          ("mw" "Weight" table-line (file+headline "~/Projects/Code/emacs-from-scratch/OrgFiles/Metrics.org" "Weight")
           "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
	      (lambda () (interactive) (org-capture nil "jj")))

  (efs/org-font-setup)
  (require 'org-tempo))

;; #+end_src

;; *** Nicer Heading Bullets

;; [[https://github.com/sabof/org-bullets][org-bullets]] replaces the heading stars in =org-mode= buffers with nicer looking characters that you can control.  Another option for this is [[https://github.com/integral-dw/org-superstar-mode][org-superstar-mode]] which we may cover in a later video.

;; #+begin_src emacs-lisp

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; #+end_src

;; *** Center Org Buffers

;; We use [[https://github.com/joostkremers/visual-fill-column][visual-fill-column]] to center =org-mode= buffers for a more pleasing writing experience as it centers the contents of the buffer horizontally to seem more like you are editing a document.  This is really a matter of personal preference so you can remove the block below if you don't like the behavior.

;; #+begin_src emacs-lisp

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;; #+end_src


;; https://lucidmanager.org/productivity/ricing-org-mode/
;; Required for proportional font
(use-package company-posframe
  :config
  (company-posframe-mode 1))

;; Improve org mode looks
(setq org-startup-indented t
      org-startup-with-inline-images t
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-hide-leading-stars t
      org-image-actual-width '(300))

;; Show hidden emphasis markers
(use-package org-appear
  :hook (org-mode . org-appear-mode))

;; https://mstempl.netlify.app/post/beautify-org-mode/
(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "†")
                                       ("#+END_SRC" . "†")
                                       ("#+begin_src" . "†")
                                       ("#+end_src" . "†")
                                       (">=" . "≥")
                                       ("=>" . "⇨")))
(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'org-mode-hook 'prettify-symbols-mode)

;; * Configure Babel Languages

;; To execute or export code in =org-mode= code blocks, you'll need to set up =org-babel-load-languages= for each language you'd like to use.  [[https://orgmode.org/worg/org-contrib/babel/languages.html][This page]] documents all of the languages that you can use with =org-babel=.

;; #+begin_src emacs-lisp

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

;; #+end_src




(eval-when-compile (require 'cl-lib nil t))
(setq byte-compile-warnings '(not cl-functions obsolete))

(leaf rainbow-delimiters
  :ensure t
  :blackout t
  :hook (prog-mode-hook . rainbow-delimiters-mode))


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

(defun options-base-name ()
  (sys-base-name "options" ".el"))

(defun options-file-name ()
  (expand-file-name (locate-user-emacs-file (options-base-name))))

(setq custom-file (options-file-name))
(if (file-exists-p custom-file)
    (load custom-file t nil nil))


(defun ido-base-name ()
  (sys-base-name "ido" ".last"))

(defun ido-file-name ()
  (expand-file-name (locate-user-emacs-file (ido-base-name))))

(setq ido-save-directory-list-file (ido-file-name))


(defun recentf-base-name ()
  (host-base-name "recentf" ".dat"))

(defun recentf-file-name ()
  (expand-file-name (locate-user-emacs-file (recentf-base-name))))

(setq recentf-save-file (recentf-file-name))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf smartparens
  :disabled t
  :ensure t
  :blackout t
  :defun (sp-pair)
  :hook (after-init-hook . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf *keepscratchbuffer
  :preface
  (defun my:make-scratch (&optional arg)
    " *scratch* を作成して buffer-list に放り込む."
    (interactive)
    (progn
      (set-buffer (get-buffer-create "*scratch*"))
      (funcall initial-major-mode)
      (erase-buffer)
      (when (and initial-scratch-message (not inhibit-startup-message))
        (insert initial-scratch-message))
      (or arg
          (progn
            (setq arg 0)
            (switch-to-buffer "*scratch*")))
      (cond ((= arg 0) (message "*scratch* is cleared up."))
            ((= arg 1) (message "another *scratch* is created")))))
  (defun my:buffer-name-list ()
    "buffer 一覧の取得"
    (mapcar (function buffer-name) (buffer-list)))
  ;;
  :hook
  ((kill-buffer-query-functions
    . (lambda ()
        (if (string= "*scratch*" (buffer-name))
            (progn (my:make-scratch 0) nil)
          t)))
   (after-save-hook
    . (lambda ()
        (unless (member "*scratch*" (my:buffer-name-list))
          (my:make-scratch 1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; common lisp family
(show-paren-mode t)

(use-package paredit
  :ensure t
  :commands enable-paredit-mode
  :hook ((lsp-mode
          lsp-interaction-mode
          slime-repl-mode
          eval-expression-minibuffer-setup
          emacs-lisp-mode
          ;;; ielm-mode
          racket-mode
          racket-repl-mode)
         . enable-paredit-mode))

;; (eval-when-compile (require 'cl))

;; (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

;; (add-hook 'lisp-mode-hook 'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
;; (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
;; (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
;; (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
;; ;;; (add-hook 'ielm-mode-hook 'enable-paredit-mode)
;; (add-hook 'racket-mode-hook 'enable-paredit-mode)
;; (add-hook 'racket-repl-mode-hook 'enable-paredit-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; clojure
(leaf clojure-mode :ensure t)
(leaf smartparens :ensure t)
(leaf rainbow-delimiters :ensure t)
;; (leaf aggressive-indent :ensure t)


;; (require 'clojure-mode-extra-font-locking)
(eval-after-load 'clojure-mode
  '(progn
     (add-hook 'clojure-mode-hook 'subword-mode)
     ;;(add-hook 'clojure-mode-hook 'paredit-mode)
     (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
     (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
     ;; (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; common lisp
(eval-after-load 'lisp-mode
  '(progn
     (add-hook 'lisp-mode-hook 'subword-mode)
     (add-hook 'lisp-mode-hook 'paredit-mode)
     ;; (add-hook 'lisp-mode-hook 'smartparens-strict-mode)
     (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
     ;; (add-hook 'lisp-mode-hook 'aggressive-indent-mode)
     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs lisp
(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'subword-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
;;(add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)

(eval-after-load 'ielm-mode
  '(progn
     (add-hook 'ielm-mode-hook 'paredit-but-ielm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gauche scheme
(use-package geiser-gauche
  ;; :init (add-to-list 'geiser-active-implementations 'gauche))
  :after geiser)


(defun gosh-path ()
  (if (eq system-type 'windows-nt)
      "c:/Users/kenjiro/scoop/shims/gosh.exe"
    "/home/linuxbrew/.linuxbrew/bin/gosh"))

(setq geiser-gauche-binary (gosh-path))

;;; racket scheme
;; (use-package geiser-racket
;;   :after geiser
;;   ;; :init (add-to-list 'geiser-active-implementations 'gauche))

(defun racket-path ()
  (if (eq system-type 'windows-nt)
      "c:/Users/kenjiro/scoop/shims/racket.exe"
    "/usr/bin/racket"))


(setq geiser-racket-binary (racket-path))

(eval-after-load "geiser-impl"
  '(add-to-list 'geiser-implementations-alist
		'(((regexp "\\.scm$") guile)
 		  ;;((regexp "\\.ss$") racket)
 		  ;;((regexp "\\.rkt$") racket)
		  )))
;;		(list ((dir "/home/kenjiro/Nextcloud/lisp/guile") guile)
;;
;;
;;		      ((dir "/home/kenjiro/Nextcloud/lisp/gauche") gauche)
;;		      ((dir "/home/kenjiro/Nextcloud/lisp/racket") racket)		
;;		      )))

;; Racket
;; racket-mode にて、拡張子判定が行われても、scheme-mode が優先されてしまうので
;; 予め、除外しておく
(setq auto-mode-alist (cl-remove-if
		                   (lambda (e)
			                   (equal "\\.rkt\\'" (car e)))
		                   auto-mode-alist))
(use-package racket-mode
  :ensure t)

(eval-after-load 'racket-mode
  '(progn
     (add-hook 'racket-mode-hook 'racket-xp-mode)
     (add-hook 'racket-mode-hook 'subword-mode)
     ;;(add-hook 'racket-mode-hook 'paredit-mode)
     (add-hook 'racket-mode-hook 'smartparens-strict-mode)
     (add-hook 'racket-mode-hook 'rainbow-delimiters-mode)
     ;;(add-hook 'racket-mode-hook 'aggressive-indent-mode)
     ))


(use-package pollen-mode :ensure t)
(use-package company-pollen :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ido)
(ido-mode t)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package slime
  :if (file-exists-p (expand-file-name "~/.roswell/helper.el"))
  :ensure slime-company
  :init (load (expand-file-name "~/.roswell/helper.el"))
  :custom (inferior-lisp-program "ros -Q run ")
  :config (slime-setup '(slime-fancy slime-company)))

(defun my-slime-sync-repl ()
  "現在のバッファのパッケージに移動してからREPLに移行"
  (slime-sync-package-and-default-directory)
  (slime-switch-to-output-buffer))

(global-set-key (kbd "C-c C-f") 'slime-sync-package-and-default-directory)


;;(setq inferior-lisp-program "clisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package expand-region
  :ensure t)
(global-set-key (kbd "C-q") 'er/expand-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defun x-clipboard-copy ()
  (interactive)
  (when (region-active-p)
    (shell-command-on-region (region-beginning) (region-end) "xsel -ib" nil nil)))

(use-package htmlize
  :ensure t)

(use-package lsp-mode
  :ensure t
  :hook ((ruby-mode . lsp)
         (js-mode . lsp)))


;; pin to MELPA Stable
(use-package cider
  :ensure t
  :pin melpa)

(use-package magit
  :ensure t
  :pin melpa)

;; https://agel.readthedocs.io/en/latest/index.html
(use-package ag
  :ensure t)

(use-package ccls
  :ensure t)

(defun ccls-path ()
  (if (eq system-type 'windows-nt)
      "c:/ProgramData/chocolatey/bin/ccls.exe"
    "/snap/bin/ccls"))


(setq ccls-executable (ccls-path))
(add-hook 'objc-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'c-mode-hook #'lsp)

(use-package flyspell
  :ensure t)


;; modula-2
;; https://splendidisolation.ddns.net/Southwales/gaius/web/gm2-mode.html
;; always use auto-fill-mode when in text mode
;;   (stops emacs from line wrapping in makefiles !)
(add-hook 'text-mode-hook
          '(lambda () (auto-fill-mode 1)))

;; auto load definitions
(defun prepend-to-auto-mode-alist (pair)
  "Add PAIR onto start of auto-mode-alist"
  (setq auto-mode-alist (cons pair auto-mode-alist)))

(prepend-to-auto-mode-alist '("\\.mod$" . gm2-mode))
(prepend-to-auto-mode-alist '("\\.def$" . gm2-mode))
(autoload 'gm2-mode "gm2-mode" "GNU Modula-2 mode")

;; calma GPL
(prepend-to-auto-mode-alist '("\\.GS$" . cgpl-mode))
(autoload 'cgpl-mode "cgpl-mode" "Calma GPL mode")


(prepend-to-auto-mode-alist '("\\.p$" . mpascal-mode))
(autoload 'mpascal "mpascal-mode" "Mac Pascal mode")


(setq make-backup-files nil)

(use-package cmake-mode
  :ensure t)
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))


(use-package astyle
  :ensure t
  :when (executable-find "astyle"))
;; :hook (c-mode-common . astyle-on-save-mode)

;; (setq c-basic-offset 2)
;; (setq c-default-style "bsd")



(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom (python-shell-interpretter "python"))

(use-package py-autopep8
  :ensure t
  :hook (python-mode . py-autopep8-mode))

(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode))

(use-package flycheck-inline
  :ensure t)

(use-package filetree
  :ensure t)

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

;;; opendylan
(if (file-directory-p "/home/kenjiro/opendylan-2002.1")
    (progn 
      (require 'dime)
      (dime-setup '(dime-repl dime-note-tree))
      (setq dime-dylan-implementations
	    '((opendylan ("/home/kenjiro/opendylan-2022.1/bin/dswank")
			 :env ("OPEN_DYLAN_USER_REGISTRIES=/home/kenjiro/opendylan-2022.1/sources/registry"))
	      ))))
(use-package sudo-edit
  :ensure t
  :pin melpa)

(c-add-style "gnustep" '("gnu" (c-basic-offset . 2)))
(c-add-style "bsd4" '("bsd" (c-basic-offset . 4)))
(setq c-default-style '((objc-mode . "gnustep")
			(other . "bsd4")))

;; (setq auto-mode-alist
;;       (append
;;        '(("\\.pas\\'" . opascal-mode)
;; 	 ("\\.pp\\'" . opascal-mode)
;; 	 ("\\.inc\\'" . opascal-mode)
;; 	 )
;;        auto-mode-alist))


(setq gdb-many-windows t)

(defun org-org-html--format-image (source attributes info)
  (format "<img src=\"data:image/%s+xml;base64,%s\"%s />"
	  (or (file-name-extension source) "")
	  (base64-encode-string
	   (with-temp-buffer
             (insert-file-contents-literally source)
             (buffer-string)))
	  (file-name-nondirectory source)))
(advice-add #'org-html--format-image :override #'org-org-html--format-image)

(setq org-src-tab-acts-natively t)

;;; @see
;;; [https://github.com/mfoo/dotfiles/blob/master/.emacs.d/config.org]

(defun kf/inseret-notranslate-script (template info)
  "Append notranslate effect script after body"
  (let ((html-template (org-html-template template info)))
    (let ((pos (string-match (regexp-quote "</body>") html-template)))
      (concat (substring html-template 0 pos)
	      "<script>
  document.querySelectorAll('pre').forEach((el) => {
  if (!el.hasAttribute('translate')) {
    el.setAttribute('translate', 'no');
  }
});
</script>"
        (substring html-template pos)))))

(eval-after-load "org"
  '(progn
     (require 'ox-html)
     (org-export-define-derived-backend 'kf/inseret-notranslate-script 'html
       :translate-alist
       '((template . kf/inseret-notranslate-script)))))

(eval-after-load "org"
  '(require 'ox-gfm nil t))

(setq-default tab-width 2)

(load-file (expand-file-name "~/.emacs.d/lisp/mpascal.el"))
(load-file (expand-file-name "~/.emacs.d/lisp/cgpl-mode.el"))

(use-package yasnippet
  :ensure t
  :hook ((lsp-mode . yas-minor-mode)))

(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp-deferred))

(use-package flycheck-rust
  :ensure t)

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

;;; Javascript
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'js-mode)
(flycheck-add-mode 'javascript-eslint 'javascript-mode)

(setq-default indent-tabs-mode nil)
(add-hook 'js-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq tab-width 2)
            (setq js-indent-level 2)))

(use-package doom-modeline
  :ensure t
  :when (display-graphic-p)
  :hook (after-init . doom-modeline-mode)
	;;  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install t)
  (global-treesit-auto-mode))

(use-package transpose-frame
  :ensure t)

(use-package clang-format
  :ensure t)

;; https://github.com/seudut/perspeen
;; http://emacs.rubikitch.com/perspeen/
(use-package perspeen
  :ensure t
  :init
  (setq perspeen-use-tab t)
  :config
  (perspeen-mode))

(define-key perspeen-command-map (kbd "h") 'perspeen-tab-prev)
(define-key perspeen-command-map (kbd "l") 'perspeen-tab-next)
(define-key perspeen-command-map (kbd "C-d") 'perspeen-tab-del)
(global-set-key (kbd "<C-tab>") 'perspeen-tab-next)
(global-set-key (kbd "<C-S-tab>") 'perspeen-tab-pre)

(use-package git-gutter
  :init
  (progn
    (global-git-gutter-mode t)
    (git-gutter:linum-setup))
  :bind
  (("C-x C-g" . git-gutter:toggle))
  :config
  (progn
    (custom-set-variables
     '(git-gutter:modified-sign "  ")
     '(git-gutter:added-sign "++")
     '(git-gutter:deleted-sign "--"))
    (set-face-background 'git-gutter:modified "purple")
    (set-face-foreground 'git-gutter:added "green")
    (set-face-foreground 'git-gutter:deleted "red")))



;;; GDB 関連
;;; 有用なバッファを開くモード
(setq gdb-many-windows t)

;;; 変数の上にマウスカーソルを置くと値を表示
(add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))

;;; I/O バッファを表示
(setq gdb-use-separate-io-buffer t)

;;; t にすると mini buffer に値が表示される
(setq gud-tooltip-echo-area nil)

(use-package dap-mode
  :ensure t)

(require 'dap-lldb)
(require 'dap-gdb-lldb)

;; Enabling only some features
(setq dap-auto-configure-features '(sessions locals controls tooltip))
(dap-mode 1)

;; The modes below are optional

(dap-ui-mode 1)
;; enables mouse hover support
(dap-tooltip-mode 1)
;; use tooltips for mouse hover
;; if it is not enabled `dap-mode' will use the minibuffer.
(tooltip-mode 1)
;; displays floating panel with debug buttons
;; requies emacs 26+
(dap-ui-controls-mode 1)

;; -*- lexical-binding: t -*-
(define-minor-mode +dap-running-session-mode
  "A mode for adding keybindings to running sessions"
  nil
  nil
  (make-sparse-keymap)
  (evil-normalize-keymaps) ;; if you use evil, this is necessary to update the keymaps
  ;; The following code adds to the dap-terminated-hook
  ;; so that this minor mode will be deactivated when the debugger finishes
  (when +dap-running-session-mode
    (let ((session-at-creation (dap--cur-active-session-or-die)))
      (add-hook 'dap-terminated-hook
                (lambda (session)
                  (when (eq session session-at-creation)
                    (+dap-running-session-mode -1)))))))

;; Activate this minor mode when dap is initialized
(add-hook 'dap-session-created-hook '+dap-running-session-mode)

;; Activate this minor mode when hitting a breakpoint in another file
(add-hook 'dap-stopped-hook '+dap-running-session-mode)

;; Activate this minor mode when stepping into code in another file
(add-hook 'dap-stack-frame-changed-hook (lambda (session)
                                          (when (dap--session-running session)
                                            (+dap-running-session-mode 1))))


(require 'kf-command)

(provide 'init)
;;; init.el ends here
