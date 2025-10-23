;;; early-init.el --- Early Initialization. -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Emacs 27+ introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;
;;; Code:
(setq package-enable-at-startup nil)
;;; @see https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
(setenv "LSP_USE_PLISTS" "true")
(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
