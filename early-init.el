;;; early-init.el --- Early Initialization. -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Emacs 27+ introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;
;;; Code:

;; GUID: sd550a2b-4c5d-4e5f-b6b7-c8d9e0f1a2c4
;; ========================================================================
;; システム側 site-start（50emacs-mozc等）の一括ロードを物理的に完全強制遮断
;; ========================================================================
;; /etc/emacs/site-start.d/ 配下の古いスクリプトが及ぼす変数衝突エラーと
;; 起動遅延を根本から完全にパージするため、システム初期化ファイルの読み込み自体を抑制します。
(setq site-run-file nil)

(setq package-enable-at-startup nil)

;;; @see https://github.io
(setenv "LSP_USE_PLISTS" "true")

(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
