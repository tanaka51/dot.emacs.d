(setq user-full-name "Koichi Tanaka")

;; theme
(load-theme `wombat t)

;; variables
(setq-default ring-bell-function 'ignore)
(setq-default inhibit-startup-message t)
(setq-default inhibit-splash-screen t)
(setq-default make-backup-files nil)
(setq-default auto-save-default nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default windmove-wrap-around t)
(setq-default show-trailing-whitespace t)
(setq-default completion-ignore-case t)

;; global
(global-auto-revert-mode t)
(menu-bar-mode -1)
(show-paren-mode t)
(delete-selection-mode t)
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

;; key-binds
(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "M-r") 'revert-buffer)
(global-set-key (kbd "M-+") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "M-\-") (lambda () (interactive) (text-scale-decrease 1)))
(global-set-key (kbd "C-M-k") 'windmove-up)
(global-set-key (kbd "C-M-j") 'windmove-down)
(global-set-key (kbd "C-M-l") 'windmove-right)
(global-set-key (kbd "C-M-h") 'windmove-left)
(global-set-key (kbd "C-M-_") 'indent-region) ; For Mac keybord

(cond
 ;; Mac
 ((string-match "apple-darwin" system-configuration)

  ;; Dictionary
  (require 'thingatpt)
  (defun macdict-lookup (word)
    "Lookup word with Dictionary.app"
    (call-process "open" nil 0 nil (concat "dict://" word)))

  (defun macdict-lookup-word ()
    "Lookup the word at point with Dictionary.app."
    (interactive)
    (macdict-lookup (word-at-point)))

  (global-set-key (kbd "C-^") 'macdict-lookup-word)

  ;; command key -> Meta key
  (setq ns-command-modifier (quote meta))
  (setq ns-alternate-modifier (quote super)))

 ;; Linux
 ((string-match "linux" system-configuration))

 ;; Windows
 ((string-match "mingw" system-configuration)))
