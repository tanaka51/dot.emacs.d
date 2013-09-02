(setq user-full-name "Koichi TANAKA")

;; package
(require 'package)
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

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
(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode t)
(delete-selection-mode t)
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(electric-pair-mode t)

;; key-binds
(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "M-r") 'revert-buffer)
(global-set-key (kbd "M-+") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "M-\-") (lambda () (interactive) (text-scale-decrease 1)))
(global-set-key (kbd "C-M-k") 'windmove-up)
(global-set-key (kbd "C-M-j") 'windmove-down)
(global-set-key (kbd "C-M-l") 'windmove-right)
