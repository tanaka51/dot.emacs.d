(setq user-full-name "Koichi TANAKA")

;; package
(require 'package)
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; theme
(load-theme `wombat t)

;; requires
(require 'popwin)
(require 'open-junk-file)
(require 'direx)
(require 'direx-project)
(require 'yascroll)
(require 'redo+)
(require 'web-mode)
(require 'helm-ls-git)
(require 'ruby-end)
(require 'ruby-block)
(require 'migemo)

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

(setq display-buffer-function 'popwin:display-buffer)
(setq open-junk-file-format "~/junk/%Y/%m/%d/%H%M%S.")
(setq open-junk-file-find-file-function 'find-file)
(setq recentf-max-saved-items 500)
(setq recentf-exclude '("/TAGS$"))
(setq undo-limit 600000)
(setq undo-strong-limit 900000)
(setq helm-split-window-default-side 'below)
(setq ruby-block-highlight-toggle t)
(setq direx:leaf-icon "  "
      direx:open-icon "v "
      direx:closed-icon "> ")
(setq web-mode-tag-autocomplete-style 2)
(setq ruby-end-insert-newline nil)
(setq ruby-deep-indent-paren-style nil)

;; global
(global-auto-revert-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode t)
(delete-selection-mode t)
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(set-frame-height (next-frame) 55)
(set-frame-width (next-frame) 100)

(global-yascroll-bar-mode 1)
(electric-pair-mode t)
(ruby-block-mode t)
(yas-global-mode 1)

;; key-binds
(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "M-r") 'revert-buffer)
(global-set-key (kbd "M-+") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "M-\-") (lambda () (interactive) (text-scale-decrease 1)))
(global-set-key (kbd "C-M-k") 'windmove-up)
(global-set-key (kbd "C-M-j") 'windmove-down)
(global-set-key (kbd "C-M-l") 'windmove-right)
(global-set-key (kbd "C-M-h") 'windmove-left)
(global-set-key (kbd "C-x j") 'open-junk-file)
(global-set-key (kbd "C-M-/") 'redo)
(global-set-key (kbd "C-x C-j") 'direx-project:jump-to-project-root-other-window)
(global-set-key (kbd "C-x M-j") 'direx:jump-to-directory-other-window)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-;") 'helm-mini)
(global-set-key (kbd "M-i") 'helm-imenu)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-t") 'helm-ls-git-ls)

(define-key web-mode-map (kbd "C-;") 'helm-mini)

;; modes
(add-to-list 'auto-mode-alist '("\\.erb$"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml$"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.rake$"   . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$"   . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))

;; ruby-mode
(add-hook 'ruby-mode-hook
          (lambda ()
            (electric-indent-mode t)
            (electric-layout-mode t)))

;; popwin
(push '("^\*helm .+\*$" :regexp t :height 30)
      popwin:special-display-config)
(push '(direx:direx-mode :position left :width 60 :dedicated t)
      popwin:special-display-config)

;; font by @igaiga
(let* ((size 12) ; ASCII font size [9/10/12/14/15/17/19/20/...]
       (asciifont "Ricty") ; ASCII font
       (jpfont "Ricty") ; Japanese font
       (h (* size 12))
       (fontspec (font-spec :family asciifont))
       (jp-fontspec (font-spec :family jpfont)))
  (set-face-attribute 'default nil :family asciifont :height h)
  (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
  (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
  (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
  (set-fontset-font nil '(#x0370 . #x03FF) fontspec))

;; Migemo
(setq migemo-command "/usr/local/bin/cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(setq migemo-regex-dictionary nil)
(load-library "migemo")
(migemo-init)


;; OS
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

  (global-set-key (kbd "C-M-_") 'indent-region)
  (global-set-key (kbd "C-^") 'macdict-lookup-word)

  ;; command key -> Meta key
  (setq ns-command-modifier (quote meta))
  (setq ns-alternate-modifier (quote super)))

 ;; Linux
 ((string-match "linux" system-configuration))

 ;; Windows
 ((string-match "mingw" system-configuration)))
