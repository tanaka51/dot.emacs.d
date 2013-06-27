;; migemo
(bundle! emacs-jp/migemo
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))

  ;; Set your installed path
  (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")

  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init))

;; helm
(bundle helm
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-;") #'helm-mini)
  (global-set-key (kbd "M-i") #'helm-imenu)
  (global-set-key (kbd "M-y") #'helm-show-kill-ring))

;; git-gutter
(bundle git-gutter)

;; open-junk-file
(bundle open-junk-file
  (global-set-key (kbd "C-x j") #'helm-show-kill-ring))


;;;
;;; launguages
;;;

;; ruby
(bundle ruby-mode
  (add-to-list 'auto-mode-alist '("\\.rake$"   . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile$"   . ruby-mode))
  (add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))

  (setq ruby-block-highlight-toggle t)
  (setq ruby-end-insert-newline nil)
  (setq ruby-deep-indent-paren-style nil))

;; haml
(bundle haml-mode)

;; coffee
(bundle coffee-mode)

;; scss
(bundle scss-mode
  (setq scss-compile-at-save nil))

;; yaml
(bundle yaml-mode)

;; web-mode
(bundle! web-mode
  (add-to-list 'auto-mode-alist '("\\.erb$"    . web-mode))
  (add-to-list 'auto-mode-alist '("\\.rhtml$"  . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?$"  . web-mode)))
