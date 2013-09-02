;; open-junk-file
(require 'open-junk-file)
(setq open-junk-file-format "~/junk/%Y/%m/%d/%H%M%S.")
(setq open-junk-file-find-file-function 'find-file)
(global-set-key (kbd "C-x j") 'open-junk-file)

;; recentf
(setq recentf-max-saved-items 500)
(setq recentf-exclude '("/TAGS$"))

;; undo-tree
(setq undo-limit 600000)
(setq undo-strong-limit 900000)
(global-undo-tree-mode)

;; migemo
(setq migemo-command "/usr/local/bin/cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(setq migemo-regex-dictionary nil)
(load-library "migemo")
(migemo-init)

;; direx
(setq direx:leaf-icon "  "
      direx:open-icon "v "
      direx:closed-icon "> ")
(global-set-key (kbd "C-x C-j") 'direx-project:jump-to-project-root-other-window)
(global-set-key (kbd "C-x M-j") 'direx:jump-to-directory-other-window)

;; yascroll
(scroll-bar-mode -1)
(global-yascroll-bar-mode 1)

;; git-gutter
(global-git-gutter-mode t)
