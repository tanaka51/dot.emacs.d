;; mode
(add-to-list 'auto-mode-alist '("\\.rake$"   . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$"   . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))

(lazyload "ruby-mode"
          (setq ruby-end-insert-newline nil)
          (setq ruby-deep-indent-paren-style nil)
          (setq ruby-block-highlight-toggle t)
          (require 'ruby-end)
          (require 'ruby-block)
          (ruby-block-mode t)

          ;; Use last-command-event instead of last-command-char
          (defun ruby-electric-brace (arg)
            (interactive "P")
            (insert-char last-command-event 1)
            (ruby-indent-line t)
            (delete-char -1)
            (self-insert-command (prefix-numeric-value arg))))

(add-hook 'ruby-mode-hook
          (lambda ()
            (electric-pair-mode t)
            (electric-indent-mode t)
            (electric-layout-mode t)))
