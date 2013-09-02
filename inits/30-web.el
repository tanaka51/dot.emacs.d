;; mode
(add-to-list 'auto-mode-alist '("\\.erb$"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml$"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"  . web-mode))

(lazyload "web-mode"
          (setq web-mode-tag-autocomplete-style 2))

(lazyload "scss-mode"
          (setq scss-compile-at-save nil))

(lazyload "coffee-mode"
          (coffee-tab-width 2))
