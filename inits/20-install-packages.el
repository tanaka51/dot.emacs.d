;; http://kei10in.hatenablog.jp/entry/2012/09/12/220833

(require 'cl)

(defvar installing-package-list
  '(
    coffee-mode
    direx
    git-gutter
    haml-mode
    helm
    helm-c-moccur
    helm-ls-git
    helm-migemo
    less-css-mode
    migemo
    open-junk-file
    popwin
    recentf-ext
    ruby-block
    ruby-end
    ruby-mode
    rvm
    scss-mode
    undo-tree
    web-mode
    yaml-mode
    yascroll
    ))

(let ((not-installed (loop for x in installing-package-list
                           when (not (package-installed-p x))
                           collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))
