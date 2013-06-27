;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/config/el-get/recipes")

;; bundle
(add-to-list 'el-get-sources
             '(:name bundle :type github :pkgname "tarao/bundle-el"))
(el-get 'sync 'bundle)


;; after (bundle foo), do (load "config/bundle/foo") if config/bundle/foo exists.
;; (defadvice bundle (after load-bundled-config)
;;   (let ((target-path (concat "~/.emacs.d/config/bundle/" (format "%s" (ad-get-arg 0)))))
;;     (if (file-exists-p target-path) (load target-path))))

;;
;;
;; LOAD ELFILE
;;
;;
(load "Elfile")
