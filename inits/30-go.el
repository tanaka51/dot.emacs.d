;; golang
(lazyload "go-mode"
          (setq c-basic-offset 4)
          (setq indent-tabs-mode t)
          (require 'go-flymake)
          ;; move to global if use flymake other language mode
          (defun my-flymake-display-err-minibuf-for-current-line ()
            "Displays the error/warning for the current line in the minibuffer"
            (interactive)
            (let* ((line-no            (flymake-current-line-no))
                   (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info line-no)))
                   (count              (length line-err-info-list)))
              (while (> count 0)
                (when line-err-info-list
                  (let* ((text       (flymake-ler-text (nth (1- count) line-err-info-list)))
                         (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
                    (message "[%s] %s" line text)))
                (setq count (1- count))))))
