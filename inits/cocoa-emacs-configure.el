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
