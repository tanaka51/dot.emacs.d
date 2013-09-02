;; 必要なライブラリを読み込んだ後が良いらしい

;; popwin
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(push '("^\*helm .+\*$" :regexp t :height 30)
      popwin:special-display-config)
(push '(direx:direx-mode :position left :width 60 :dedicated t)
      popwin:special-display-config)
