(defmacro lazyload (lib &rest body)
  `(when (locate-library ,lib)
     (eval-after-load ,lib
       '(progn
          ,@body))))
