;; todo - smarter early-init
;;(push (concat user-emacs-directory "lisp") load-path)
(if (and (fboundp 'native-comp-available-p) (native-comp-available-p))
    (progn (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))))
;;(load-library "config")
;; todo - handle nil
(native-elisp-load (comp-lookup-eln "lisp/config.el"))
;;(require 'config)
;;(native-elisp-load "config.elc")

