(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

(require 'package)
(setq
  package-archives
  '(("gnu" . "http://elpa.gnu.org/packages/")
    ("org" . "http://orgmode.org/elpa/")
    ("melpa" . "http://melpa.org/packages/")
    ("melpa-stable" . "http://stable.melpa.org/packages/"))
  ;; prefer stable unless explicit pin on melpa
  package-archive-priorities
      '(("melpa-stable" . 20)
        ("org" . 20)
        ("gnu" . 10)
        ("melpa" . 5)))
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((org-plus-contrib . "org"))))
(package-initialize)

(unless (or (package-installed-p 'use-package)
            (package-installed-p 'diminish)
            (package-installed-p 'benchmark-init))
  (package-refresh-contents)
  (package-install 'benchmark-init)
  (package-install 'use-package)
  (package-install 'diminish))

(eval-when-compile
  (require 'use-package))
(require 'benchmark-init)
(require 'diminish)
(require 'bind-key)

(org-babel-load-file (concat user-emacs-directory "config.org"))
