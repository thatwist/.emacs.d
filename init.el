;;; package --- Summary
;;; Code:
;;; Commentary:

(setq user-full-name "Yurii Ostapchuk"
      user-mail-address "twist522@gmail.com")

;; test
;;(toggle-debug-on-error)

;; global variables
(setq
  create-lockfiles nil
  make-backup-files nil
  column-number-mode t
  scroll-error-top-bottom t
  ;;show-paren-delay 0.5
  sentence-end-double-space nil
  browse-url-browser-function 'browse-url-default-browser)

;; buffer local variables
(setq-default
  indent-tabs-mode nil
  tab-width 4
  c-basic-offset 4)

;; for easier integration with Dropbox/org paths
(when (eq system-type 'window-nt)
  (setenv "HOME" "C:\\Users\\Admin"))

(when (not (eq system-type 'windows-nt))
  (progn
    ; using default with mu4e
    ;(setq message-directory "~/.config/emacs/mail/")
    (setq gnus-directory "~/.config/emacs/news/")))

;; Startup time
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time))) gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time) 

;; Transparency - testing, works only in windows, not in i3
(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;; fonts
;; set default
(if (eq system-type 'windows-nt)
  (set-face-attribute 'default nil :font "Fira Mono" :height 95) ;; defaults to 139
  ; this doesn't work
  ;(set-face-attribute 'default t :font "Input Mono Narrow" :height 100)
  ;(set-face-attribute 'default t :font "Source Code Pro-10")
  ;; equivalent (this works)
  (add-to-list 'default-frame-alist '(font . "Input Mono Narrow-9.5"))
  ;(add-to-list 'default-frame-alist '(font . "Source Code Pro-10"))
  )

;; to set for current frame and future frames (works instantly)
;(set-face-attribute 'default nil :font "Input Mono Narrow" :height 95)
;;(set-face-attribute 'default nil :font "Source Code Pro" :height 150) ;; defaults to 139
;;(set-face-attribute 'default nil :font "Source Code Pro Medium")
;; equivalent of
;;(set-frame-font "Source Code Pro Medium" nil t)

;; or use M-x menu-set-font, or use M-x set-frame-font

;; testing
;;(set-fontset-font t 'latin "Noto Sans")

;; something for icons?
(setq inhibit-compacting-font-caches t)

;; modes
;;(electric-indent-mode 0)
;; omg how could I live without this - to remove selection (if active) when inserting text
(delete-selection-mode 1)
(menu-bar-mode -1)
(fringe-mode 25)
(set-scroll-bar-mode nil)
(tool-bar-mode -1)
(save-place-mode 1) ; remember file position in the visited previously file
 ;;; highlight current line
(global-hl-line-mode 1)
;;(set-face-background hl-line-face "gray87")

;; experimenting with this
(auto-insert-mode t)

;; input method
(setq default-input-method "ukrainian-computer")

(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'text-mode-hook #'flyspell-mode)

(when (eq system-type 'windows-nt)
    (setq ispell-dictionary "en_US")
    (setq ispell-hunspell-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8))))

;(set-language-environment "UTF-8")
;(set-default-coding-systems 'utf-8)

;(load-file "/usr/share/festival/festival.el")
;(autoload 'say-minor-mode "festival" "Menu for using Festival." t)
;(say-minor-mode t)

(require 'desktop)
(setq desktop-load-locked-desktop t) ; do not ask that lock-file exists, this fixes the issue with emacs daemon waiting for answer
;; actually it's better to have everything you need opened in a few keystrokes than keep buffers around for ages, and it's muuuch faster to init
;;(desktop-save-mode 1)

;; this will prevent asking when visiting git-controlled symlink
(setq vc-follow-symlinks t)

(global-unset-key (kbd "C-z"))

;; redefine mouse-2 ?
;;(define-key key-translation-map (kbd "<s-mouse-1>") (kbd "<mouse-2>"))

;; define binding lookup for init.el
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

;; define binding for init.el
;;(global-set-key (kbd "C-c I") 'find-user-init-file)

;; for mac
;;(setq mac-option-modifier 'meta)
;;(setq mac-command-modifier 'super)

;; local lisp files
(push (concat user-emacs-directory "lisp") load-path)

;; package manager
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

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Enable ensure by default for use-package
;; Keep auto-save/backup files separate from source code
(setq
  use-package-always-ensure t
  use-package-always-defer t
  use-package-verbose t
  auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "auto-save/") t))
  backup-directory-alist `(("." . ,(expand-file-name (concat user-emacs-directory "backups"))))
)

;; quelpa
(use-package quelpa :demand)
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)
;; to install some packages with quelpa but use use-package-always-ensure to install all others from an ELPA repo :ensure needs to be disabled if the :quelpa keyword is found
(quelpa-use-package-activate-advice)


(use-package bug-hunter)

;; experimental - minibuffer within minibuffer
(setq enable-recursive-minibuffers t)

;; EasyPG encryption
(require 'epa-file)
(epa-file-enable)

;; used for prompts on gpg - if pinentry program = emacs
(use-package pinentry)
;; This should force Emacs to use its own internal password prompt instead of an external pin entry program
(setenv "GPG_AGENT_INFO" nil)

;; password store
(use-package password-store
  :config (setq password-store-executable (executable-find "pass.bat")))
;; this one is better
(use-package pass)

(require 'auth-source-pass)
(auth-source-pass-enable)
;; was used until auth-source-pass came
;;(setq auth-sources '("~/.authinfo.gpg" "~/.netrc"))

;; close buffers which will ask for user input on the next start and prevent emacs-server to start through systemctl
(add-hook 'kill-emacs-hook (lambda()
                             (save-some-buffers t)
                             (kill-matching-buffers ".*.gpg" nil t)
                             (kill-matching-buffers "ejc-sql-editor" nil t) ;; this may ask for authinfo on next load
                             ))

;; to sudo-edit files
(use-package sudo-edit)

;; clipboard share with x11
(if (eq system-type 'windows-nt)
  nil
  (use-package xclip
    :demand
    :config (xclip-mode 1)))

;; edit server for chrome plugin
(use-package edit-server
  :ensure t
  :commands edit-server-start
  :init (if after-init-time
              (edit-server-start)
            (add-hook 'after-init-hook
                      #'(lambda() (edit-server-start))))
  :config (setq edit-server-new-frame-alist
                '((name . "Edit with Emacs FRAME")
                  (top . 200)
                  (left . 200)
                  (width . 80)
                  (height . 25)
                  (minibuffer . t)
                  (menu-bar-lines . t)
                  (window-system . x))))

;; scroll one line at a time (less "jumpy" than defaults)
;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)((meta)) ((control) . text-scale))) ;; one line at a time
;(setq mouse-wheel-progressive-speed t);;nil ;; (not) accelerate scrolling
;(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;(setq scroll-step 1) ;; keyboard scroll one line at a time

; finally!
(use-package good-scroll
  :demand
  :config
  (global-set-key [next] #'good-scroll-up-full-screen)
  (global-set-key [prior] #'good-scroll-down-full-screen)
  (good-scroll-mode 1))


;; multiple problems with this package: 1. no font size change. 2. line separator ^L problem (page-break-lines)
;;(use-package display-line-numbers :custom (global-display-line-numbers-mode t))

(use-package linum
  ;:custom (global-linum-mode t)
  :config
  (require 'page-break-lines)
  :hook (prog-mode . linum-mode))

(use-package page-break-lines
  ;:after linum-mode
  :config
  (global-page-break-lines-mode)
  ;; unused already - fix width of line (there was a problem with global-display-line-number but not linum)
  ;(set-fontset-font "fontset-default"
  ;                (cons page-break-lines-char page-break-lines-char)
  ;                (face-attribute 'default :family))
)

;; zoom
;(use-package zoom-frm :quelpa (zoom-frm :fetcher wiki))
(quelpa '(frame-fns :repo "frame-fns.el" :fetcher wiki))
(quelpa '(frame-cmds :repo "frame-cmds.el" :fetcher wiki))
(quelpa '(zoom-frm :repo "zoom-frm.el" :fetcher wiki))
(require 'zoom-frm)

;; this does not work, need something else (the walkaround is to delete other frames)
;; do not kill frame if quit last window
;;(setq frame-auto-hide-function 'ignore)

;;(use-package beacon
;;  :custom
;;  (beacon-color "#f1fa8c")
;;  :hook (after-init . beacon-mode))
  

;;;;;;; DASHBOARD ;;;;;;;;;
(use-package dashboard
  :after all-the-icons elfeed-dashboard
  :demand
  :preface
  (defun dashboard-performance-statement (list-size)
    (insert (all-the-icons-faicon "check" :height 1.2 :v-adjust 0.0 :face 'font-lock-keyword-face))
    (insert (propertize " Think" 'face 'dashboard-heading))
    (insert (propertize "\n\t★ SLEEP\n\t★ ROUTINE\n\t★ NUTRITION\n\t★ SPORT\n\t★ REST" 'face '(:height 110))))
  :custom
  (dashboard-banner-logo-title "With Great Power Comes Great Responsibility")
  (dashboard-startup-banner 'official) ;; 1,2,3,'logo,'official
  (dashboard-center-content t)
  (dashboard-items '((performance)
                     (elfeed . 10)
                     ;;(agenda . 5)
                     ;;(recents  . 5)
                     ;;(projects . 5)
                     ;;(bookmarks . 5)
                     ;;(registers . 5)
                     ))
  (dashboard-set-file-icons t)
  (dashboard-set-heading-icons t)
  (dashboard-set-init-info t)
  (dashboard-set-navigator t)
  :config
  (require 'dashboard-elfeed)
  (require 'elfeed-dashboard)
  (setq de/key "b")
  (setq de/dashboard-search-filter "")
  (add-to-list 'dashboard-item-generators '(elfeed . dashboard-elfeed))
  ;(add-to-list 'dashboard-items '(elfeed) t)

  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (add-to-list 'dashboard-item-generators '(performance . dashboard-performance-statement))
  (elfeed-dashboard-update)
  (dashboard-setup-startup-hook)
  )
;;;;;;;;;;;;;;

;;; CSV-MODE ;;;
(use-package csv-mode)
;;;;;;;;;;;;;;;;

(use-package ace-window
  :ensure t
  :defer t
  :init
  (progn
    (global-set-key (kbd "M-p") 'ace-window)
    ))

;; show indents in all modes
(use-package indent-guide
  :hook (prog-mode . indent-guide-mode))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; navigate snakeCase
(add-hook 'prog-mode-hook #'subword-mode)

(use-package whitespace
  :custom
  (whitespace-line-column 170) ;; limit line length
  (whitespace-style
        '(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark))
  :hook (prog-mode . whitespace-mode))

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  :bind ("C-c h" . highlight-symbol))


(add-to-list 'auto-mode-alist '("\\.avsc$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.hql$" . sql-mode))

;;;;;;; SMARTPARENS ;;;;;;;;
; if M-<backspace> annoys - see this - https://github.com/Fuco1/smartparens/pull/861/files
(use-package smartparens
  :diminish smartparens-mode
  :demand t
  :config
  (require 'smartparens-config)
  (require 'smartparens-scala)
  (sp-use-smartparens-bindings)
  (smartparens-global-mode 1)
  ;; to be protected from introducing unbalanced pairs by editing commands which delete regions, what you want is smartparens-strict-mode
  ;;(smartparens-strict-mode)
  (sp-pair "(" ")" :wrap "C-(") ;; how do people live without this?
  (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
  (sp-pair "{" "}" :wrap "C-{")
  (bind-key "C-S-<left>" nil smartparens-mode-map)
  (bind-key "C-S-<right>" nil smartparens-mode-map)
  (bind-key "s-<delete>" 'sp-kill-sexp smartparens-mode-map)
  (bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map)
  (bind-key "s-{" 'sp-rewrap-sexp smartparens-mode-map)
  ;; unbind the annoying one
  (unbind-key "M-<backspace>" smartparens-mode-map)
  (add-hook 'prog-mode-hook #'show-smartparens-mode))

(use-package evil-smartparens
  :hook (emacs-lisp . evil-smartparens-mode))

;; string manipulation (not really using directly)
(use-package s)

(use-package expand-region :commands 'er/expand-region)

;;; FLYCHECK ;;;;;
(use-package flycheck
  :ensure t
  :pin melpa ;; need 32 version for lsp-mode+flycheck :sigh:
  :init (global-flycheck-mode)
  :custom (flycheck-global-modes '(not org-mode)))

(use-package projectile
  :init   (setq projectile-use-git-grep t)
  :config
  (require 'counsel-projectile)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-project-search-path (list "~/Documents"))
)

(use-package counsel-projectile
  :after projectile counsel
  :config (counsel-projectile-mode))

(use-package ripgrep)


(use-package treemacs
  :ensure t
  :config
  (require 'treemacs-themes)
  (require 'treemacs-icons)
  (require 'treemacs-icons-dired)
  (require 'treemacs-evil)
  (require 'treemacs-projectile)
  (require 'treemacs-magit)
  :bind (:map global-map ("C-x t t"   . treemacs))
  :commands treemacs-modify-theme
  :config (add-hook 'treemacs-mode-hook                                                                      
          (lambda () (define-key evil-motion-state-map (kbd "TAB") 'treemacs-TAB-action))) 
)

(use-package treemacs-evil
  :after treemacs evil
  :config
  ;;(evil-set-initial-state 'treemacs-mode 'evil)
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

;; don't know what it is but this one is unstable
;;(use-package treemacs-persp
;;  :after treemacs persp-mode
;;  :ensure t
;;  :config (treemacs-set-scope-type 'Perspectives))

; trying neotree
(use-package neotree)

(use-package all-the-icons
  :demand
  :ensure t)

;; bad with hidpi - icons modeline 
;(use-package mode-icons :config (mode-icons-mode -1))

;; modeline
(use-package doom-modeline
      :hook (after-init . doom-modeline-mode)
      :config
      ; these will hardcode height and zoom-frm will not work for mode-line
      ;(set-face-attribute 'mode-line nil :height 90)
      ;(set-face-attribute 'mode-line-inactive nil :height 50)
      (setq doom-modeline-height 25)
      (setq doom-modeline-bar-width 6)
      ;; scala projects may have very long file paths, in that case doommodeline doesn't truncate it
      ;;:custom (doom-modeline-buffer-file-name-style 'truncate-with-project)
      (setq doom-modeline-icon t)
)

;;;;;;; THEMES ;;;;;;;;
;; (load-theme 'dracula t)
;; (load-theme 'atom-one-dark t)
;; (load-theme 'avk-dark-blue-yellow t)
;; (load-theme 'nimbus-theme t)
;; (load-theme 'dracula-theme t)
;; (load-theme 'solarized-theme t)
;; (load-theme 'zenburn t)
(use-package gruvbox-theme)
(load-theme 'gruvbox t)
;; (load-theme 'nord t)

;; todo - doesn't work
;; (use-package theme-changer
;;   :config
;;   (setq calendar-location-name "Dallas, TX") 
;;   (setq calendar-latitude 32.85)
;;   (setq calendar-longitude -96.85)
;;   (change-theme nil 'dracula-theme)
;; )

(use-package modus-operandi-theme)
(use-package modus-vivendi-theme)
;; Define coordinates
(setq calendar-latitude 49.784443
      calendar-longitude 24.056473)
;; Light at sunrise
;(load-theme 'modus-operandi t t)
;(run-at-time (nth 1 (split-string (sunrise-sunset)))
;             (* 60 60 24)
;             (lambda ()
;               (enable-theme 'modus-operandi)))
;;; Dark at sunset
;(load-theme 'modus-vivendi t t)
;(run-at-time (nth 4 (split-string (sunrise-sunset)))
;             (* 60 60 24)
;             (lambda ()
;               (enable-theme 'modus-vivendi)))
;;;;;;;;;;;;;;;;;;;;;;;
(use-package doom-themes
  :demand
  :config
  ;(load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (setq doom-themes-treemacs-theme "doom-colors")
  )

;;;;;;;;;;; IVY ;;;;;;;;;;;;

(use-package posframe)

(use-package flx)

(use-package wgrep)

(use-package wgrep-ag)

(use-package counsel
  :after ivy
  :config (counsel-mode)
  :bind (("M-x" . counsel-M-x)
         ("C-c s c" . counsel-compile)
         ("C-c s g" . counsel-git)
         ("C-c s j" . counsel-git-grep)
         ("C-c s L" . counsel-git-log)
         ("C-c s k" . counsel-rg)
         ("C-c s m" . counsel-linux-app)
         ("C-c s n" . counsel-fzf)
         ("C-c s l" . counsel-locate)
         ("C-c s J" . counsel-file-jump)
         ("C-c s b" . counsel-bookmark)
         ("C-c s D" . counsel-descbinds)
         ("C-c s o" . counsel-outline)
         ("C-c s t" . counsel-load-theme)
         ("C-c s f" . counsel-org-file)
         ("C-c s u" . counsel-unicode-char)
         ("C-c s v" . counsel-set-variable)
         ("C-c s p" . counsel-package)
         ("C-c i" . counsel-info-lookup-symbol)
         ("M-y" . counsel-yank-pop)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h l" . counsel-find-library)
         ("C-x C-f" . counsel-find-file)
         ))

(use-package ivy
  :diminish
  :demand
  :bind (("C-c C-r" . ivy-resume)
         ("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window))
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         (:map ivy-minibuffer-map ("C-c C-c" . hydra-ivy/body))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config
  (ivy-mode)
  (setq ivy-re-builders-alist
        '(
          (ivy-switch-buffer . ivy--regex-fuzzy)
          (counsel-ag        . ivy--regex-plus)
          (counsel-git-grep  . ivy--regex-plus)
          (swiper            . ivy--regex-plus)
          (t                 . ivy--regex-fuzzy)))
  ;; all fuzzy init
  ;;(setq ivy-initial-inputs-alist nil)
)

(use-package ivy-hydra
  :ensure t
  :after ivy)

(defun ivy-rich-switch-buffer-icon (candidate)
  (with-current-buffer
      (get-buffer candidate)
    (let ((icon (all-the-icons-icon-for-mode major-mode)))
      (if (symbolp icon)
          (all-the-icons-icon-for-mode 'fundamental-mode)
        icon))))

(use-package ivy-rich
  :demand
  :after counsel
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  (ivy-rich-display-transformers-list
      '(ivy-switch-buffer
        (:columns
         (
          (ivy-rich-switch-buffer-icon (:width 2))
          (ivy-rich-candidate (:width 30))
          (ivy-rich-switch-buffer-size (:width 7))
          (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
          (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
          (ivy-rich-switch-buffer-project (:width 15 :face success))
          (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3)))))
          )
         :predicate
         (lambda (cand) (get-buffer cand)))))
  :config
  (ivy-rich-mode)
  (ivy-rich-project-root-cache-mode) ;; speed-up
  )

(use-package ag
  :ensure t
  :custom
  (ag-highlight-search t)
  (ag-reuse-buffers t)
  :config
  (add-to-list 'ag-arguments "--word-regexp"))

;; using ivy rich for now
(use-package all-the-icons-ivy
  :demand
  :after ivy-rich
  :config
  (require 'ivy-rich)
  (setq all-the-icons-ivy-file-commands
      '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir))
  ;;(all-the-icons-ivy-setup)
  )

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)))

;; testing it
;;(use-package ivy-posframe) ;; problem on windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package which-key
  :custom (which-key-idle-delay 0.5)
  :config (which-key-mode))

;;; help ;;;
;; in terminal C-h is basically a backspace
(global-set-key (kbd "C-c C-h") 'help-command)

(use-package helpful
  ;:config
  ;(require 'major-mode-hydra)
  ; experimenting, doesn't work
  ;:pretty-hydra
  ;((:color teal :quit-key "q")
  ; ("Helpful"
  ;  (("f" helpful-callable "callable")
  ;   ("v" helpful-variable "variable")
  ;   ("k" helpful-key "key")
  ;   ("c" helpful-command "command")
  ;   ("d" helpful-at-point "thing at point"))))
                                        ;:bind ("C-h H" . helpful-hydra/body)
  )

;; testing (todo - if no internet fails)
;(quelpa '(help-fns+ :fetcher wiki) :upgrade t)
;(require 'help-fns+)
;(use-package help-fns+
;  :quelpa (help-fns+ :fetcher wiki :upgrade t))

;; discover-my-major - not very helpful
(use-package discover-my-major
  :ensure t
  :commands (discover-my-major)
  ;; this one conflicts with help+
  :bind ("C-h C-m" . discover-my-major)
  :config
  (add-to-list 'evil-emacs-state-modes 'makey-key-mode))

;; navigation by optimized keystrokes
(use-package avy)

;;;;;;;;;;;;; EVIL MODE ;;;;;;;;;;;;;;
(use-package evil
  :demand
  :init
  ;; these 2 are for evil-collection
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  ;;(progn (evil-mode 1))
  :config
  (evil-mode)
  ;; disable evil in help mode (emacs by default)
  (define-key evil-motion-state-map [tab] nil)
  (add-to-list 'evil-emacs-state-modes 'debugger-mode)
  (evil-set-initial-state 'Info-mode 'emacs)
  ;;(evil-set-initial-state 'process-menu-mode 'emacs)
  ;;(evil-set-initial-state 'dashboard-mode 'emacs)
  ;;(evil-set-initial-state 'dired-mode 'emacs)
  ;;(evil-set-initial-state 'special-mode 'emacs)
  ;;(evil-set-initial-state 'messages-major-mode 'emacs)
  ;; conflict in terminal mode (because C-i and TAB is not distinguishable in terminal, C-i is evil jump forward)
  ; todo - this overrides C-i everywhere, do not want to give up evil-jump-forward for this
  ;(add-hook 'org-mode-hook                                                                      
  ;        (lambda ()                                                                          
  ;      (define-key evil-normal-state-map (kbd "TAB") 'org-cycle))) 
)

(use-package evil-leader
  :demand
  :after evil
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "s" 'save-buffer
    "b" 'switch-to-buffer
    "f" 'find-file
    "I" 'find-user-init-file
    "F" 'hydra-flycheck/body
    "B" 'hydra-btoggle/body
    "y" 'hydra-yasnippet/body
    "J" 'hydra-avy/body
    "j" 'avy-goto-char-timer
    "p" 'hydra-projectile/body
    "(" 'hydra-smartparens/body
    "g" 'hydra-magit/body
    "m" 'hydra-smerge/body
    "w" 'hydra-windows/body
    "O" 'hydra-folding/body
    "n" 'hydra-next-error/body
    "o" 'hydra-org/body
    "[" 'hydra-accessibility/body
    "h" 'major-mode-hydra
    "e" 'eshell-new
    "a" 'org-agenda
    "i" 'org-capture
    "l" 'hydra-lsp/body
    "L" 'ledger-kredo-replace
    "S" 'sbt-hydra
    "t" 'treemacs
    "k" 'hydra-s/body
    "M" 'evil-mc-mode
    "c" 'hydra-org-clock/body
    "v" 'er/expand-region
    "<SPC>" 'other-window
    "qq" 'save-buffers-kill-terminal
    "qQ" 'save-buffers-kill-emacs)
  (global-evil-leader-mode nil))

(use-package paredit
  :config (add-hook 'lisp-mode-hook 'enable-paredit-mode))

(use-package evil-cleverparens
  :init   (add-hook 'paredit-mode-hook 'evil-cleverparens-mode)
  :config
  (setq
   evil-cleverparens-swap-move-by-word-and-symbol t
   evil-cleverparens-use-additional-movement-keys t))

(use-package evil-surround
  :demand
  :after evil
  :config
  (global-evil-surround-mode 1)
  (add-to-list 'evil-surround-operator-alist '(evil-cp-change . change))
  (add-to-list 'evil-surround-operator-alist '(evil-cp-delete . delete)))

(use-package evil-org
  :demand
  :after evil org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode (lambda() (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-define-key 'motion org-agenda-mode-map "ZK" 'org-habit-toggle-display-in-agenda)
  (evil-define-key 'motion org-agenda-mode-map "ZD" 'org-agenda-toggle-deadlines)
)

(use-package evil-mc
  :after evil)

(use-package evil-collection
  :after evil
  :demand
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-want-unimpaired-p nil) ;; conflicts [,] bindings in org-evil-agenda
  (evil-collection-company-use-tng nil) ;; can't find company-tng-mode
  :config
  (evil-collection-init)
  (evil-collection-define-key 'normal 'ivy-minibuffer-map
    (kbd "<SPC> <SPC>") 'ivy-done))

;;;;;;; CUSTOM DEFINITIONS ;;;;;;;
(defun close-and-kill-next-pane ()
  "If there are multiple windows, then close the other pane and kill the buffer in it also."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(defun close-and-kill-current-pane ()
  "Kill current buffer and close the pane, works differently to 'kill-buffer-and-window' as it check whether there are other windows at all."
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

;;;;;;; CUSTOM BINDINGS ;;;;;;;
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "s-<right>") 'next-buffer)
(global-set-key (kbd "s-<left>") 'previous-buffer)
(evil-global-set-key 'normal (kbd "z j") 'evil-next-buffer)
(evil-global-set-key 'normal (kbd "z k") 'evil-prev-buffer)
(global-set-key (kbd "s-k") 'close-and-kill-current-pane)
(global-set-key (kbd "s-0") 'delete-window)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") 'split-window-below)
(global-set-key (kbd "s-3") 'split-window-right)
;;(global-set-key (kbd "M-.") 'projectile-find-tag)
;;(global-set-key (kbd "M-,") 'pop-tag-mark)
(global-set-key (kbd "C-;") 'comment-region)
(global-set-key (kbd "C-:") 'uncomment-region)
(global-set-key (kbd "C-x 4 1") 'close-and-kill-next-pane)
(global-set-key (kbd "s-!") 'close-and-kill-next-pane)
(global-set-key (kbd "C-c w") 'toggle-truncate-lines); wrap
;;; RESIZE BUFFERS ;;;
(global-set-key (kbd "M-S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-S-C-<down>") 'shrink-window)
(global-set-key (kbd "M-S-C-<up>") 'enlarge-window)

(define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)

;; todo - this has to be lazy loaded after agenda load
(add-hook 'org-agenda-mode-hook (lambda () (define-key org-agenda-mode-map (kbd "s-,") 'hydra-org-agenda/body)))



;;;; buffer menu highlighting
(setq buffer-menu-buffer-font-lock-keywords
      '(("^....[*]Man .*Man.*"   . font-lock-variable-name-face) ; Man page
        (".*Dired.*"             . font-lock-comment-face)       ; Dired
        ("^....[*]shell.*"       . font-lock-preprocessor-face)  ; shell buff
        (".*[*]scratch[*].*"     . font-lock-function-name-face) ; scratch buffer
        ("^....[*].*"            . font-lock-string-face)        ; "*" named buffers
        ("^..[*].*"              . font-lock-constant-face)      ; Modified
        ("^.[%].*"               . font-lock-keyword-face)))     ; Read only

(defun buffer-menu-custom-font-lock  ()
      (let ((font-lock-unfontify-region-function
             (lambda (start end)
               (remove-text-properties start end '(font-lock-face nil)))))
        (font-lock-unfontify-buffer)
        (set (make-local-variable 'font-lock-defaults)
             '(buffer-menu-buffer-font-lock-keywords t))
        (font-lock-fontify-buffer)))

(add-hook 'buffer-menu-mode-hook 'buffer-menu-custom-font-lock)



;; rename file and buffer ;;
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))
(global-set-key (kbd "C-c r")  'rename-file-and-buffer)

;; use ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package super-save
  :demand
  :config
  (super-save-mode +1)
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  (add-to-list 'super-save-triggers 'ivy-switch-buffer)
  ;; save on find-file
  (add-to-list 'super-save-hook-triggers 'find-file-hook))

(defun contextual-backspace ()
  "Hungry whitespace or delete word depending on context."
  (interactive)
  (if (looking-back "[[:space:]\n]\\{2,\\}" (- (point) 2))
      (while (looking-back "[[:space:]\n]" (- (point) 1))
        (delete-char -1))
    (cond
     ((and (boundp 'smartparens-strict-mode)
           smartparens-strict-mode)
      (sp-backward-kill-word 1))
     ((and (boundp 'subword-mode) 
           subword-mode)
      (subword-backward-kill 1))
     (t
      (backward-kill-word 1)))))

(global-set-key (kbd "C-M-<backspace>") 'contextual-backspace)

(global-auto-revert-mode t)

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~          HYDRA        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(use-package hydra
  :ensure t)

(use-package hydra-posframe
  :quelpa (hydra-posframe :fetcher github :repo "Ladicle/hydra-posframe")
  :hook (after-init . hydra-posframe-enable)
  )

(use-package major-mode-hydra
  :after hydra
  :preface
  (defun with-alltheicon (icon str &optional height v-adjust)
    "Displays an icon from all-the-icon."
    (s-concat (all-the-icons-alltheicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-faicon (icon str &optional height v-adjust)
    "Displays an icon from Font Awesome icon."
    (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-fileicon (icon str &optional height v-adjust)
    "Displays an icon from the Atom File Icons package."
    (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-octicon (icon str &optional height v-adjust)
    "Displays an icon from the GitHub Octicons."
    (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str)))

(major-mode-hydra-define emacs-lisp-mode nil
  ("Eval"
   (("b" eval-buffer "buffer")
    ("e" eval-defun "defun")
    ("r" eval-region "region"))
   "REPL"
   (("I" ielm "ielm"))
   "Test"
   (("t" ert "prompt")
    ("T" (ert t) "all")
    ("F" (ert :failed) "failed"))
   "Doc"
   (("d" describe-foo-at-point "thing-at-pt")
    ("f" describe-function "function")
    ("v" describe-variable "variable")
    ("i" info-lookup-symbol "info lookup"))))

;;Hydra / BToggle
;;Group a lot of commands.
(pretty-hydra-define hydra-btoggle
  (:hint nil :color amaranth :quit-key "q" :title (with-faicon "toggle-on" "Toggle" 1 -0.05))
  ("Basic"
   (("a" abbrev-mode "abbrev" :toggle t)
    ("n" auto-insert-mode "auto-insert" :toggle t)
    ("h" global-hungry-delete-mode "hungry delete" :toggle t))
   "Coding"
   (("e" electric-operator-mode "electric operator" :toggle t)
    ("f" flycheck-mode "flycheck" :toggle t)
    ("l" lsp-mode "lsp" :toggle t)
    ("H" hl-todo-mode "hl-todo" :toggle t)
    ("o" origami-mode "origami" :toggle t)
    ("s" smartparens-mode "smartparens" :toggle t))
   "Debug"
   (("de" toggle-debug-on-error "debug on error" :toggle debug-on-error)
    ("dq" toggle-debug-on-quit "debug on C-g" :toggle debug-on-quit))
   "UI"
   (("i" ivy-rich-mode "ivy-rich" :toggle t))))

(pretty-hydra-define hydra-flycheck
  (:hint nil :color teal :quit-key "q" :title (with-faicon "plane" "Flycheck" 1 -0.05))
  ("Checker"
   (("?" flycheck-describe-checker "describe")
    ("d" flycheck-disable-checker "disable")
    ("m" flycheck-mode "mode")
    ("s" flycheck-select-checker "select"))
   "Errors"
   (("k" flycheck-previous-error "previous" :color pink)
    ("j" flycheck-next-error "next" :color pink)
    ("f" flycheck-buffer "check")
    ("l" flycheck-list-errors "list"))
   "Other"
   (("M" flycheck-manual "manual")
    ("v" flycheck-verify-setup "verify setup"))))

(defhydra hydra-yasnippet (:color blue :hint nil)
  "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
  ("d" yas-load-directory)
  ("e" yas-activate-extra-mode)
  ("i" yas-insert-snippet)
  ("f" yas-visit-snippet-file :color blue)
  ("n" yas-new-snippet)
  ("t" yas-tryout-snippet)
  ("l" yas-describe-tables)
  ("g" yas/global-mode)
  ("m" yas/minor-mode)
  ("a" yas-reload-all))

(defhydra hydra-smartparens (:hint nil)
  "
 Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
------------------------------------------------------------------------------------------------------------------------
 [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
 [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
 [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
 [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
  ;; Moving
  ("a" sp-beginning-of-sexp)
  ("e" sp-end-of-sexp)
  ("f" sp-forward-sexp)
  ("b" sp-backward-sexp)
  ("n" sp-down-sexp)
  ("N" sp-backward-down-sexp)
  ("p" sp-up-sexp)
  ("P" sp-backward-up-sexp)
  
  ;; Slurping & barfing
  ("h" sp-backward-slurp-sexp)
  ("H" sp-backward-barf-sexp)
  ("l" sp-forward-slurp-sexp)
  ("L" sp-forward-barf-sexp)
  
  ;; Wrapping
  ("R" sp-rewrap-sexp)
  ("u" sp-unwrap-sexp)
  ("U" sp-backward-unwrap-sexp)
  ("(" sp-wrap-round)
  ("{" sp-wrap-curly)
  ("[" sp-wrap-square)
  
  ;; Sexp juggling
  ("S" sp-split-sexp)
  ("s" sp-splice-sexp)
  ("r" sp-raise-sexp)
  ("j" sp-join-sexp)
  ("t" sp-transpose-sexp)
  ("A" sp-absorb-sexp)
  ("E" sp-emit-sexp)
  ("o" sp-convolute-sexp)
  
  ;; Destructive editing
  ("c" sp-change-inner :exit t)
  ("C" sp-change-enclosing :exit t)
  ("k" sp-kill-sexp)
  ("K" sp-backward-kill-sexp)
  ("w" sp-copy-sexp)

  ("q" nil)
  ("g" nil))

;; TODO this doesn't work
(pretty-hydra-define hydra-s
  (:hint t :color teal :quit-key "RET" :title "String manipulation")
  ("Pertaining to words"
   (("w" (lambda()(interactive)(s-split-words (buffer-substring-no-properties (region-beginning) (region-end)))) "split words")
    ("c" (lambda()(interactive)(s-lower-camel-case (buffer-substring-no-properties (region-beginning) (region-end)))) "lower camel")
    ("C" (lambda()(interactive)(s-upper-camel-case (buffer-substring-no-properties (region-beginning) (region-end)))) "upper camel")
    ("s" (lambda()(interactive)(s-snake-case (buffer-substring-no-properties (region-beginning) (region-end)))) "snake")
    ("d" (lambda()(interactive)(s-dashed-words (buffer-substring-no-properties (region-beginning) (region-end)))) "dashed")
    ("W" (lambda()(interactive)(s-capitalized-words (buffer-substring-no-properties (region-beginning) (region-end)))) "capital")
    ("t" (lambda()(interactive)(s-titleized-words (buffer-substring-no-properties (region-beginning) (region-end)))) "titleize")
    ("i" (lambda()(interactive)(s-word-initials (buffer-substring-no-properties (region-beginning) (region-end)))) "initials"))))

(defhydra hydra-avy (:exit t :hint nil)
  "
 Line^^       Region^^        Goto
----------------------------------------------------------
 [_y_] yank   [_Y_] yank      [_j_] timed char  [_c_] char          [_C_] char-2
 [_m_] move   [_M_] move      [_w_] word        [_W_] any word
 [_k_] kill   [_K_] kill      [_l_] line        [_L_] end of line"
  ("j" avy-goto-char-timer)
  ("c" avy-goto-char)
  ("C" avy-goto-char-2)
  ("w" avy-goto-word-1)
  ("W" avy-goto-word-0)
  ("l" avy-goto-line)
  ("L" avy-goto-end-of-line)
  ("m" avy-move-line)
  ("M" avy-move-region)
  ("k" avy-kill-whole-line)
  ("K" avy-kill-region)
  ("y" avy-copy-line)
  ("Y" avy-copy-region))

(defhydra hydra-smerge
  (:color red :hint nil
          :pre (smerge-mode 1))
  "
^Move^ ^Keep^ ^Diff^ ^Pair^
------------------------------------------------------
_n_ext _b_ase  _R_efine _<_: base-upper
_p_rev _u_pper _E_diff _=_: upper-lower
^ ^ _l_ower _C_ombine _>_: base-lower
^ ^ _a_ll _r_esolve
_q_uit _RET_: current
"
  ("RET" smerge-keep-current)
  ("C" smerge-combine-with-next)
  ("E" smerge-ediff)
  ("R" smerge-refine)
  ("a" smerge-keep-all)
  ("b" smerge-keep-base)
  ("u" smerge-keep-upper)
  ("n" smerge-next)
  ("l" smerge-keep-lower)
  ("p" smerge-prev)
  ("r" smerge-resolve)
  ("<" smerge-diff-base-upper)
  ("=" smerge-diff-upper-lower)
  (">" smerge-diff-base-lower)
  ("q" nil :color blue))

(pretty-hydra-define hydra-projectile
  (:hint nil :color teal :quit-key "q" :title (with-faicon "rocket" "Projectile" 1 -0.05))
  ("Buffers"
   (("b" counsel-projectile-switch-to-buffer "list")
    ("k" projectile-kill-buffers "kill all")
    ("S" projectile-save-project-buffers "save all"))
   "Find"
   (("d" counsel-projectile-find-dir "directory")
    ("F" projectile-recentf "recent files")
    ("D" projectile-dired "dired")
    ("g" counsel-projectile-find-file-dwim "file dwim")
    ("f" counsel-projectile-find-file "file")
    ("p" counsel-projectile-switch-project "project"))
   "Other"
   (("i" projectile-invalidate-cache "reset cache")
    ("x" projectile-remove-known-project "remove known project")
    ("z" projectile-cache-current-file "cache current file")
    ("X" projectile-cleanup-known-projects "cleanup known projects"))
   "Search"
   (("r" projectile-replace "replace")
    ("o" projectile-multi-occur "occur")
    ("R" projectile-replace-regexp "regexp replace")
    ("sg" counsel-projectile-grep "grep")
    ("ss" counsel-projectile-ag "ag")
    ("sr" counsel-projectile-rg "rg")
    ("ss" counsel-rg "search")
    )))

(defhydra hydra-next-error (:hint nil)
    "
Compilation errors:
_j_: next error        _h_: first error    _q_uit
_k_: previous error    _l_: last error
"
    ("`" next-error     nil)
    ("j" next-error     nil :bind nil)
    ("k" previous-error nil :bind nil)
    ("h" first-error    nil :bind nil)
    ("l" (condition-case err
             (while t
               (next-error))
           (user-error nil))
     nil :bind nil)
    ("q" nil            nil :color blue))

(pretty-hydra-define hydra-lsp
  (:hint nil :color teal :quit-key "q" :exit t :title (with-faicon "rocket" "Lsp"))
 ("Find"
  (("D" lsp-find-declaration "declaration")
   ("d" lsp-find-definition "definition")
   ("R" lsp-find-references "references")
   ("i" lsp-find-implementation "implementation")
   ("gt" lsp-find-type-definition "type")
   ("f" lsp-ivy-workspace-symbol "symbol")
   ("F" lsp-ivy-global-workspace-symbol "global symbol")
   ("uf" lsp-ui-find-workspace-symbol "ui symbol")
   ("pd" lsp-ui-peek-find-definitions "peek def")
   ("pr" lsp-ui-peek-find-references "peek refs")
   ("pf" lsp-ui-peek-find-workspace-symbol "peek symb")
   ("pi" lsp-ui-peek-find-implementation "peek impl"))
  "Toggle"
  (("Td" lsp-ui-doc-mode "doc" :toggle t)
   ("TS" lsp-ui-sideline-mode "sideline" :toggle t)
   ("Ts" lsp-ui-sideline-toggle-symbols-info "side symb" :toggle t)
   ("Tl" lsp-lens-mode "lens" :toggle t)
   ("Ti" lsp-toggle-trace-io "trace-io" :toggle t)
   ("Th" lsp-toggle-symbol-highlight "symb highlight")
   ("Tf" lsp-toggle-on-type-formatting "format" :toggle t)
   ("TF" lsp-ui-flycheck-list "flycheck")
   ("TT" lsp-treemacs-sync-mode "treemacs sync" :toggle t)
   ("TD" lsp-diagnostics-modeline-mode "diag line" :toggle t)
   ("Tnf" lsp-signature-toggle-full-docs "sign docs full")
   ("Tna" lsp-signature-activate "sign activate help")
   ("Tns" lsp-toggle-signature-auto-activate "sign auto activate"))
  "Help"
  (("hd" lsp-ui-doc-glance "doc glance")
   ("hh" lsp-describe-thing-at-point "describe"))
  "Code"
  (("=f" lsp-format-buffer "format")
   ("=r" lsp-format-region "region")
   ("r" lsp-rename "rename")
   ("o" lsp-organize-imports "org imports")
   ("m" lsp-ui-imenu "imenu")
   ("x" lsp-execute-code-action "action"))
  "Other"
  (("l" lsp-avy-lens "avy lens")
   ("ge" lsp-treemacs-errors-list "errors")
   ("gh" lsp-treemacs-call-hierarchy "hierarchy")
   ("gf" lsp-ui-flycheck-list "flycheck")
   ("ga" xref-find-apropos "xref-apropos"))
  "Metals"
  (("Mb" lsp-metals-build-import "build import")
   ("Ms" lsp-metals-sources-scan "sources rescan")
   ("Mr" lsp-metals-build-connect "bloop reconnect"))
  "Session"
  (("s?" lsp-describe-session "describe")
   ("ss" lsp "start")
   ("sd" lsp-disconnect "disconnect")
   ("sr" lsp-workspace-restart "restart")
   ("sq" lsp-workspace-shutdown "shutdown")
   ("sl" lsp-workspace-show-log "log")
   ("sfa" lsp-workspace-folders-add "folders +")
   ("sfo" lsp-workspace-folders-open "folder")
   ("sfr" lsp-workspace-folders-remove "folders -")
   ("sfb" lsp-workspace-blacklist-remove "blacklist -"))))

(pretty-hydra-define hydra-magit
  (:hint nil :color teal :quit-key "q" :title (with-alltheicon "git" "Magit" 1 -0.05))
  ("Action"
   (("b" magit-blame-addition "blame")
    ("c" magit-clone "clone")
    ("i" magit-init "init")
    ("t" git-timemachine "time machine")
    ("l" magit-log-buffer-file "commit log (current file)")
    ("L" magit-log-current "commit log (project)")
    ("g" magit-status "status"))))

(pretty-hydra-define hydra-windows
  (:hint nil :forein-keys warn :quit-key "q" :title (with-faicon "windows" "Windows" 1 -0.05))
  ("Window"
   (("d" delete-window "delete window")
    ("o" delete-other-windows "delete others" :exit t)
    ("s" split-window-below "split below")
    ("h" split-window-horizontally "split horizontally")
    ("v" split-window-vertically "split vertically")
    ("w" other-window "other window" :exit t)
    ("r" rename-buffer "rename buffer" :exit t)
    ("a" ace-window "ace" :exit t)
    ("k" kill-buffer-and-window "kill buffer and window" :exit t))
   "Frame"
   (("fk" delete-frame "delete frame")
    ("fo" delete-other-frames "delete others")
    ("fn" make-frame-command "make frame"))
   "Size"
   (("b" balance-windows "balance")
    ("L" shrink-window-horizontally "narrow")
    ("H" enlarge-window-horizontally "widen")
    ("J" shrink-window "lower")
    ("K" enlarge-window "heighten")
    ("S" switch-window-then-swap-buffer "swap" :color teal))
   "Zoom"
   (("-" zoom-out "out");text-scale-decrease "out")
    ("+" zoom-in "in");text-scale-increase "in")
    ("=" zoom-frm-unzoom "reset"))));(text-scale-increase 0) "reset"))))

(defhydra hydra-buffer-menu (:color pink
                             :hint nil)
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))

(defhydra hydra-folding (:color red)
   "
  _o_pen node    _n_ext fold       toggle _f_orward  _s_how current only
  _c_lose node   _p_revious fold   toggle _a_ll
  "
   ("o" origami-open-node)
   ("c" origami-close-node)
   ("n" origami-next-fold)
   ("p" origami-previous-fold)
   ("f" origami-forward-toggle-node)
   ("a" origami-toggle-all-nodes)
   ("s" origami-show-only-node))

(pretty-hydra-define hydra-accessibility
  (:hint nil :color teal :quit-key "q" :title (with-faicon "universal-access" "Accessibility" 1 -0.05))
  ("TTS" (
    ("b" festival-say-buffer "festival bufer")
    ("r" festival-say-region "festival region")
    ("k" festival-kill-process "festival kill"))))

(pretty-hydra-define hydra-org
  (:hint nil :color teal :quit-key "q" :title (with-fileicon "org" "Org" 1 -0.05))
  ("Action"
   (
    ("a" org-agenda "agenda")
    ("j" hydra-org-clock/body "clock")
    ("O" hydra-org-agenda/body "agenda hydra")
    ("C" cfw:open-org-calendar "calfw-org")
    ("s" my/org-ql-goals "goals")
    ("c" org-capture "capture")
    ("g" org-gcal-fetch "gcal fetch")
    ("G" org-gcal-sync "gcal sync")
    ("L" org-store-link "store-link")
    ("l" org-insert-link-global "insert-link")
    ("i" org-id-copy "copy id")
    ("A" org-archive-done-in-file "archive done in file")
    ("d" org-decrypt-entry "decrypt")
    ("I" org-info-find-node "org info find")
    ("k" org-cut-subtree "cut-subtree")
    ("o" org-open-at-point-global "open-link")
    ("r" org-refile "refile")
    ("t" org-show-todo-tree "todo-tree"))))

(pretty-hydra-define hydra-org-clock
  (:hint nil :color blue :quit-key "q" :exit t :title (with-faicon "clock-o" "Clock"))
  ("Clock"
   (("i" org-mru-clock-in "pick in")
    ("I" org-clock-in "in")
    ("o" org-clock-out "out")
    ("c" org-clock-in-last "in last")
    ("j" org-mru-clock-select-recent-task "select recent")
    ("J" org-clock-goto "goto")
    ("e" org-clock-modify-effort-estimate "edit")
    ("q" org-clock-cancel "quit")
    ("?" (org-info "Clocking commands") "info"))
   "Clock report"
   (("d" org-clock-display "display")
    ("r" org-clock-report "report"))
   "Pomodoro"
   (("pp" (org-pomodoro '(16)) "start") ;; (4) - will ask for task interactively
    ("pr" (org-pomodoro-reset) "reset"))
   "Timer"
   (("ts" org-timer-start "start")
    ("tt" org-timer-set-timer "set")
    ("tp" org-timer-pause-or-continue "pause")
    ("tq" org-timer-stop "stop")
    ("t?" (org-info "Timers") "info"))
   "Timer insert"
   (("tm" org-timer "time")
    ("ti" org-timer-item "item"))))

(defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
                                 :post (setq which-key-inhibit nil)
                                 :hint none)
  "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
  ;; Entry
  ("hA" org-agenda-archive-default)
  ("hk" org-agenda-kill)
  ("hp" org-agenda-priority)
  ("hr" org-agenda-refile)
  ("h:" org-agenda-set-tags)
  ("ht" org-agenda-todo)
  ;; Visit entry
  ("o"   link-hint-open-link :exit t)
  ("<tab>" org-agenda-goto :exit t)
  ("TAB" org-agenda-goto :exit t)
  ("SPC" org-agenda-show-and-scroll-up)
  ("RET" org-agenda-switch-to :exit t)
  ;; Date
  ("dt" org-agenda-date-prompt)
  ("dd" org-agenda-deadline)
  ("+" org-agenda-do-date-later)
  ("-" org-agenda-do-date-earlier)
  ("ds" org-agenda-schedule)
  ;; View
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vt" org-agenda-fortnight-view)
  ("vm" org-agenda-month-view)
  ("vy" org-agenda-year-view)
  ("vn" org-agenda-later)
  ("vp" org-agenda-earlier)
  ("vr" org-agenda-reset-view)
  ;; Toggle mode
  ("ta" org-agenda-archives-mode)
  ("tA" (org-agenda-archives-mode 'files))
  ("tr" org-agenda-clockreport-mode)
  ("tf" org-agenda-follow-mode)
  ("tl" org-agenda-log-mode)
  ("td" org-agenda-toggle-diary)
  ;; Filter
  ("fc" org-agenda-filter-by-category)
  ("fx" org-agenda-filter-by-regexp)
  ("ft" org-agenda-filter-by-tag)
  ("fr" org-agenda-filter-by-tag-refine)
  ("fh" org-agenda-filter-by-top-headline)
  ("fd" org-agenda-filter-remove-all)
  ;; Clock
  ("cq" org-agenda-clock-cancel)
  ("cj" org-agenda-clock-goto :exit t)
  ("ci" org-agenda-clock-in :exit t)
  ("co" org-agenda-clock-out)
  ;; Other
  ("q" nil :exit t)
  ("gd" org-agenda-goto-date)
  ("." org-agenda-goto-today)
  ("gr" org-agenda-redo))

;; came from here - https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-elisp.el
(defhydra hydra-edebug (:color amaranth
                        :hint  none)
  "
    EDEBUG MODE
^^_<SPC>_ step             ^^_f_ forward sexp         _b_reakpoint set                previous _r_esult      _w_here                    ^^_d_ebug backtrace
^^_n_ext                   ^^goto _h_ere              _u_nset breakpoint              _e_val expression      bounce _p_oint             _q_ top level (_Q_ nonstop)
_g_o (_G_ nonstop)         ^^_I_nstrument callee      next _B_reakpoint               _E_val list            _v_iew outside             ^^_a_bort recursive edit
_t_race (_T_ fast)         step _i_n/_o_ut            _x_ conditional breakpoint      eval _l_ast sexp       toggle save _W_indows      ^^_S_top
_c_ontinue (_C_ fast)      ^^^^                       _X_ global breakpoint
"
  ("<SPC>" edebug-step-mode)
  ("n"     edebug-next-mode)
  ("g"     edebug-go-mode)
  ("G"     edebug-Go-nonstop-mode)
  ("t"     edebug-trace-mode)
  ("T"     edebug-Trace-fast-mode)
  ("c"     edebug-continue-mode)
  ("C"     edebug-Continue-fast-mode)

  ("f"     edebug-forward-sexp)
  ("h"     edebug-goto-here)
  ("I"     edebug-instrument-callee)
  ("i"     edebug-step-in)
  ("o"     edebug-step-out)

  ;; breakpoints
  ("b"     edebug-set-breakpoint)
  ("u"     edebug-unset-breakpoint)
  ("B"     edebug-next-breakpoint)
  ("x"     edebug-set-conditional-breakpoint)
  ("X"     edebug-set-global-break-condition)

  ;; evaluation
  ("r"     edebug-previous-result)
  ("e"     edebug-eval-expression)
  ("l"     edebug-eval-last-sexp)
  ("E"     edebug-visit-eval-list)

  ;; views
  ("w"     edebug-where)
  ("p"     edebug-bounce-point)
  ("v"     edebug-view-outside) ; maybe obsolete??
  ("P"     edebug-view-outside) ; same as v
  ("W"     edebug-toggle-save-windows)

  ("d"     edebug-backtrace)

  ;; quitting and stopping
  ("q"     top-level :color blue)
  ("Q"     edebug-top-level-nonstop :color blue)
  ("a"     abort-recursive-edit :color blue)
  ("S"     edebug-stop :color blue))

(with-eval-after-load 'edebug
  (bind-key "?" #'hydra-edebug/body edebug-mode-map))


;;===================================================================================================
;;===================================================================================================
;;===================================================================================================
;;===============================            END HYDRA        =======================================
;;===================================================================================================
;;===================================================================================================
;;===================================================================================================




;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~        ORG_MODE       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(require 'org)

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; unset - C-tab used for window cycling
(define-key org-mode-map [(control tab)] nil)

(setq org-directory "~/Dropbox/org/")
;;(setq org-agenda-files (quote ("~/org/agendas.org")))

(setq org-startup-folded 'fold)

;; ret follows link (in evil, go to <insert> and then return)
(setq org-return-follows-link t)

(defun twist/create-talk-file()
    "Create an org file for a new talk"
    (interactive)
    (let ((name (read-string "Filename: ")))
      (expand-file-name (format "%s.org" name) "~/Dropbox/org/talks/")))

(setq org-capture-templates
      '(
        ("i" "Todo [inbox]" entry (file "~/Dropbox/org/inbox.org" ) "* TODO %i%?")
        ("g" "Goal" entry (file "~/Dropbox/org/goals.org") "\
* GOAL *%^{Goal title}* \t:%^G:goal:\n\
  :PROPERTIES:\n\
  :CREATED: %U\n\
  :END:\n\
  %^{Goal description}")
        ("p" "Project" entry (file "~/Dropbox/org/inbox.org") "\
* PROJECT *%^{Project title}* [%] :%^G:project:\n\
  :PROPERTIES:\n\
  :CREATED: %U\n\
  :END:\n\
  %^{Project description}\n\
** TODO %?\n\
** TODO review %\\1 \n\
   SCHEDULED: <%<%Y-%m-%d %a .+14d>>\n\
   :PROPERTIES:\n\
   :Effort: %^{Effort}\n\
   :END:\n\
** _IDEAS_")
        ("h" "Habit" entry (file+headline "~/Dropbox/org/personal.org" "*habits*") "\
* %?\n\
  SCHEDULED: <%<%Y-%m-%d %a .+1d>>\n\
  :PROPERTIES:\n\
  :CREATED: %U\n\
  :STYLE: habit\n\
  :REPEAT_TO_STATE: \n\
  :LOGGING: DONE(!)\n\
  :ARCHIVE: archive/%s_archive::* Habits\n\
  :END:\n")
        ("B" "Budget entry" entry (file+olp "~/Dropbox/org/personal.org" "*finance*" "*budgeting*" "finance Oct 2019")
         "* %^{Entry description}\n  :PROPERTIES:\n  :AMOUNT:   %^{Amount}\n  :CURRENCY: UAH\n  :DATETIME:  %U\n  :CATEGORY:  %^{Category}\n  :TYPE:     CASH\n  :END:\n")
        ("T" "Talk" plain (file twist/create-talk-file) "\
#+OPTIONS: reveal_global_footer:t\n\
#+REVEAL_THEME: beige\n\
#+REVEAL_PLUGINS: (highlight notes)\n\
#+REVEAL_INIT_OPTIONS: slideNumber:true\n\
#+REVEAL_HLEVEL: 1\n\
#+TITLE: %^{Title}\n\
#+AUTHOR: https://git.io/Jvd9c\n\
#+EMAIL: twist522@gmail.com\n\
#+OPTIONS: reveal_title_slide:\"<h2>\\%t</h2><h4>%^{Sub Title}</h4>\"\n\
#+OPTIONS: toc:nil\n\
#+OPTIONS: num:0\n\
\n\
* Plan\n\
  :PROPERTIES:\n\
  :UNNUMBERED: t\n\
  :END:\n\
  - %^{point1}\n\
  - %^{point2}\n\
  - %^{point3}\n\
  - %^{point4}\n\
* Problem\n\
* Solution\n\
* Call to Action\n\
  - %^{action1}\n\
  - %^{action2}\n\
* \n\
:PROPERTIES:\n\
:UNNUMBERED: t\n\
:END:\n\
#+REVEAL_HTML: <h1>&#x1F603;</h1>")
        ("a" "Appointment" entry (file  "~/Dropbox/org/gcal/personal.org") "* %?\n\n%^T")
        ("j" "Journal" entry (file+olp+datetree "~/Dropbox/org/journal.org") "* %<%H:%M> %?\n %i\n\n  From: %a" :empty-lines 1)
        ("e" "Word [english]" entry (file "~/Dropbox/org/english.org") "* %i%?")
        ("o" "Org idea" entry (file+olp "~/Dropbox/org/org.org" "ideas" "org ideas") "*** TODO %i%?")
        ("b" "Buylist" entry (file+olp "~/Dropbox/org/personal.org" "*buylist*") "** TODO %i%?")
        ("m" "Meal" entry (file+olp "~/Dropbox/org/food.org" "_MEAL_") "** %t meal\n\t- breakfast: %^{Breakfast}\n\t- lunch: %^{Lunch}\n\t- snack: %^{Snack}\n\t- dinner: %^{Dinner}")
        ("t" "Personal task" entry (file+olp "~/Dropbox/org/personal.org" "_TASKS_") "** TODO %i%?\n   SCHEDULED: <%<%Y-%m-%d %a>>")
        ("I" "Idea")
        ("Ib" "Idea" entry (file+olp "~/Dropbox/org/ideas.org" "*talk/blog*") "** TODO %i%?")
        ("E" "Emacs todo" entry (file+headline "~/Dropbox/org/emacs.org" "ideas / todo") "* TODO %i%?")
        ("l" "Ledger")
        ("lb" "Bank" plain (file "~/Dropbox/org/ledger/ledger.dat")
            "%(org-read-date) %^{Payee}\n\tExpenses:%^{Account}  ₴%^{Ammount}\n\tKredo" :empty-lines 1 :immediate-finish t)
        ("lc" "Cash" plain (file "~/Dropbox/org/ledger/ledger.dat")
            "%(org-read-date) * %^{Payee}\n\tExpenses:%^{Account}  ₴%^{Amount}\n\tCash" :empty-lines 1 :immediate-finish t)))

;; description of capture
;;(setq org-capture-templates '((
;;     "t"                ; key
;;     "Todo"             ; description
;;     entry              ; type
;;     (file+headline "C:/.../org/notes.org" "tasks")       ; target
;;     "* TODO [#B] %^{Todo} %(org-set-tags) \n:PROPERTIES:\n:Created: %U\n:END:\n\n%?"  ; template
;;     :prepend t        ; properties
;;     :empty-lines 1    ; properties
;;     :created t        ; properties
;;     )))

;; todo - this may not work when installing from scratch
(use-package org-plus-contrib)
(require 'org-expiry)

(setq org-expiry-inactive-timestamps t)
;; adds CREATED property - works after each capture
(add-hook 'org-capture-before-finalize-hook 
         #'(lambda()
               (save-excursion
                    (org-back-to-heading)
                    (org-expiry-insert-created))))

;; adds CREATED property - works after changing state into TODO
(add-hook 'org-after-todo-state-change-hook
          (lambda ()
            (when (string= org-state "TODO")
              (save-excursion
                (org-back-to-heading)
                (org-expiry-insert-created)))))

(setq org-log-done t)

(setq org-todo-keywords
      '(
        (sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w@/!)" "DELEGATED(e@/!)" "ON-HOLD(h@/!)" "|")
        (sequence "MAYBE(m)" "SOMEDAY(s)" "PROJECT(p)" "|")
        (sequence "VISION(v)" "GOAL(g)" "FOCUS(f)" "MODE(o)" "|")
        (sequence "|" "DONE(d!)" "CLOSED(c@/!)" "CANCELLED(C@/!)" "SKIPPED(S@/!)")
        )
)

;; Setting Colours (faces) for todo states to give clearer view of work 
;; lookup by M-x list-colors-display
(setq org-todo-keyword-faces
      '(
        ("PROJECT" . "maroon2")
        ("GOAL" . "SeaGreen4")
        ("VISION" . "DeepSkyBlue")
        ("FOCUS" . "orange")
        ("MODE" . "peru")
        ("TODO" . "orange red")
        ("SOMEDAY" . "IndianRed2")
        ("MAYBE" . "IndianRed2")
        ("IN-PROGRESS" . "dark goldenrod")
        ("WAITING" . "blue violet")
        ("DELEGATED" . "dark olive green")
        ("ON-HOLD" . "orange")
        ("DONE" . "forest green")
        ("CLOSED" . "cyan4")
        ("CANCELLED" . "cyan4")
        ("SKIPPED" . "cyan4")
        )
)

(setq org-tag-alist '(
        ("@office" . ?O)
        ("@home" . ?H)
        ("@rivne" . ?r)
        ("@phone" . ?o)
        ("@computer" . ?c)
        ("@internet" . ?i)
        ("@shop" . ?S)
        ("@grocery" . ?g)
        ("@pharmacy" . ?R)
        ("work" . ?w)
        ("blocking" . ?B)
        ("goal" . ?G)
        ("family" . ?f)
        ("personal" . ?p)
        ("project" . ?P)
        ("area" . ?A)
        ("health" . ?h)
        ("buy" . ?b)
        ("car" . ?a)
        ("sell" . ?s)
        ("income" . ?I)
        ("expense" . ?E)
        ("ptashka" . ?k)
        ("deep" . ?d)
        )
)

(setq org-journal-tag-alist '(
                              ("emotions" . ?e)))

(setq org-default-priority ?C org-lowest-priority ?D)

(setq org-agenda-prefix-format '(
       (agenda . " %i %-16:c%?-12t% s")
       ;(agenda . " %i %-23b %-16:c%?-12t% s")
       (todo . " %i %-16:c")
       (tags . " %i %-16:c")
       (search . " %i %-16:c")))

(setq org-stuck-projects '("+project" ("TODO" "IN-PROGRESS") nil ""))
      ;default
      ;'("+LEVEL=2/-DONE" ("TODO" "NEXT" "NEXTACTION") nil ""))

;; custom agendas ;;
(setq org-agenda-custom-commands
      '(("c" . "Custom Agendas")
        ("cB" "Blocking others" ((tags "+blocking/!")) nil nil)
        ("ct" "Today" ((agenda "" ((org-agenda-span 1))) nil) nil)
        ("cT" "All Todo" ((tags-todo "-project-book/!-GOAL-VISION-MODE-FOCUS-SOMEDAY-MAYBE-DRAFT-IDEA-TOREAD-READING")) nil nil)
        ("cA" "Appointments" agenda* nil nil)
        ("cW" "Waiting for" ((todo "WAITING")) nil nil)
        ("cd" "Delegated" ((todo "DELEGATED")) nil nil)
        ("cD" "Done" ((todo "DONE|CANCELLED|CLOSED|SKIPPED")) nil nil)
        ("cu" "Unscheduled" ((tags-todo "-project-book/!-GOAL-MODE-FOCUS-VISION-SOMEDAY-MAYBE-DRAFT-IDEA-TOREAD-READING"
              ((org-agenda-overriding-header "\nUnscheduled TODO")
               (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp)))))
           nil
           nil)
        ("cI" "All A-B Todo" ((tags-todo "-project+PRIORITY=\"A\"|-project+PRIORITY=\"B\"/!-GOAL-VISION-MODE-FOCUS-SOMEDAY-MAYBE-DRAFT-IDEA-TOREAD-READING"))
         ((org-agenda-overriding-header "All A-B Todo")) nil)
        ("ci" "All In Progress" ((todo "IN-PROGRESS")) ((org-agenda-max-entries 25)) nil)
        ("cp" "Projects" ((tags-todo "+project")) nil nil)
        ("cg" "Goals" ((todo "GOAL")) nil nil)
        ("cv" "Vision" ((todo "VISION")) nil nil)
        ("cS" "Someday/Maybe" ((todo "SOMEDAY|MAYBE")) nil nil)
        ("cs" "Stuck Projects" ((stuck "")) nil nil)
        ("ca" "Areas" ((tags "+area")) nil nil)
        ("cb" "Buylist" ((tags "+buy")) nil nil)
        ("co" "Books" ((tags-todo "+book")) nil nil)
        ;("cD" "Deep" ((tags-todo "+deep")) nil nil)
        ;("ck" "Deep work" ((tags-todo "+deep+work")) nil nil)
        ("ch" "Habits" ((tags "STYLE=\"habit\""))
          ((org-agenda-overriding-header "Habits")
          (org-agenda-sorting-stragety '(todo-state-down effort-up category-keep))) nil)
        ;("cf" "test occur" ((occur-tree "idea")))
        ;("c," "Process" ((tags-todo "-deep-project")) nil nil)
        ;("c0" "(testing)Buylist(-tree doesn't work?)" ((tags-tree "+buy")) nil nil)
        ;("c1" "(testing)Waiting for(-tree doesn't work?)" ((todo-tree "WAITING")) nil nil)
        ("cz" "All TODOs groups by category" alltodo "" ((org-super-agenda-groups '((:auto-category t)))))
        ("a" "Action" (
         (todo "IN-PROGRESS"
                    ((org-agenda-overriding-header "⚡ Doing:")
                     (org-agenda-prefix-format " %-3i %12c %-30(concat \"❱ \" (my/org-get-parent-goal)) ")
                     (org-agenda-todo-keyword-format "%11s")))
         (tags-todo "-project+PRIORITY=\"A\"-TODO=\"IN-PROGRESS\"|-project+PRIORITY=\"B\"-TODO=\"IN-PROGRESS\"/!-GOAL-VISION-MODE-FOCUS-SOMEDAY-MAYBE-DRAFT-IDEA-TOREAD-READING"
         ;(tags-todo "+TODO=\"IN-PROGRESS\"|-project+PRIORITY=\"A\"|-project+PRIORITY=\"B\"/!-GOAL-VISION-MODE-FOCUS-SOMEDAY-MAYBE-DRAFT-IDEA-TOREAD-READING"
                    ((org-agenda-overriding-header "⚡ Next:")
                     (org-agenda-max-entries 20)
                     (org-agenda-prefix-format " %-3i %12c %-30(concat \"❱ \" (my/org-get-parent-goal)) ")
                     (org-agenda-todo-keyword-format "%11s")))
         (agenda "" ((org-agenda-span 5)
                     (org-agenda-todo-keyword-format " 🔨")
                     ;; (org-agenda-skip-scheduled-if-done t)
                     ;; (org-agenda-skip-timestamp-if-done t)
                     ;; (org-agenda-skip-deadline-if-done t)
                     ;; (org-agenda-remove-tags t)
                     ;; (org-agenda-start-day "+0d")
                     ;; (org-agenda-span 5)
                     ;; (org-agenda-repeating-timestamp-show-all nil)
                     (org-agenda-current-time-string "⮜┈┈┈┈┈┈┈ now")
                     (org-agenda-scheduled-leaders '("⏰" "⏰.%2dx: "))
                     (org-agenda-deadline-leaders '("☠" "In %3d d.: " "%2d d. ago: "))
                     (org-agenda-time-grid (quote ((today require-timed remove-match) (0900 2100) "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))
                     (org-agenda-overriding-header "⚡ Schedule:")
                     (org-agenda-prefix-format " %-3i %12c %-25(concat \"❱ \" (my/org-get-parent-goal)) %?-12t% s")
                     ))))
        ("r" "Review" (
         (tags "+blocking/!" ((org-agenda-overriding-header "Blocking others")))
         (todo "DELEGATED" ((org-agenda-overriding-header "Delegated")))
         (todo "WAITING" ((org-agenda-overriding-header "Waiting for")))
         (tags "+goal+current" ((org-agenda-overriding-header "⚡ Current goals:")))
         (todo "IN-PROGRESS" ((org-agenda-overriding-header "In progress")))
         (tags-todo "-project+PRIORITY=\"A\"-TODO=\"IN-PROGRESS\"|-project+PRIORITY=\"B\"-TODO=\"IN-PROGRESS\"/!-GOAL-DRAFT-TOREAD-IDEA"
                    ((org-agenda-overriding-header "Most important to do")))
         (tags-todo "+project+PRIORITY=\"A\"|+project+PRIORITY=\"B\"" ((org-agenda-overriding-header "A-B Projects") (org-agenda-max-entries 15)))
         (tags-todo "+project+PRIORITY=\"C\"|+project+PRIORITY=\"D\"" ((org-agenda-overriding-header "Other Projects")))
         (todo "SOMEDAY|MAYBE" ((org-agenda-overriding-header "Someday/Maybe")))
         (tags-todo "-project-book-PRIORITY=\"A\"-PRIORITY=\"B\"-TODO=\"IN-PROGRESS\"/!-WAITING-GOAL-VISION-MODE-FOCUS-SOMEDAY-MAYBE-DRAFT-IDEA-TOREAD-READING"
                    ((org-agenda-overriding-header "Other to do")))
         (tags "STYLE=\"habit\"" ((org-agenda-overriding-header "Habits") (org-agenda-sorting-stragety '(todo-state-down effort-up category-keep))) nil)
         ;; todo: ideas
         ;; todo: books
         ;;
         ;;(stuck "") ; review stuck projects as designated by org-stuck-projects
         ;;(org-ql-block '(tags "project") ((org-agenda-overriding-header "Projects"))) ; example of mixing in org-ql
         ; trying not to schedule stuff unless it is appointment, habit or deadline
         ;(tags-todo "-project-book+PRIORITY=\"B\"|-project-book+PRIORITY=\"C\"/!-GOAL-VISION-MODE-FOCUS-SOMEDAY-MAYBE-DRAFT-IDEA-TOREAD-READING"
         ;     ((org-agenda-overriding-header "Unscheduled B-C")
         ;      ;;(org-agenda-max-entries 20)
         ;      (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))))
         ;; todo - to-archive list (DONE tasks not under project, with _TASKS_ parrent or specific location)
         ;;(agenda "" ((org-agenda-span 1) (org-agenda-overriding-header "Today")))
         ))
        ))

;; agenda icons

;(setq org-agenda-category-icon-alist `(
;  ;("personal" ,(list (all-the-icons-material "check_box" :height 1.2)) nil nil :ascent center)
;  ("personal" ,(list (all-the-icons-faicon "home")) nil nil :ascent center)
;  ("work" ,(list (all-the-icons-material "work")) nil nil :ascent center)
;  ("content" ,(list (all-the-icons-fileicon "video")) nil nil :ascent center)
;  ("blog" ,(list (all-the-icons-octicon "book")) nil nil :ascent center)
;  ("employment" ,(list (all-the-icons-material "people")) nil nil :ascent center)
;  ("finance" ,(list (all-the-icons-faicon "money")) nil nil :ascent center)
;  ; todo
;  ("rivne" ,(list (all-the-icons-faicon "sun-o")) nil nil :ascent center)
;  ("bigtrip" ,(list (all-the-icons-faicon "sun-o")) nil nil :ascent center)
;  ("emacs" ,(list (all-the-icons-faicon "sun-o")) nil nil :ascent center)
;  ("software" ,(list (all-the-icons-faicon "sun-o")) nil nil :ascent center)
;  ("holiday" ,(list (all-the-icons-faicon "sun-o")) nil nil :ascent center)
;  ("health" ,(list (all-the-icons-faicon "sun-o")) nil nil :ascent center)
;  ("consume" ,(list (all-the-icons-faicon "sun-o")) nil nil :ascent center)
;  ("org" ,(list (all-the-icons-faicon "sun-o")) nil nil :ascent center)
;  ))
;(setq org-agenda-category-icon-alist nil)

(setq org-deadline-warning-days 7)

(setq org-agenda-breadcrumbs-separator " ❱ ")

;(setq org-ellipsis "…")

(use-package mixed-pitch
  ;:hook
  ;; If you want it in all text modes:
  ;(text-mode . mixed-pitch-mode)
)

;; writing
(use-package olivetti)

(defun my-org-mode-autosave-settings ()
  (add-hook 'auto-save-hook 'org-save-all-org-buffers nil nil))

;(add-hook 'org-mode-hook
           ;#'olivetti-mode ;; ugly
           ;#'mixed-pitch-mode) ;; ugly
(add-hook 'org-mode-hook
           #'my-org-mode-autosave-settings)
;; experiments
(setq bidi-paragraph-direction t
  org-hide-emphasis-markers t
  org-fontify-done-headline t
  org-fontify-whole-heading-line t
  org-fontify-quote-and-verse-blocks t
  org-agenda-skip-scheduled-delay-if-deadline t
  org-agenda-skip-scheduled-if-deadline-is-shown t
  org-agenda-skip-deadline-prewarning-if-scheduled t
  org-agenda-block-separator (string-to-char " ")
  )

(setq org-refile-targets `(
                           (nil :maxlevel . 9)
                           ((,(concat org-directory "english.org"),(concat org-directory "org.org"),(concat org-directory "knowledge.org")) :maxlevel . 9)
                           (org-agenda-files :maxlevel . 5) ;; todo remove gcal files
                           ))
(setq org-outline-path-complete-in-steps nil)          ; Refile in a single go
(setq org-refile-use-outline-path 'file)               ; Show full paths for refiling - trick to refile in 0 level
(setq org-refile-allow-creating-parent-nodes 'confirm) ; create new parent on the fly

;; archived location
(setq org-archive-location "~/Dropbox/org/archive/%s_archive::")

;; inheritance
(setq org-tags-exclude-from-inheritance (quote ("project" "area")))

;; we can control inheritance directly in function org-entry-get
(setq org-use-property-inheritance nil) ;'("GOAL" "VISION"))

;; log into LOGBOOK
(setq org-log-into-drawer "LOGBOOK")

;; effort & column view

;;(setq org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS")
(setq org-columns-default-format-for-agenda "%60ITEM(Task) %6Effort(Estim){:}")
(add-to-list 'org-global-properties '("Effort_ALL" . "0 0:05 0:10 0:15 0:25 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00 10:00 15:00 24:00"))

;; actually bound to C-c <tab>
;;(bind-key "C-c C-<tab>" 'org-force-cycle-archived org-mode-map)

;; org plantuml
(use-package plantuml-mode
  :custom
  (plantuml-default-exec-mode 'jar)
  (plantuml-jar-path (expand-file-name "~/plantuml/plantuml.jar"))
  )
(setq org-plantuml-jar-path (expand-file-name "~/plantuml/plantuml.jar"))

;; Enable plantuml-mode for PlantUML files
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("sql" "sh"))))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; gnupplot
(use-package gnuplot
  :config
  (setq gnuplot-program-version "5.4") ;; auto-determine version within gnuplot.el doesn't work
  (autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
  (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)
  ;; this line automatically causes all files with the .gp extension to be loaded into gnuplot mode
  (setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))
  ;; This line binds the function-9 key so that it opens a buffer into gnuplot mode
  (global-set-key [(f9)] 'gnuplot-make-buffer)
)

;; info - https://org-babel.readthedocs.io/en/latest/header-args/#results
;; load babel langs
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (emacs-lisp . t)
   (gnuplot . t)
   (plantuml . t)
   (python . t)
   (shell . t)
   (ledger . t)
   (sql . t)))

;; without this it gets crazy when editing src inline
(setq org-src-preserve-indentation t)

;; latex ;;

(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; bulk rename tag - utility ;;;;;
(defun change-tag (old new)
  (when (member old (org-get-tags))
    (org-toggle-tag new 'on)
    (org-toggle-tag old 'off)
    ))
(defun org-rename-tag (old new)
  (interactive "scurrent tag: \nsnew name: ")
  (org-map-entries
   (lambda () (change-tag old new))
   (format "+%s" old)
   'agenda-with-archives
   ))
;;;; archive all DONEs in file ;;;;
;; in-progress, but not needed actually - do the same - C-c a T DONE * B $
(defun org-archive-done-in-file ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; persist history of clock-in clock-out between emacs shutdowns
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(setq org-clock-idle-time 90)

(setq org-agenda-clockreport-parameter-plist 
      '(:link t :maxlevel 4 :hidefiles t :fileskip0 t))

; alert if not clocking
(run-with-timer 0 (* 5 60) #'(lambda ()
                               (when (not (org-clocking-p)) (progn (alert "din din" :severity 'low :title "clock in" :category "clock"))))) ; org-mru-clock-in
; todo alert/clock-out if clocking for too long

(use-package org-mru-clock
  :ensure t
  :bind* (("C-c C-x i" . org-mru-clock-in)
          ("C-c C-x j" . org-mru-clock-select-recent-task))
  :init
  (setq org-mru-clock-how-many 40
        org-mru-clock-completing-read #'ivy-completing-read
        ))

(use-package org-journal
  :ensure t
  :defer t
  :bind (("C-c j j" . org-journal-new-entry))
  :custom
  (org-journal-dir "~/Dropbox/org/journal/")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-file-type 'weekly)
  (org-journal-enable-agenda-integration t)
)

(use-package alert
  :config
  (if (eq system-type 'windows-nt)
      (progn
        ;; assumes it's in site-lisp
        (require 'alert-toast)
        (setq alert-default-style 'toast))
      (setq alert-default-style 'libnotify)))

(use-package org-alert)

(use-package org-pomodoro
  :commands (org-pomodoro)
  :config
  (require 'org-pomodoro-pidgin)
  (require 'alert)
  :custom
  (org-pomodoro-length 50)
  (org-pomodoro-short-break-length 10)
  (org-pomodoro-format "%s")
  (org-pomodoro-short-break-format "%s")
  (org-pomodoro-long-break-format "~~%s~~")
  (org-pomodoro-audio-player "mplayer")
  (org-pomodoro-long-break-sound "/usr/share/sounds/freedesktop/stereo/window-attention.oga")
  (org-pomodoro-long-break-sound-args "-af volume=5")
  (org-pomodoro-short-break-sound "/usr/share/sounds/freedesktop/stereo/window-attention.oga")
  (org-pomodoro-short-break-sound-args "-af volume=5")
  (org-pomodoro-finished-sound "/usr/share/sounds/freedesktop/stereo/complete.oga")
  (org-pomodoro-finished-sound-args "-af volume=5")
  (org-pomodoro-start-sound "/usr/share/sounds/freedesktop/stereo/complete.oga")
  (org-pomodoro-start-sound-args "-af volume=5")
  :hook
  (org-pomodoro-break-finished . (lambda () (interactive) (org-pomodoro '(16))))
  (org-pomodoro-finished . (lambda () (interactive) (shell-command "i3lock-fancy-rapid 6 6"))))

;;;;;;;;;;;;;;; ORG-GCAL ;;;;;;;;;;;;;;;;

(setq package-check-signature nil)

;; trying to fix encoding problem
;;(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
;;(set-language-environment "UTF-8")
;;  (set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
;;  (setq locale-coding-system 'utf-8)
;;  (set-default-coding-systems 'utf-8)
;;  (set-terminal-coding-system 'utf-8)
;;  (set-selection-coding-system
;;    (if (eq system-type 'windows-nt)
;;        'utf-16-le  ;; https://rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
;;      'utf-8))
;;  (prefer-coding-system 'utf-8)

(defun filter-gcal-event-maybe (event)
  "Function for [org-gcal-fetch-event-filters]."
  (let* ((case-fold-search t)
        (attendees (plist-get event :attendees))
        (my-response (when attendees
                        (reduce (lambda (last next)
                                  (if (plist-get next :self) next last))
                                attendees
                                :initial-value nil))))
    (cond ((string-equal (plist-get my-response :responseStatus) "declined") nil) (t t))
    )
)

(setq org-gcal-local-timezone nil)
(use-package org-gcal
  :after org
  :ensure t
  :pin melpa
  ;this doesn't really work
  ;:custom (org-gcal-local-timezone "America/Managua")
  :config
  (require 'auth-source)
  (let ((gcal-auth (nth 0 (auth-source-search :host "api.google.com" :requires '(:login :password)))))
    (let ((gcal-secret (plist-get gcal-auth :secret)))
      (setq org-gcal-client-id (plist-get gcal-auth :user)
            org-gcal-client-secret (if (functionp gcal-secret) (funcall gcal-secret) gcal-secret))))
  (setq org-gcal-file-alist '(
                        ("twist.522@gmail.com" . "~/Dropbox/org/gcal/personal.org")
                        ("3fq436g1h8aigd0k0k5jtrv4po@group.calendar.google.com" . "~/Dropbox/org/gcal/sport.org")
                        ("0saojhu0tmsuhvii1vccddgvvk@group.calendar.google.com" . "~/Dropbox/org/gcal/routine.org")
                        ("d9tv5thudt39po9amct0m1jrag@group.calendar.google.com" . "~/Dropbox/org/gcal/nutrition.org")
                        ("family07835897960350574739@group.calendar.google.com" . "~/Dropbox/org/gcal/family.org")
                        ))
)

;; todo - this requires authinfo parse, need to run this after startup - e.g. on agenda open hook
;;(org-gcal-sync)

  ;; TODO
  ;;(add-to-list 'org-gcal-fetch-event-filters 'filter-gcal-event-maybe)

;(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
;(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))

;; org-timeline ;;
;;(use-package org-timeline)
;;(require 'org-timeline)
;;(add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append)

;; org columns finances ;;
(defun custom/org-collect-food (property)
  "Return `PROPERTY' for `food' entries."
  (let ((prop (org-entry-get nil property))
    (catgry (org-entry-get nil "CATGRY")))
    (if (and prop (string= "food" catgry))
    prop
      "0")))

(setq org-columns-summary-types
      '(("food+" org-columns--summary-sum
     custom/org-collect-food)))
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; jira ;;;;
(use-package org-jira
  :config
  (setq
   jiralib-url "http://timmedia.atlassian.net"
   jiralib-user-login-name "yurii.o@thetimmedia.com"
   org-jira-custom-jqls '(
                          (:jql "project = \"TIM\" and sprint = \"Tim Data Sprint 39\" and resolution = Unresolved order by rank asc"
                                :limit 20
                                :filename "sprint-board")
                          (:jql "project = \"TIM\" and sprint = \"Tim Data backlog\" and resolution = Unresolved order by rank asc"
                                :limit 20
                                :filename "tim-data-backlog")
                          )
   )
)

;; confluence support
;;(require 'ox-confluence)
;;;;;;;;;;;;;;

;;; ORG-MODE PRESENTATIONS ;;;
(use-package org-tree-slide
  :ensure t
  :bind (:map org-mode-map (
    ("C-c t t"   . org-tree-slide-mode)
    ("C-c t T d" . org-tree-slide-skip-done-toggle)
    ("C-c t T h" . org-tree-slide-display-header-toggle)
    ("C-c t P s" . org-tree-slide-simple-profile)
    ("C-c t P p" . org-tree-slide-presentation-profile)
    ("C-c t P n" . org-tree-slide-narrowing-control-profile)
)))

(use-package ox-reveal
  :config
  ;this works fine but no speaker notes and highlight plugins
  ;(setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  :custom
  (org-reveal-root "/home/twist/reveal.js")
  (org-reveal-reveal-js-version 4)
  (org-reveal-highlight-css "%r/plugin/highlight/zenburn.css"))

;;; babel ;;;
(require 'ob-clojure)
(unless (package-installed-p 'cider)
  (package-install 'cider))
(require 'cider)
(setq org-babel-clojure-backend 'cider)

(use-package htmlize)

(use-package ob-async)

;;;;; CALFW ;;;;;;
;; example - https://cestlaz.github.io/posts/using-emacs-26-gcal/#.WIqBud9vGAk
;; should use ical link - it works only if calendar is public

(use-package calfw-org)
(use-package calfw
  :commands cfw:open-calendar-buffer cfw:open-org-calendar
  :config
  (require 'calfw)
  (require 'calfw-org)
  (setq cfw:org-overwrite-default-keybinding t))

;;; local additional holidays to diplay through org-calendar-holiday func
(setq holiday-local-holidays '((holiday-fixed 5 22 "День вишиванки")))
(load-library "ukrainian-holidays")

;; prettify
;; todo - this destroys some of the org-mode and evil bindings for some reason
(add-hook 'org-mode-hook (lambda ()
   "Beautify Org Checkbox Symbol"
   (push '("[ ]" . "☐") prettify-symbols-alist)
   (push '("[X]" . "☑" ) prettify-symbols-alist)
   (push '("[-]" . "❍" ) prettify-symbols-alist)
   (push '("#+BEGIN_SRC" . "✎") prettify-symbols-alist) ;; ➤ 🖝 ➟ ➤ ✎
   (push '("#+END_SRC" . "⏹") prettify-symbols-alist) ;; ⏹ □
   (push '("[#A]" . "❗" ) prettify-symbols-alist)
   (push '("[#B]" . "⬆" ) prettify-symbols-alist)
   (push '("[#C]" . "❖" ) prettify-symbols-alist)
   (push '("[#D]" . "⬇" ) prettify-symbols-alist)
   (push '("<=" . "≤") prettify-symbols-alist)
   (push '("part_d" . "∂") prettify-symbols-alist)
   (push '("Gamma" . "Γ") prettify-symbols-alist)
   (push '("sigmoid" . "σ") prettify-symbols-alist)
   (prettify-symbols-mode)))

(defun yant/str-to-glyph (str)
  "Transform string into glyph, displayed correctly."
  (let ((composition nil))
    (dolist (char (string-to-list str)
    (nreverse (cdr composition)))
(push char composition)
(push '(Br . Bl) composition))))
	    ;(?▤ org-specific ":LOGBOOK:" (org-mode))
        ;(?⚙ org-specific ":PROPERTIES:" (org-mode))
        ;(?⏏ org-specific ":END:" (org-mode))
        ;((yant/str-to-glyph "☐") org-specific "\\(?:^*+ +\\)\\(\\<TODO\\>\\)" (org-mode) 1)
        ;((yant/str-to-glyph "☑") org-specific "\\(?:^*+ +\\)\\(\\<DONE\\>\\)" (org-mode) 1)
        ;((yant/str-to-glyph "✘") org-specific "\\(?:^*+ +\\)\\(\\<FAILED\\>\\)" (org-mode) 1)
        ;((yant/str-to-glyph "✘") org-specific "\\(?:^*+ +\\)\\(\\<CANCELLED\\>\\)" (org-mode) 1)
        ;((yant/str-to-glyph "▶") org-specific "\\(?:^*+ +\\)\\(\\<NEXT\\>\\)" (org-mode) 1)
        ;    ((yant/str-to-glyph "☇") org-specific "\\(?:^*+ +\\)\\(\\<MERGED\\>\\)" (org-mode) 1)
        ;((yant/str-to-glyph "⚑") org-specific "\\(?:^*+ +\\)\\(\\<WAITING\\>\\)" (org-mode) 1)
        ;((yant/str-to-glyph "♲") org-specific "\\(?:^*+ +\\)\\(\\<HOLD\\>\\)" (org-mode) 1)
        ;((yant/str-to-glyph "☠D") org-specific "\\<DEADLINE:" (org-mode))
        ;((yant/str-to-glyph "◴S") org-specific "\\<SCHEDULED:" (org-mode))))))

(use-package org-sidebar)

(use-package org-ql)

(defun my/org-ql-parents ()
  (interactive)
  (org-ql-search (org-agenda-files) '(todo) :super-groups '((:auto-parent t)))
)

(defun my/org-get-parent-goal ()
  (interactive)
  (-when-let* ((goal-link (org-entry-get (point) "GOAL" t)))
    (save-window-excursion
      (org-link-open-from-string goal-link)
      (org-get-heading 'notags 'notodo)
      )
  ))

(defun my/org-set-goal ()
  (interactive)
  ; todo
  )

(defun my/org-ql-goals ()
  (interactive)
  (org-ql-search (org-agenda-files) '(and (todo) (not (todo "GOAL")) (not (todo "VISION")))
      :super-groups '((:auto-map
                   (lambda (item)
                     (-when-let* ((goal-link (org-entry-get (org-super-agenda--get-marker item) "GOAL")))
                       (message goal-link)
                       (org-link-open-from-string goal-link)
                       (org-get-heading 'notags 'notodo)
                       )))))
)

(use-package org-super-agenda
  :config
 ;; (setq org-super-agenda-header-map (copy-keymap evil-org-agenda-mode-map))
  )

;; will create id on C-c C-l
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
(org-id-update-id-locations)

(use-package org-bullets
  :hook (org-mode . (lambda() (org-bullets-mode 1))))

(use-package org-roam
      :ensure t
      ;:hook (after-init . org-roam-mode)
      :custom (org-roam-directory "~/Dropbox/org/")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

;; feed (experiment)
;(setq org-feed-alist
;      '(("Slashdot"
;         "http://rss.slashdot.org/Slashdot/slashdot"
;         "~/Dropbox/org/feeds.org" "Slashdot Entries")))
;;===================================================================================================;
;;===================================================================================================;
;;===================================================================================================;
;;=============================         END ORG MODE      ===========================================;
;;===================================================================================================;
;;===================================================================================================;
;;===================================================================================================;


;; dired
;(with-eval-after-load "dired" (require 'dired-filter))
; dired buffers keep hanging around - this annoys me very much
(with-eval-after-load 'dired (evil-define-key 'normal dired-mode-map "q" 'kill-this-buffer))
;(add-hook 'dired-mode-hook #'dired-du-mode)
(use-package dired-avfs)
(use-package dired-filter
  :after dired
  :config
  (define-key dired-mode-map (kbd "F") dired-filter-map))
(use-package dired-hacks-utils)
(use-package dired-open)
(use-package dired-subtree)
(use-package dired-narrow)
;; this one produces "Permission denied" on listing in Win10 with JUNCTION folders
;;(use-package dired-collapse :hook (dired-mode . dired-collapse-mode))
(use-package dired-rainbow) 
;; too long to init
;;(use-package dired-du)
(use-package peep-dired
  :config
  (evil-define-key 'normal peep-dired-mode-map (kbd "<SPC>") 'peep-dired-scroll-page-down
                                             (kbd "C-<SPC>") 'peep-dired-scroll-page-up
                                             (kbd "<backspace>") 'peep-dired-scroll-page-up
                                             (kbd "j") 'peep-dired-next-file
                                             (kbd "k") 'peep-dired-prev-file)
  (add-hook 'peep-dired-hook 'evil-normalize-keymaps)
  ;:hook (dired-mode . peep-dired)
  )
(use-package ranger)

;;; ledger ;;;
(use-package ledger-mode
  :config (require 'flycheck-ledger)
  :custom (ledger-reconcile-default-commodity nil))
(use-package flycheck-ledger
  :after ledger-mode)
(use-package evil-ledger
  :after evil ledger-mode)
(fset 'euro
      (lambda (&optional arg) "Keyboard macro." (interactive "p")
        (kmacro-exec-ring-item (quote ([24 56 return 35 120 50 48 65 67 return] 0 "%d")) arg)))

;;;;;;; GIT ;;;;;;;
(use-package magit
  :commands magit-status magit-blame
  :init (setq magit-revert-buffers nil)
  ;;:custom (magit-credential-cache-daemon-socket "/home/twist/.git-credential-cache/socket")
  :config
  (require 'evil-magit)
  ;(require 'magit-gh-pulls)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :bind (("C-c g g" . magit-status)
         ("C-c g b" . magit-blame)
         ("C-c g c" . magit-clone)))

(use-package git-timemachine
  :bind ("C-c g t" . git-timemachine)
  :config
  (progn
     (evil-make-overriding-map git-timemachine-mode-map 'normal)
     ;; force update evil keymaps after git-timemachine-mode loaded
     (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))
  )

(use-package magit-gh-pulls
  :after magit
  ;:hook ((magit-mode . turn-on-magit-gh-pulls)
  ;       ;;(magit-mode . magit-gh-pulls-reload)
  ;       )
  :config
  (gh-auth-remember (gh-profile-current-profile) :token (auth-source-pass-get "oauth-token" "github.com/thatwist"))
  (gh-auth-remember (gh-profile-current-profile) :username "thatwist")
  )

(use-package evil-magit
  :after evil magit
  :config
  (setq evil-magit-state 'motion))

(setq ediff-window-setup-function 'ediff-setup-windows-plain) ; ediff use same frame

(use-package diff-hl
  :config
  :hook
  (prog-mode . turn-on-diff-hl-mode)
  (vc-dir-mode-hook . turn-on-diff-hl-mode))


(use-package company
  :commands company-mode
  :init
  (setq
   company-dabbrev-ignore-case nil
   company-dabbrev-code-ignore-case nil
   company-dabbrev-downcase nil
   company-idle-delay 0.5
   company-minimum-prefix-length 2)
  ;; :bind (:map company-active-map
  ;;             ("<return>" . company-complete-common)
  ;;             ("RET" . company-complete-common)
  ;;             ("C-SPC" . company-complete-selection))
  :config
  (global-company-mode 1)
  ;; testing this one, pretty awesome
  (add-to-list 'company-backends '(company-capf :with company-dabbrev))
  ;; following lines to make TAB call company-mode instead of completion-at-point
  (setq tab-always-indent 'complete)
  (defvar completion-at-point-functions-saved nil)
  (defun company-indent-for-tab-command (&optional arg)
    (interactive "P")
    (let ((completion-at-point-functions-saved completion-at-point-functions)
          (completion-at-point-functions '(company-complete-common-wrapper)))
      (indent-for-tab-command arg)))
  (defun company-complete-common-wrapper ()
    (let ((completion-at-point-functions completion-at-point-functions-saved))
      (company-complete-common)))
  (with-eval-after-load 'company
    (define-key company-mode-map [remap indent-for-tab-command] 'company-indent-for-tab-command)
    ;;(define-key company-active-map (kbd "M-n") nil)
    ;;(define-key company-active-map (kbd "M-p") nil)
    ;;(define-key company-active-map (kbd "C-n") #'company-select-next)
    ;;(define-key company-active-map (kbd "C-p") #'company-select-previous)
    (define-key company-mode-map (kbd "C-<space>") #'company-complete)
    ;;(define-key company-active-map (kbd "RET") #'company-complete-selection)
    (define-key company-active-map (kbd "<return>") #'company-complete-selection)
    (define-key company-active-map (kbd "<tab>") #'company-complete-common)
    (define-key company-active-map (kbd "TAB") #'company-complete-common)
    ;; to complete common and then cycle
    ;;(define-key company-active-map (kbd "C-n") (lambda () (interactive) (company-complete-common-or-cycle 1)))
    ;;(define-key company-active-map (kbd "C-p") (lambda () (interactive) (company-complete-common-or-cycle -1)))
    )
)

(use-package company-quickhelp)

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))



(use-package yasnippet
  :demand t
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :config
  (yas-reload-all)
  ;; default dir is ~/.emacs.d/snippets, others are somehow loaded from yasnippet-snippets
  (yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.
  ;;:bind ("<tab>" . yas-expand)
)

(use-package yasnippet-snippets
  :after yasnippet
  :ensure t)

(use-package ivy-yasnippet
  :after yasnippet)

(use-package aws-snippets
  :after yasnippet)

(defun yas/org-very-safe-expand ()
          "Safe TAB in 'org-mode' (see 'org-mode' conflicting packages documentation)."
          (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)))

;;;;; Scala ;;;;;
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\|c\\)$"
  :config
  (defun sp-restrict-c (sym)
    "Smartparens restriction on `SYM' for C-derived parenthesis."
    (sp-restrict-to-pairs-interactive "{([" sym))
  (bind-key "C-S-<tab>" 'dabbrev-expand scala-mode-map)
  (bind-key "s-<delete>" (sp-restrict-c 'sp-kill-sexp) scala-mode-map)
  (bind-key "s-<backspace>" (sp-restrict-c 'sp-backward-kill-sexp) scala-mode-map)
  (bind-key "s-<home>" (sp-restrict-c 'sp-beginning-of-sexp) scala-mode-map)
  (bind-key "s-<end>" (sp-restrict-c 'sp-end-of-sexp) scala-mode-map)
  :hook (scala-mode . (lambda()
  ;; Bind the 'newline-and-indent' command to RET (aka 'enter'). This
  ;; is normally also available as C-j. The 'newline-and-indent'
  ;; command has the following functionality: 1) it removes trailing
  ;; whitespace from the current line, 2) it create a new line, and 3)
  ;; indents it.  An alternative is the
  ;; 'reindent-then-newline-and-indent' command.
  (local-set-key (kbd "RET") 'newline-and-indent)

  ;; Alternatively, bind the 'newline-and-indent' command and
  ;; 'scala-indent:insert-asterisk-on-multiline-comment' to RET in
  ;; order to get indentation and asterisk-insertion within multi-line
  ;; comments.
  (local-set-key (kbd "RET")
                 '(lambda ()
                    (interactive)
                    (newline-and-indent)
                    (scala-indent:insert-asterisk-on-multiline-comment)))

  ;; Bind the backtab (shift tab) to
  ;; 'scala-indent:indent-with-reluctant-strategy command. This is usefull
  ;; when using the 'eager' mode by default and you want to "outdent" a
  ;; code line as a new statement.
  (local-set-key (kbd "<backtab>") 'scala-indent:indent-with-reluctant-strategy)

  ;; clean-up whitespace at save
  (make-local-variable 'before-save-hook)
  (add-hook 'before-save-hook 'whitespace-cleanup)

  (setq comment-start "/* "
	  comment-end " */"
	  comment-style 'multi-line
	  comment-empty-lines t)
  ;; turn on highlight. To configure what is highlighted, customize
  ;; the *whitespace-style* variable. A sane set of things to
  ;; highlight is: face, tabs, trailing
  (whitespace-mode)
  (show-paren-mode)
  (smartparens-mode)
  (yas-minor-mode)
  (company-mode)
  (scala-mode:goto-start-of-code)
)))

(use-package play-routes-mode
  :config
  (add-hook 'play-routes-mode-hook
               (lambda () (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|fixme\\|todo\\):" 1 font-lock-warning-face t)))))
)

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; Allows using space when in the minibuffer
  (substitute-key-definition
    'minibuffer-complete-word
    'self-insert-command
     minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
  :bind (:map sbt-mode-map
              ("<space>"  . sbt-hydra) ;; fixme
              ))

(use-package elpy
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  )

(use-package py-autopep8)

(use-package blacken)

;;;;; LSP ;;;;;
(use-package lsp-mode
  :init
  (setq lsp-prefer-flymake nil)
  (setq lsp-keymap-prefix "C-l")
  :hook (
         ; disable automatic lsp start to save resources
         ;(scala-mode . lsp-deferred)
         ;(java-mode . lsp-deferred)
         ;(js-mode . lsp-deferred)
         ;(xml-mode . lsp-deferred)
         ;(yaml-mode . lsp-deferred)
         ;(python-mode . lsp-deferred)
         ;(php-mode . lsp-deferred)
         ;(json-mode . lsp-deferred)
         ;(dockerfile-mode . lsp-deferred)
         ;(sh-mode . lsp-deferred)
         ;(html-mode . lsp-deferred)
         ;(css-mode . lsp-deferred)
         (lsp-mode . lsp-lens-mode))
  ;; waits too long when typing
  ;;:config (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  :config (setq lsp-signature-auto-activate nil)
  (require 'lsp-protocol)
  :commands (lsp lsp-deferred))

(use-package lsp-metals)

(use-package lsp-ui
  ;; this plays bad with customized at the bottom of init.el
  :custom
    ;(lsp-ui-doc-enable t)
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-doc-position 'top)
    (lsp-ui-doc-include-signature t)
    (lsp-ui-flycheck-enable t)
    (lsp-ui-flycheck-list-position 'bottom)
    (lsp-ui-flycheck-live-reporting t)
    (lsp-ui-sideline-enable t)
    (lsp-ui-sideline-ignore-duplicate t)
    (lsp-ui-sideline-show-symbol t)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-sideline-show-diagnostics t)
    (lsp-ui-sideline-show-code-actions t)
    (lsp-ui-sideline-code-actions-prefix " ")
    (lsp-ui-peek-enable t)
    (lsp-ui-peek-list-width 60)
    (lsp-ui-peek-peek-height 25)
    (lsp-ui-imenu-enable t)
    (lsp-ui-imenu-kind-position 'top)
  :bind (:map lsp-mode-map ("C-l m" . lsp-ui-imenu))
  :config
    (lsp-diagnostics-modeline-mode)
  :hook (lsp-mode . lsp-ui-mode)
  :commands (lsp-ui-mode)
  :after lsp-mode)

(use-package lsp-ivy
  :after lsp-mode
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :after lsp treemacs
  :config
  ;(lsp-metals-treeview-enable t)
  ;(lsp-treemacs-sync-mode 1)
  ;(setq lsp-metals-treeview-show-when-views-received t)
  :commands (lsp-treemacs-errors-list lsp-treemacs-references))

(use-package dap-mode
  :after lsp-mode posframe
  :config
  (dap-auto-configure-mode)
  (add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

;; java config taken from https://blog.jmibanez.com/2019/03/31/emacs-as-java-ide-revisited.html
(use-package lsp-java
  :init
  (defun java-mode-config ()
    (toggle-truncate-lines 1)
    (setq-local tab-width 4)
    (setq-local c-basic-offset 4))
  :config
  ;; Enable dap-java
  (require 'dap-java)
  (setq lombok-jar (expand-file-name "~/lombok/lombok.jar"))
  ;;(setq my/java-format-settings-file (expand-file-name "~/projects/defaultFormatterProfile.xml"))
  (setq lsp-java-vmargs (list "-noverify"
              "-Xmx2G"
              "-XX:+UseG1GC"
              "-XX:+UseStringDeduplication"
              (concat "-javaagent:" lombok-jar)
              (concat "-Xbootclasspath/a:" lombok-jar))
        lsp-java-import-order '["" "java" "javax" "#"]
        ;; Don't organize imports on save
        lsp-java-save-action-organize-imports nil
        ;; Formatter profile
        lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
  :hook (java-mode . java-mode-config)
  :after (lsp-mode dap-mode))

;; Lsp completion
(use-package company-lsp
  :after lsp-mode company
  :custom
  (company-lsp-cache-candidates t) ;; auto, t(always using a cache), or nil
  (company-lsp-async t)
  (company-lsp-enable-snippet t)
  (company-lsp-enable-recompletion t)
  :commands company-lsp)

;; todo - fetch this package from a given git hash when lsp-mode stable version was used
;;(use-package lsp-origami
;;  :after lsp
;;  :config
;;  (global-origami-mode)
;;  (add-hook 'origami-mode-hook #'lsp-origami-mode)
;;  :bind
;;  ("C-S-t" . origami-toggle-node)
;;  ("C-S-c" . origami-toggle-all-nodes))


(use-package typescript-mode)
(use-package php-mode)


;;; comments
(use-package evil-nerd-commenter
  :config
  (evilnc-default-hotkeys nil t)
  (evil-leader/set-key
    ";i" 'evilnc-comment-or-uncomment-lines)
)

;;;; elfeed - rss feeds ;;;;
(use-package elfeed
  :config
  (require 'elfeed-org)
  (require 'elfeed-goodies)
  (require 'elfeed-score)
  (add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
  (add-to-list 'evil-emacs-state-modes 'elfeed-show-mode))

(use-package elfeed-org
  :after elfeed
  :config
  (setq rmh-elfeed-org-files (list (concat org-directory "elfeed/feeds.org")))
  (defadvice elfeed (before configure-elfeed activate)
    "Load all feed settings before elfeed is started"
    (rmh-elfeed-org-configure))
  (elfeed-org)
  (elfeed-update))

(use-package elfeed-goodies
  :config (elfeed-goodies/setup))

(use-package elfeed-dashboard
  :bind ("C-c f" . elfeed-dashboard)
  :commands elfeed-dashboard
  :config
  (add-to-list 'evil-emacs-state-modes 'elfeed-dashboard-mode)
  (setq elfeed-dashboard-file (concat org-directory "elfeed/dashboard.org"))
  ;; update feed counts on elfeed-quit
  (advice-add 'elfeed-search-quit-window :after #'elfeed-dashboard-update-links))

(use-package elfeed-web)

(use-package elfeed-score
  :config (progn
    (elfeed-score-enable)
    (evil-define-key 'normal elfeed-search-mode-map "=" elfeed-score-map)))

;; testing
;;(use-package general)

;; TRAMP
;(use-package tramp ;; with use-package
;  :config
;  ;;(setq-default tramp-default-method "scp")
;  (setq remote-file-name-inhibit-cache nil) ;; set if editing outside of tramp as well
;  (setq vc-ignore-dir-regexp
;        (format "%s\\|%s"
;                      vc-ignore-dir-regexp
;                      tramp-file-name-regexp))
;  (setq tramp-verbose 4) ;; raise if debug tramp errors
;  ;;(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
;  (setq tramp-default-method "ssh")
;  (customize-set-variable 'tramp-syntax 'simplified)
;)

;; docker ;;
(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))
;;(use-package docker-tramp)


;; built-in
(use-package request
  :custom
  (request-log-level 'debug))

;; jupyter
;(use-package ein
;  :config
;  (require 'ein-jupyterhub))

(use-package 2048-game)


;;;; multimedia ;;;;
(use-package emms
  :config
  (emms-all)
  (emms-default-players)
  (define-emms-simple-player mplayer '(file url)
      (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
                    ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
                    ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
      "mplayer" "-slave" "-quiet" "-really-quiet" "-fullscreen")
  ;; (define-emms-simple-player afplay '(file)
  ;;     (regexp-opt '(".mp3" ".m4a" ".aac"))
  ;;     "afplay")
  ;;   (setq emms-player-list `(,emms-player-afplay))
)
;; todo - xterm colors for shell
;;https://github.com/atomontage/xterm-color

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(add-hook 'yaml-mode-hook
        (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
;;;;;
(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("todo"   . "#FF0000")
          ("FIXME"  . "#FF0000")
          ("fixme"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("why"    . "#FF4500")
          ("STUB"   . "#1E90FF")))
  (define-key hl-todo-mode-map (kbd "C-c H p") 'hl-todo-previous)
  (define-key hl-todo-mode-map (kbd "C-c H n") 'hl-todo-next)
  (define-key hl-todo-mode-map (kbd "C-c H o") 'hl-todo-occur)
  (define-key hl-todo-mode-map (kbd "C-c H i") 'hl-todo-insert)
  (add-hook 'prog-mode-hook 'hl-todo-mode)
)

;; integrates with evil-fold z-.. awesome!
;; !! do not use lsp-origami as it will depend on unstable lsp-mode packages
(use-package origami
  :config
  (add-hook 'prog-mode-hook 'origami-mode)
)

;; github gist integration
;;(use-package gist)

;; eshell
(require 'eshell)
(require 'em-smart) ; smart eshell features
(eshell-smart-initialize)
(use-package eshell-git-prompt ; use-theme ..
  :demand ;; todo - require lazily
  :config (eshell-git-prompt-use-theme 'powerline))

(defun eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

; ansi coloring in compilation-mode buffers (e.g. for dap-java-run-test-class)
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; testing this
(use-package shell-switcher
  :config
  (setq shell-switcher-mode t)
  (setq-default shell-switcher-new-shell-function 'eshell-new))

;; systemd-mode
(use-package systemd)

;; evil in terminal - cursor shapes (doesn't work in gui)
;(add-hook 'evil-insert-state-entry-hook (lambda () (send-string-to-terminal "\033[5 q")))
;(add-hook 'evil-normal-state-entry-hook (lambda () (send-string-to-terminal "\033[0 q")))

(if (eq system-type 'windows-nt)
  nil
  ;; terminal cursor (e.g. for evil)
  ;(quelpa '(term-cursor :repo "h0d/term-cursor.el" :fetcher github))
  ;;(global-term-cursor-mode)
  )

;; testing
(load-library "iscroll")

;; this doesn't work actually
;(add-to-list 'after-make-frame-functions
;             (lambda(&rest _)
;               (when (not (display-graphic-p))
;                 ;;(setq doom-modeline-icon -1) ;; todo - switch doommodeline icons somehow
;                 (term-cursor-mode)))) ;; cannot use (global-term-cursor-mode) with lsp-ui

;; blogging
(use-package ox-hugo
  :after ox org-capture
  :pin melpa
  :config
  ;; Populates only the EXPORT_FILE_NAME property in the inserted headline.
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ; Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n")))
  (add-to-list 'org-capture-templates
               '("h"                ;`org-capture' binding + h
                 "Hugo post"
                 entry
                 ;; It is assumed that below file is present in `org-directory'
                 ;; and that it has a "Blog Ideas" heading. It can even be a
                 ;; symlink pointing to the actual location of all-posts.org!
                 (file+olp "blog.org" "Blog Ideas")
                 (function org-hugo-new-subtree-post-capture-template))))

;; sql
(use-package ejc-sql
  ;;:after company-quickhelp - this makes it loading realy after explicit quickhelp load
  :config
  ;;(require 'ejc-sql)
  ;;(require 'ejc-autocomplete)
  (require 'ejc-company)
  (push 'ejc-company-backend company-backends)
  (add-hook 'ejc-sql-minor-mode-hook
            (lambda ()
              ;;(auto-complete-mode t)
              ;;(ejc-ac-setup)
              (ejc-eldoc-setup)
              (company-mode t)))
  (add-hook 'ejc-sql-connected-hook
            (lambda ()
              (ejc-set-fetch-size 200)
              (ejc-set-max-rows 200)
              ;(ejc-set-show-too-many-rows-message t)
              (ejc-set-column-width-limit 100)
              ;(ejc-set-use-unicode t)
              ))
  ;(setq ejc-result-table-impl 'ejc-result-mode)
  (setq ejc-result-table-impl 'orgtbl-mode)
  ;; this will change begin_example to lines starting with :
  (setq org-babel-min-lines-for-block-output 1000)
  ;; sets timeout for long queries, otherwise nrepl will fail (default is 10)
  (setq nrepl-sync-request-timeout 600)
  (global-set-key (kbd "C-c eb") 'ejc-get-temp-editor-buffer)
  (require 'company-quickhelp)
  (company-quickhelp-mode)
  (ejc-create-connection
    "presto"
    :subprotocol "presto"
    :dependencies [[com.facebook.presto/presto-jdbc "0.232"]]
    ;;:classpath (concat "~/.m2/repository/com/facebook/presto/presto-jdbc/0.232/" "presto-jdbc-0.232.jar")
    :connection-uri (concat
                    "jdbc:presto://presto-db.thetimmedia.site:8889/hive/default?"
                    "user=hadoop"))
  (ejc-create-connection
    "conf-db"
    :subprotocol "mysql"
    ;;:dependencies [[mysql/mysql-connector-java "6.0.5"]]
  
    :dependencies [[org.mariadb.jdbc/mariadb-java-client "2.6.2"]]
    ;;:classname "com.mysql.cj.jdbc.Driver"
    :classname "org.mariadb.jdbc.Driver"
    :connection-uri "jdbc:mariadb://conf-db.thetimmedia.site:3306/data"
    :user "app"
    :password (funcall (plist-get (nth 0 (auth-source-search :host "conf-db.thetimmedia.site" :require '(:user :secret))) :secret)))
  (ejc-create-connection
    "content-db"
    :subprotocol "mysql"
    :dependencies [[org.mariadb.jdbc/mariadb-java-client "2.6.2"]]
    :classname "org.mariadb.jdbc.Driver"
    :connection-uri "jdbc:mariadb://content-db.thetimmedia.site:3306/data"
    :user "admin"
    :password (funcall (plist-get (nth 0 (auth-source-search :host "content-db.thetimmedia.site" :require '(:user :secret))) :secret)))
  (ejc-create-connection
    "reporting-db"
    :subprotocol "postgresql"
    :classname "org.postgresql.Driver"
    :classpath "~/.m2/repository/postgresql/postgresql/9.3.1102.jdbc41/postgresql-9.3-1102.jdbc41.jar"
    :connection-uri "jdbc:postgresql://reporting-db.thetimmedia.site:3306/postgres"
    :user "postgres"
    :password (funcall (plist-get (nth 0 (auth-source-search :host "reporting-db.thetimmedia.site" :require '(:user :secret))) :secret)))
  (ejc-create-connection
    "redshift"
    :subprotocol "postgres"
    ;;:dependencies [[com.amazon.redshift/redshift-jdbc42 "1.2.37.1061"]]
    :classpath "/home/twist/.m2/repository/com/amazon/redshift/redshift-jdbc42/jar/redshift-jdbc42-jar.1.2.37.1061"
    :classname "com.amazon.redshift.jdbc42.Driver"
    :connection-uri "jdbc:redshift://redshift-db.thetimmedia.site:5439/dev"
    :user "adminelad"
    :password (funcall (plist-get (nth 0 (auth-source-search :host "redshift-db.thetimmedia.site" :require '(:user :secret))) :secret)))
  (ejc-create-connection
    "hive"
    :subprotocol "hive"
    ;;:dependencies [[org.apache.hive/hive-jdbc "2.3.6"]]
    ;;:classpath "/home/twist/.m2/repository/org/apache/hive/hive-jdbc/2.3.6/hive-jdbc-2.3.6.jar"
    ;;:classname "org.apache.hive.jdbc.HiveDriver"
    ;;in fact this path is not valid maven path, but for valid clomacs fails as it tries to resolve artifact in some remote inaccessible repo
    :classpath "/home/twist/.m2/repository/org/amazon/hive/hive-jdbc41-amazon/jar/hive-jdbc41-amazon-jar.2.6.2"
    :classname "com.amazon.hive.jdbc41.HS2Driver"
    :connection-uri "jdbc:hive2://presto-db.thetimmedia.site:10000/default"
    :user "hadoop")
  (ejc-create-connection
   "spark-test"
   :subprotocol "spark-sql"
   :classpath ["/home/twist/.m2/repository/org/amazon/spark/spark-jdbc41-amazon/alljars/hive-jdbc-1.2.1-spark2-amzn-1.jar"
               "/home/twist/.m2/repository/org/amazon/spark/spark-jdbc41-amazon/alljars/libthrift-0.9.3.jar"
               "/home/twist/.m2/repository/org/amazon/spark/spark-jdbc41-amazon/alljars/hadoop-common-2.8.5-amzn-5.jar"
               "/home/twist/.m2/repository/org/amazon/spark/spark-jdbc41-amazon/alljars/hive-metastore-1.2.1-spark2-amzn-1.jar"
               "/home/twist/.m2/repository/org/amazon/spark/spark-jdbc41-amazon/alljars/hive-exec-1.2.1-spark2-amzn-1.jar"
               "/home/twist/.m2/repository/org/amazon/spark/spark-jdbc41-amazon/alljars/spark-hive-thriftserver_2.11-2.4.4.jar"]
   :classname "org.apache.hive.jdbc.HiveDriver"
   :connection-uri "jdbc:hive2://spark2.thetimmedia.site:10001"
   :user "hadoop")
)

; trying sql.el
(use-package sql-presto
  ;:custom
  ; todo need supply proper options / password from pass store
  ;(sql-presto-options ("--output-format" "CSV_HEADER" ))
  ;(sql-presto-program "TRINO_PASSWORD=admin TZ=UTC trino")
  )

(use-package sqlformat)

;;; kredo-replace, kredo-cleanup
;(require 'ledger-kredo-regex)

;; i3wm
(use-package i3wm-config-mode
  :pin melpa)

;; profiler
(use-package esup
  :ensure t
  ;; To use MELPA Stable use ":pin mepla-stable",
  :pin melpa-stable
  :commands (esup))

;; ex-wm

(when (not (eq system-type 'windows-nt))
  (require 'exwm)
  (require 'exwm-config)
  (setq exwm-input-global-keys `(,(kbd "s-&") .
                               (lambda (command)
                                 (interactive (list (read-shell-command "$ ")))
                                 (start-process-shell-command command nil command))))
  (require 'exwm-systemtray)
  ;; didn't work
  ;;(exwm-systemtray-enable)
  ;; uncomment to start exwm
  ;;(exwm-config-default)
)

;; vterm
(use-package vterm)

;; matrix
;(use-package matrix-client
;  :quelpa (matrix-client :fetcher github :repo "alphapapa/matrix-client.el"
;                         :files (:defaults "logo.png" "matrix-client-standalone.el.sh")))

;; docop
;;(use-package docopt)

;; colors
(use-package rainbow-mode)

; doesn't work
;(use-package go-translate
;  :config
;  (setq go-translate-local-language "en")
;  (setq go-translate-target-language "uk")
;  (setq go-translate-extra-directions '(("uk" . "ru") ("ru" . "en")))
;  (setq go-translate-buffer-follow-p t)       ; focus the result window
;  (setq go-translate-buffer-source-fold-p t)  ; fold the source text in the result window
;  (setq go-translate-buffer-window-config nil) ; config the result window as your wish
;  (setq go-translate-debug-p t)
;  (global-set-key "\C-ct" 'go-translate)
;  (global-set-key "\C-cT" 'go-translate-popup))

; crazy, finally it works
(use-package google-translate
  ;:ensure t
  ;:demand t
  :init (require 'google-translate)
  ;:functions (google-translate--search-tkk)
  :config
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
  ;(require 'google-translate-smooth-ui)
  ;(setq google-translate-backend-method 'curl)
  (setq google-translate-input-method-auto-toggling t)
  (setq google-translate-preferable-input-methods-alist '((nil . ("en"))
                                                         (ukrainian-computer . ("ru" "uk"))))
  (setq google-translate-translation-directions-alist
      '(("uk" . "en") ("ru" . "en") ("en" . "uk")))
  :custom
  ;(google-translate--tkk-url "http://translate.google.com/")
  ;(google-translate-base-url "http://translate.google.com/")
  ;(google-translate-backend-debug t)
  (google-translate-show-phonetic t)
  :bind
  ("C-c T" . google-translate-smooth-translate)
  ("C-c t" . google-translate-at-point)
  )

;; mail

(when (not (eq system-type 'windows-nt))
  (progn
    (require 'mu4e)
    ;; use mu4e for e-mail in emacs
    (setq mail-user-agent 'mu4e-user-agent)
    (setq mu4e-sent-folder (concat message-directory "sent"))
    (setq mu4e-drafts-folder (concat message-directory "drafts"))
    (setq mu4e-trash-folder (concat message-directory "trash"))))

;; smtp mail setting; these are the same that `gnus' uses.
(setq
   message-send-mail-function 'smtpmail-send-it
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587
   smtpmail-stream-type 'starttls
   ;smtpmail-default-smtp-server "smtp.example.com"
   ;smtpmail-local-domain        "example.com"
   )
;; gnus
(setq gnus-select-method '(nnimap "gmail"
          (nnimap-address "imap.gmail.com")
          (nnimap-server-port 993)
          (nnmail-expiry-wait immediate)))

; msgs
(use-package telega)

; nice pure lisp find-grep replacement - works on windows well
(use-package xah-find)

;; keep customize settings in their own file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)
