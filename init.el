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

(if (eq system-type 'window-nt)
  (setenv "HOME" "C:\\Users\\Admin")
  nil)

;; fonts
(if (eq system-type 'windows-nt)
  ;;nil ;; todo - windows font
  (set-face-attribute 'default nil :font "Source Code Pro" :height 150) ;; defaults to 139
  (set-face-attribute 'default nil :font "Source Code Pro Medium"))

(set-fontset-font t 'latin "Noto Sans")
;; something for icons?
(setq inhibit-compacting-font-caches t)

;; this is like changing with C-x C-+, todo - test
(setq default-frame-alist '((font . "Source Code Pro-14")))

;; modes
;;(electric-indent-mode 0)
;; omg how could I live without this - to remove selection (if active) when inserting text
(delete-selection-mode 1)
(menu-bar-mode -1)
(fringe-mode 20)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(save-place-mode 1) ; remember file position in the visited previously file

(require 'desktop)
(setq desktop-load-locked-desktop t) ; do not ask that lock-file exists, this fixes the issue with emacs daemon waiting for answer
(desktop-save-mode 1)

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

(use-package bug-hunter)

;; experimental - minibuffer within minibuffer
(setq enable-recursive-minibuffers t)

;; EasyPG encryption
(require 'epa-file)
(epa-file-enable)
;; used for prompts on gpg
(use-package pinentry)

(setq auth-sources '("~/.authinfo.gpg" "~/.netrc"))

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

;;;; SMOOTH SCROLLING ;;;;
;;(pixel-scroll-mode t)

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
;;(when (display-graphic-p)
;;  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
;;        mouse-wheel-progressive-speed nil))
;;(setq scroll-step 1
;;      scroll-margin 0
;;      scroll-conservatively 100000)

;;(setq redisplay-dont-pause t
;;  scroll-margin 1
;;  scroll-step 1
;;  scroll-conservatively 10000
;;  scroll-preserve-screen-position 1)

;;(setq
;; scroll-conservatively 1000                     ;; only 'jump' when moving this far
;; scroll-margin 4                                ;; scroll N lines to screen edge
;; scroll-step 1                                  ;; keyboard scroll one line at a time
;; mouse-wheel-scroll-amount '(6 ((shift) . 1))   ;; mouse scroll N lines
;; mouse-wheel-progressive-speed nil              ;; don't accelerate scrolling
;;
;; redisplay-dont-pause t                         ;; don't pause display on input
;;
;; ;; Always redraw immediately when scrolling,
;; ;; more responsive and doesn't hang!
;; fast-but-imprecise-scrolling nil
;; jit-lock-defer-time 0
;; )

;; works best so far, scroll 1 line always
;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
;;(setq mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
;;(setq-default smooth-scroll-margin 0)
;;(setq scroll-step 1
;;      scroll-margin 1
;;      scroll-conservatively 100000)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)((meta)) ((control) . text-scale))) ;; one line at a time
(setq mouse-wheel-progressive-speed t);;nil ;; (not) accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;(use-package smooth-scroll
;;  :config
;;  (smooth-scroll-mode -1)
;;  (setq smooth-scroll/vscroll-step-size 2)
;;  )

(use-package display-line-numbers
  :custom (global-display-line-numbers-mode t))

(use-package page-break-lines
  :after display-line-numbers
  :config
  (global-page-break-lines-mode)
  ;; todo - fix width of line
  (set-fontset-font "fontset-default"
                  (cons page-break-lines-char page-break-lines-char)
                  (face-attribute 'default :family))
)

;;(use-package beacon
;;  :custom
;;  (beacon-color "#f1fa8c")
;;  :hook (after-init . beacon-mode))
  

;;;;;;; DASHBOARD ;;;;;;;;;

(use-package dashboard
  :after all-the-icons
  :demand
  :preface
  (defun dashboard-performance-statement (list-size)
    (insert (all-the-icons-faicon "check" :height 1.2 :v-adjust 0.0 :face 'font-lock-keyword-face))
    (insert (propertize " Think" 'face 'dashboard-heading))
    (insert (propertize "\n\t★ SLEEP\n\t★ ROUTINE\n\t★ NUTRITION\n\t★ SPORT\n\t★ REST" 'face '(:height 110))))
  :custom
  (dashboard-banner-logo-title "With Great Power Comes Great Responsibility")
  (dashboard-startup-banner nil) ;; 1,2,3,'logo,'official
  (dashboard-center-content t)
  (dashboard-items '((performance)
                     (agenda . 5)
                     (recents  . 5)
                     (projects . 5)
                     (bookmarks . 5)
                     (registers . 5)))
  ;;(dashboard-navigator-buttons
  ;;    `(;; line1
  ;;      ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
  ;;       "Homepage"
  ;;       "Browse homepage"
  ;;       (lambda (&rest _) (browse-url "homepage")))
  ;;      ("★" "Star" "Show stars" (lambda (&rest _) (show-stars)) warning)
  ;;      ("?" "" "?/h" #'show-help nil "<" ">"))
  ;;       ;; line 2
  ;;      ((,(all-the-icons-faicon "linkedin" :height 1.1 :v-adjust 0.0)
  ;;        "Linkedin"
  ;;        ""
  ;;        (lambda (&rest _) (browse-url "homepage")))
  ;;       ("⚑" nil "Show flags" (lambda (&rest _) (message "flag")) error))))
  (dashboard-set-file-icons t)
  (dashboard-set-heading-icons t)
  (dashboard-set-init-info t)
  (dashboard-set-navigator t)
  :config
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (add-to-list 'dashboard-item-generators '(performance . dashboard-performance-statement))
  (dashboard-setup-startup-hook)
  )
;;;;;;;;;;;;;;

;;; CSV-MODE ;;;
(use-package csv-mode)
;;;;;;;;;;;;;;;;

(use-package which-key
  :custom (which-key-idle-delay 0.5)
  :config (which-key-mode))

(if (window-system) (progn (global-hl-line-mode 1) ;;; highlight current line
          ;;(set-face-background hl-line-face "gray87")
                           (setq doom-modeline-icon t)
                           )
  )

(use-package ace-window
  :ensure t
  :defer t
  :init
  (progn
    (global-set-key (kbd "M-p") 'ace-window)
    ))

;; show indents in all modes
(use-package indent-guide
  :config (indent-guide-global-mode 1))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(add-hook 'prog-mode-hook #'subword-mode)

(require 'whitespace)
(setq whitespace-line-column 120) ;; limit line length
(setq whitespace-style '(face)) ;; lines-tail

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
  :commands
  smartparens-strict-mode
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair
  :init
  (setq sp-interactive-dwim t)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  (smartparens-global-mode 1)
  (sp-pair "(" ")" :wrap "C-(") ;; how do people live without this?
  (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
  (sp-pair "{" "}" :wrap "C-{")

  ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/543
  (bind-key "C-S-<left>" nil smartparens-mode-map)
  (bind-key "C-S-<right>" nil smartparens-mode-map)

  (bind-key "s-<delete>" 'sp-kill-sexp smartparens-mode-map)
  (bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map))

;;(use-package etags-select :commands etags-select-find-tag)

(use-package s)

(use-package expand-region
  :commands 'er/expand-region)

(sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
(sp-local-pair 'scala-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
(sp-local-pair 'scala-mode "{" nil :post-handlers '(:add ("||\n[i]" "RET")))

(defun sp-restrict-c (sym)
  "Smartparens restriction on `SYM' for C-derived parenthesis."
  (sp-restrict-to-pairs-interactive "{([" sym))

(bind-key "s-{" 'sp-rewrap-sexp smartparens-mode-map)

;;; FLYCHECK ;;;;;
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


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


(use-package all-the-icons
  :demand
  :ensure t)

;; icons in menu
(use-package mode-icons
  :config (mode-icons-mode -1))

;; modeline
(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode)
      :config
      (set-face-attribute 'mode-line nil :height 95)
      ;(set-face-attribute 'mode-line-inactive nil :height 50)
      (setq doom-modeline-height 25)
      ;(setq doom-modeline-bar-width 5)
)

;;;;;;;;;;; IVY ;;;;;;;;;;;;
(use-package flx
  :ensure t)

(use-package wgrep
  :ensure t)

(use-package wgrep-ag
  :ensure t)

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

(use-package ivy-rich
  :demand
  :after counsel
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode)
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
  (setq all-the-icons-ivy-file-commands
      '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir))
  ;;(all-the-icons-ivy-setup)
  )

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; help ;;;
(use-package helpful)

;; discover-my-major ;;
(use-package discover-my-major
  :ensure t
  :commands (discover-my-major)
  ;; this one conflicts with help+
  :bind ("C-h C-m" . discover-my-major)
  :config
  (add-to-list 'evil-emacs-state-modes 'makey-key-mode)
)

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
  ;; this doesn't work, eh..
  ;;(eval-after-load "evil-maps"
  ;;  (dolist (map '(evil-motion-state-map
  ;;               evil-insert-state-map
  ;;               evil-emacs-state-map))
  ;;    (define-key (eval map) [tab] nil)))
  ;; disable evil in help mode (emacs by default)
  (define-key evil-motion-state-map [tab] nil)
  (add-to-list 'evil-emacs-state-modes 'debugger-mode)
  (evil-set-initial-state 'Info-mode 'emacs)
  (evil-set-initial-state 'process-menu-mode 'emacs)
  (evil-set-initial-state 'dashboard-mode 'emacs)
  ;;(evil-set-initial-state 'dired-mode 'emacs)
  ;;(evil-set-initial-state 'special-mode 'emacs)
  ;;(evil-set-initial-state 'messages-major-mode 'emacs)
  ;; conflict in terminal mode (because C-i and TAB is not distinguishable in terminal, C-i is evil jump forward)
  (add-hook 'org-mode-hook                                                                      
          (lambda ()                                                                          
        (define-key evil-normal-state-map (kbd "TAB") 'org-cycle))) 
)

(use-package evil-leader
  :demand
  :after evil
  :config
  (global-evil-leader-mode)
  (progn
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "s" 'save-buffer
      "b" 'switch-to-buffer
      "f" 'find-file
      "I" 'find-user-init-file
      "F" 'hydra-flycheck/body
      "B" 'hydra-btoggle/body
      "y" 'hydra-yasnippet/body
      "j" 'hydra-avy/body
      "p" 'hydra-projectile/body
      "(" 'hydra-smartparens/body
      "g" 'hydra-magit/body
      "m" 'hydra-smerge/body
      "w" 'hydra-windows/body
      "O" 'hydra-folding/body
      "n" 'hydra-next-error/body
      "o" 'hydra-org/body
      "e" 'eshell-new
      "a" 'org-agenda
      "i" 'org-capture
      "l" 'hydra-lsp/body
      "S" 'sbt-hydra
      "t" 'treemacs
      "h" 'hydra-s/body
      "M" 'evil-mc-mode
      "c" 'hydra-org-clock/body
      "v" 'er/expand-region
      "<SPC>" 'other-window
      "qq" 'save-buffers-kill-terminal
      "qQ" 'save-buffers-kill-emacs)))

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
  (evil-define-key 'motion org-agenda-mode-map "ZK" 'org-habit-toggle-habits)
  (evil-define-key 'motion org-agenda-mode-map "Zk" 'org-habit-toggle-display-in-agenda)
  (evil-define-key 'motion org-agenda-mode-map "ZD" 'org-agenda-toggle-deadlines)
)

(use-package evil-mc
  :after evil)

(use-package evil-collection
  :after evil
  :demand
  :custom (evil-collection-setup-minibuffer t)
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

;; dired
;(with-eval-after-load "dired" (require 'dired-filter))
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
(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))
(use-package dired-rainbow) 
;; too long to init
;;(use-package dired-du)

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

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

(use-package super-save
  :demand
  :config
  (super-save-mode +1)
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  (add-to-list 'super-save-triggers 'ivy-switch-buffer)
  ;; save on find-file
  (add-to-list 'super-save-hook-triggers 'find-file-hook))

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
;;;;;;;;;;;;;;;;;;;;;;;

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

;;Hydra / BToggle
;;Group a lot of commands.
(pretty-hydra-define hydra-btoggle
  (:hint nil :color amaranth :quit-key "q" :title (with-faicon "toggle-on" "Toggle" 1 -0.05))
  ("Basic"
   (("a" abbrev-mode "abbrev" :toggle t)
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
    ("f" magit-file-popup "file popup")
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
    ("k" kill-buffer-and-window "kill buffer and window" :exit t))
   "Frame"
   (("fk" delete-frame "delete frame")
    ("fo" delete-other-frames "delete others")
    ("fn" make-frame-command "make frame"))
   "Size"
   (("b" balance-windows "balance")
    ("H" shrink-window-horizontally "narrow")
    ("J" shrink-window "lower")
    ("K" enlarge-window "heighten")
    ("L" enlarge-window-horizontally "widen")
    ("S" switch-window-then-swap-buffer "swap" :color teal))
   "Zoom"
   (("-" text-scale-decrease "out")
    ("+" text-scale-increase "in")
    ("=" (text-scale-increase 0) "reset"))))

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

(pretty-hydra-define hydra-org
  (:hint nil :color teal :quit-key "q" :title (with-fileicon "org" "Org" 1 -0.05))
  ("Action"
   (
    ("a" org-agenda "agenda")
    ("j" hydra-org-clock/body "clock")
    ("O" hydra-org-agenda/body "agenda hydra")
    ("c" org-capture "capture")
    ("g" org-gcal-fetch "gcal fetch")
    ("G" org-gcal-sync "gcal sync")
    ("L" org-store-link "store-link")
    ("l" org-insert-link-global "insert-link")
    ("A" org-archive-done-in-file "archive done in file")
    ("d" org-decrypt-entry "decrypt")
    ("I" org-info-find-node "org info find")
    ("k" org-cut-subtree "cut-subtree")
    ("o" org-open-at-point-global "open-link")
    ("r" org-refile "refile")
    ("t" org-show-todo-tree "todo-tree"))))

(defhydra hydra-org-clock (:color blue :hint nil)
   "
^Clock:^ ^In/out^     ^Edit^   ^Summary^    | ^Timers:^ ^Run^           ^Insert
-^-^-----^-^----------^-^------^-^----------|--^-^------^-^-------------^------
(_?_)    p_i_ck       _e_dit   _J_ goto     | (_z_)     _r_elative      ti_m_e
 ^ ^     _I_n         _q_uit   _d_isplay    |  ^ ^      cou_n_tdown     i_t_em
 ^ ^     _o_ut        _j_ump   _r_eport     |  ^ ^      _p_ause toggle
 ^ ^     _c_ontinue   ^ ^      ^ ^          |  ^ ^      _s_top
"

   ("i" org-mru-clock-in)
   ("I" org-clock-in)
   ("c" org-clock-in-last)
   ("o" org-clock-out)
   
   ("e" org-clock-modify-effort-estimate)
   ("q" org-clock-cancel)

   ("j" org-mru-clock-select-recent-task)
   ("J" org-clock-goto)
   ("d" org-clock-display)
   ("r" org-clock-report)
   ("?" (org-info "Clocking commands"))

  ("r" org-timer-start)
  ("n" org-timer-set-timer)
  ("p" org-timer-pause-or-continue)
  ("s" org-timer-stop)

  ("m" org-timer)
  ("t" org-timer-item)
  ("z" (org-info "Timers")))

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
                        :hint  nil)
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

(setq org-directory "~/Dropbox/org")
;;(setq org-agenda-files (quote ("~/org/agendas.org")))

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
** NEXT %?\n\
** TODO review %\\1 \n\
   SCHEDULED: <%<%Y-%m-%d %a .+14d>>\n\
   :PROPERTIES:\n\
   :Effort: %^{Effort}\n\
   :END:\n\
** _IDEAS_")
        ("h" "Habit" entry (file+headline "~/Dropbox/org/personal.org" "*habits*") "\
* NEXT %?\n\
  SCHEDULED: <%<%Y-%m-%d %a .+1d>>\n\
  :PROPERTIES:\n\
  :CREATED: %U\n\
  :STYLE: habit\n\
  :REPEAT_TO_STATE: NEXT\n\
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
        ("t" "Personal task" entry (file+olp "~/Dropbox/org/personal.org" "_TASKS_") "** NEXT %i%?\n   SCHEDULED: <%<%Y-%m-%d %a>>")
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
        (sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "WAITING(w@/!)" "DELEGATED(e@/!)" "ON-HOLD(h@/!)" "|")
        (sequence "MAYBE(m)" "SOMEDAY(s)" "DISCOVERY(D)" "PROJECT(p)" "|")
        (sequence "GOAL(g)" "|")
        (sequence "|" "DONE(d!)" "CLOSED(c@/!)" "CANCELLED(C@/!)")
        )
)

;; Setting Colours (faces) for todo states to give clearer view of work 
;; lookup by M-x list-colors-display
(setq org-todo-keyword-faces
      '(
        ("PROJECT" . "maroon2")
        ("GOAL" . "SeaGreen4")
        ("TODO" . "orange red")
        ("NEXT" . "cyan4")
        ("IN-PROGRESS" . "dark goldenrod")
        ("WAITING" . "blue violet")
        ("DELEGATED" . "dark olive green")
        ("ON-HOLD" . "orange")
        ("DONE" . "forest green")
        ("CLOSED" . "cyan4")
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

;; custom agendas ;;
(setq org-agenda-custom-commands
      '(("c" . "Custom Agendas")
        ("cB" "Blocking others" ((tags "+blocking/!")) nil nil)
        ("ct" "Today" ((agenda "" ((org-agenda-span 1))) nil) nil)
        ("cT" "All Todo" ((tags-todo "-project")) nil nil)
        ("cA" "Appointments" agenda* nil nil)
        ("cW" "Waiting for" ((todo "WAITING")) nil nil)
        ("cd" "Delegated" ((todo "DELEGATED")) nil nil)
        ("cN" "Done" ((todo "DONE|CANCELLED|CLOSED")) nil nil)
        ("cu" "Unscheduled"
         ((tags-todo "-project"
              ((org-agenda-overriding-header "\nUnscheduled TODO")
               (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp)))))
           nil
           nil)
        ("ci" "All In Progress" ((todo "IN-PROGRESS")) ((org-agenda-max-entries 15)) nil)
        ("cn" "All Next" ((todo "NEXT")) ((org-agenda-max-entries 15)) nil)
        ("ce" "Next for each project (TODO)" ((todo "NEXT")) nil nil)
        ("cp" "Projects" ((tags-todo "+project")) nil nil)
        ("cg" "Goals" ((todo "GOAL")) nil nil)
        ("cs" "Stuck Projects" ((stuck "")) nil nil)
        ("ca" "Areas" ((tags "+area")) nil nil)
        ("cb" "Buylist" ((tags-todo "+buy")) nil nil)
        ("co" "Books" ((tags-todo "+book")) nil nil)
        ("cD" "Deep" ((tags-todo "+deep")) nil nil)
        ("ck" "Deep work" ((tags-todo "+deep+work")) nil nil)
        ("ch" "Habits" tags-todo "STYLE=\"habit\""
          ((org-agenda-overriding-header "Habits")
          (org-agenda-sorting-stragety
            '(todo-state-down effort-up category-keep))))
        ("c," "Process" ((tags-todo "-deep-project")) nil nil)
        ;; testing
        ;;("f" occur-tree "\\<FIXME\\>")
        ;;("b" "Buylist(-tree doesn't work?)" ((tags-tree "+buy")) nil nil)
        ;;("w" "Waiting for(-tree doesn't work?)" ((todo-tree "WAITING")) nil nil)
        ("A" "Current" (
         (tags "+blocking/!" ((org-agenda-overriding-header "Blocking others")))
         (tags-todo "-project+PRIORITY=\"A\"" ((org-agenda-overriding-header "Most important")))
         (todo "DELEGATED" ((org-agenda-overriding-header "Delegated")))
         (todo "WAITING" ((org-agenda-overriding-header "Waiting for")))
         (tags-todo "-project+PRIORITY=\"B\"|-project+PRIORITY=\"C\""
              ((org-agenda-overriding-header "Unscheduled B-C")
               (org-agenda-max-entries 20)
               (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))))
         (tags-todo "+project+PRIORITY=\"A\"|-project+PRIORITY=\"B\""
                    ((org-agenda-overriding-header "Projects")
                     (org-agenda-max-entries 15)))
         (agenda "" ((org-agenda-span 1) (org-agenda-overriding-header "Today")))
        ))
        ("W" "Weekly Review"
         ((agenda "" ((org-agenda-span 7))); review upcoming deadlines and appointments
                                           ; type "l" in the agenda to review logged items 
          (stuck "") ; review stuck projects as designated by org-stuck-projects
          (todo "PROJECT") ; review all projects (assuming you use todo keywords to designate projects)
          (todo "MAYBE|SOMEDAY") ; review someday/maybe items
          (todo "WAITING"))) ; review waiting items
        ))

(setq org-refile-targets `(
                           (nil :maxlevel . 9)
                           ((,(concat org-directory "/english.org"),(concat org-directory "/org.org"),(concat org-directory "/knowledge.org")) :maxlevel . 9)
                           (org-agenda-files :maxlevel . 5)
                           ))
(setq org-outline-path-complete-in-steps nil)          ; Refile in a single go
(setq org-refile-use-outline-path 'file)               ; Show full paths for refiling - trick to refile in 0 level
(setq org-refile-allow-creating-parent-nodes 'confirm) ; create new parent on the fly

;; archived location
(setq org-archive-location "~/Dropbox/org/archive/%s_archive::")

;; inheritance
(setq org-tags-exclude-from-inheritance (quote ("project" "area")))

;; log into LOGBOOK
(setq org-log-into-drawer "LOGBOOK")

;; effort & column view

;;(setq org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS")
(setq org-columns-default-format-for-agenda "%60ITEM(Task) %6Effort(Estim){:}")
(add-to-list 'org-global-properties '("Effort_ALL" . "0 0:05 0:10 0:15 0:25 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00 10:00 15:00 24:00"))

;; org plantuml
(use-package plantuml-mode)
;; Enable plantuml-mode for PlantUML files
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

;; babel with plantuml ;;
;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)
   (shell . t)))

(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("sql" "sh"))))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(setq org-plantuml-jar-path (expand-file-name "~/plantuml/plantuml.jar"))
(setq plantuml-jar-path (expand-file-name "~/plantuml/plantuml.jar"))
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
  (setq alert-default-style 'libnotify))

(use-package org-alert)

(use-package org-pomodoro
  :ensure t
  :commands (org-pomodoro)
  :config
  (require 'org-pomodoro-pidgin)
  (setq org-pomodoro-manual-break t)
)

;; org-pomodoro mode hooks
(add-hook 'org-pomodoro-finished-hook
          (lambda ()
          (alert "Pomodoro completed!" "Time for a break.")))

(add-hook 'org-pomodoro-break-finished-hook
          (lambda ()
            (alert "Pomodoro Short Break Finished" "Ready for Another?")
            (interactive)  
            (org-pomodoro '(16))))


(add-hook 'org-pomodoro-long-break-finished-hook
          (lambda ()
            (alert "Pomodoro Long Break Finished" "Ready for Another?")))

(add-hook 'org-pomodoro-break-finished-hook
  (lambda ()
    (interactive)  
    (org-pomodoro '(16))))  

(add-hook 'org-pomodoro-killed-hook
          (lambda ()
          (alert "Pomodoro Killed" "One does not simply kill a pomodoro!")))

;;;;;;;;;;;;;;; ORG-GCAL ;;;;;;;;;;;;;;;;

(setq package-check-signature nil)

;; trying to fix encoding problem
;;(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
;;  (set-language-environment 'utf-8)
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

(defun filter-routine-sleep-events (event)
  "Filters out 'sleep' events."
)

(use-package org-gcal
  :after org
  :ensure t
  :pin melpa
  :config
  (require 'auth-source)
  (let ((gcal-auth (nth 0 (auth-source-search :host "api.google.com" :requires '(:login :password)))))
    (let ((gcal-secret (plist-get gcal-auth :secret)))
      (setq org-gcal-client-id (plist-get gcal-auth :user)
            org-gcal-client-secret (if (functionp gcal-secret) (funcall gcal-secret) gcal-secret))))
  (setq org-gcal-file-alist '(
                        ("twist.522@gmail.com" . "~/Dropbox/org/gcal/personal.org")
                        ("3fq436g1h8aigd0k0k5jtrv4po@group.calendar.google.com" . "~/Dropbox/org/gcal/sport.org")
                        ;; these two are noisy in agenda view
                        ;;("0saojhu0tmsuhvii1vccddgvvk@group.calendar.google.com" . "~/Dropbox/org/gcal/routine.org")
                        ;;("d9tv5thudt39po9amct0m1jrag@group.calendar.google.com" . "~/Dropbox/org/gcal/nutrition.org")
                        ("family07835897960350574739@group.calendar.google.com" . "~/Dropbox/org/gcal/family.org")
                        ("yostapchuk@romexsoft.com" . "~/Dropbox/org/gcal/romex.org")
                        ("t2511af1c9haf3l3rnimbfb0nrqrurc1@import.calendar.google.com" . "~/Dropbox/org/gcal/tim.org")
                        ))
)

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
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
)

;;; babel ;;;
(require 'ob-clojure)
(unless (package-installed-p 'cider)
  (package-install 'cider))
(require 'cider)
(setq org-babel-clojure-backend 'cider)

;;;;; CALFW ;;;;;;
;; example - https://cestlaz.github.io/posts/using-emacs-26-gcal/#.WIqBud9vGAk
;; should use ical link - it works only if calendar is public

(use-package calfw-org)
(use-package calfw
  :config
  (require 'calfw)
  (require 'calfw-org)
  (setq cfw:org-overwrite-default-keybinding t))

;;; local additional holidays to diplay through org-calendar-holiday func
(setq holiday-local-holidays '(
        (holiday-fixed 5 22 "День вишиванки")
       ))

;;===================================================================================================;
;;===================================================================================================;
;;===================================================================================================;
;;=============================         END ORG MODE      ===========================================;
;;===================================================================================================;
;;===================================================================================================;
;;===================================================================================================;

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
  :config
  (require 'evil-magit)
  (require 'magit-gh-pulls)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :bind (("C-c g g" . magit-status)
         ("C-c g b" . magit-blame)
         ("C-c g c" . magit-clone)
         ("C-c g f" . magit-file-popup)))

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
  :hook ((magit-mode . turn-on-magit-gh-pulls)
         ;;(magit-mode . magit-gh-pulls-reload)
         ))

(use-package evil-magit
  :after evil magit
  ;;:init
  :config
  (setq evil-magit-state 'motion))

(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package yasnippet
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

  ;;(require 'whitespace)
  ;; clean-up whitespace at save
  (make-local-variable 'before-save-hook)
  (add-hook 'before-save-hook 'whitespace-cleanup)

  (setq comment-start "/* "
	  comment-end " */"
	  comment-style 'multi-line
	  comment-empty-lines t)

  ;;(setq
  ;;  company-dabbrev-ignore-case nil
  ;;  company-dabbrev-code-ignore-case nil
  ;;  company-dabbrev-downcase nil
  ;;  company-idle-delay 0
  ;;  company-minimum-prefix-length 4)
    ;; disables TAB in company-mode, freeing it for yasnippet
  ;;(define-key company-active-map [tab] nil)
  ;;(define-key company-active-map (kbd "TAB") nil)
  ;; turn on highlight. To configure what is highlighted, customize
  ;; the *whitespace-style* variable. A sane set of things to
  ;; highlight is: face, tabs, trailing
  (whitespace-mode)
  (show-paren-mode)
  (smartparens-mode)
  (yas-minor-mode)
  (company-mode)
  (scala-mode:goto-start-of-code)
))
)

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
  :config (setq lsp-signature-auto-activate 0)
  (require 'lsp-protocol)
  :commands (lsp lsp-deferred))

(use-package lsp-metals)

(use-package lsp-ui
  ;; this plays bad with customized at the bottom of init.el
  :custom
    (lsp-ui-doc-enable t)
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
  :commands (lsp-treemacs-errors-list lsp-treemacs-references)
)

(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(use-package posframe)

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


;;;; elfeed - rss feeds ;;;;
(use-package elfeed
  :bind ("C-c f" . elfeed)
  :config
  (add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
  (add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)
)

(use-package elfeed-org
  :init
      (setq rmh-elfeed-org-files (list "~/Dropbox/org/elfeed.org"))
      (defadvice elfeed (before configure-elfeed activate)
        "Load all feed settings before elfeed is started"
        (rmh-elfeed-org-configure))
  :config
  (elfeed-org)
)

;; TRAMP
(use-package tramp ;; with use-package
  :config
  ;;(setq-default tramp-default-method "scp")
  (setq remote-file-name-inhibit-cache nil) ;; set if editing outside of tramp as well
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s"
                      vc-ignore-dir-regexp
                      tramp-file-name-regexp))
  (setq tramp-verbose 4) ;; raise if debug tramp errors
  ;;(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
)

;; docker ;;
(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))
;;(use-package docker-tramp)

(use-package 2048-game)

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
(use-package gist)

;; gnus
(setq
 send-mail-function 'smtpmail-send-it
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-stream-type 'starttls
 smtpmail-smtp-service 587
 gnus-select-method
 '(nnimap "gmail"
          (nnimap-address "imap.gmail.com")
          (nnimap-server-port 993)
          (nnmail-expiry-wait immediate)))

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

;; quelpa
(use-package quelpa)
(if (eq system-type 'windows-nt)
  nil
  ;; terminal cursor (e.g. for evil)
  (quelpa '(term-cursor :repo "h0d/term-cursor.el" :fetcher github))
  (global-term-cursor-mode))

;; blogging
(use-package ox-hugo
  :after ox)

(use-package company-quickhelp)

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
              (ejc-set-fetch-size 50)
              (ejc-set-max-rows 50)
              ;(ejc-set-show-too-many-rows-message t)
              (ejc-set-column-width-limit 50)
              ;(ejc-set-use-unicode t)
              ))
  ;(setq ejc-result-table-impl 'ejc-result-mode)
  (setq ejc-result-table-impl 'orgtbl-mode)
  ;; this will change begin_example to lines starting with :
  (setq org-babel-min-lines-for-block-output 1000)
  ;; sets timeout for long queries, otherwise nrepl will fail (default is 10)
  (setq nrepl-sync-request-timeout 60)
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
)

;;; kredo-replace
(autoload 'ledger-kredo-replace "~/Dropbox/org/ledger/kredo-regex.el")
;(load-file (expand-file-name "kredo-regex.el"))

;;;;;;;;;;;;;;;
;;(custom-set-faces
 ;;'(region ((t (:background "LightSalmon1" :distant-foreground "gtk_selection_fg_color")))))

;; custom ;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-highlight-search t t)
 '(ag-reuse-buffers t t)
 '(ansi-color-names-vector
   ["#3c3836" "#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(company-lsp-async t t)
 '(company-lsp-cache-candidates t t)
 '(company-lsp-enable-recompletion t t)
 '(company-lsp-enable-snippet t t)
 '(custom-enabled-themes '(gruvbox))
 '(custom-safe-themes
   '("850213aa3159467c21ee95c55baadd95b91721d21b28d63704824a7d465b3ba8" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" default))
 '(dashboard-banner-logo-title "With Great Power Comes Great Responsibility")
 '(dashboard-center-content t)
 '(dashboard-items
   '((performance)
     (agenda . 5)
     (recents . 5)
     (projects . 5)
     (bookmarks . 5)
     (registers . 5)))
 '(dashboard-set-file-icons t)
 '(dashboard-set-heading-icons t)
 '(dashboard-set-init-info t)
 '(dashboard-set-navigator t)
 '(dashboard-startup-banner nil)
 '(diredp-hide-details-initially-flag nil)
 '(evil-collection-setup-minibuffer t)
 '(fringe-mode 20 nil (fringe))
 '(global-display-line-numbers-mode t)
 '(guess-language-languages '(en nl) t)
 '(help-window-select t)
 '(ibuffer-saved-filter-groups
   '(("ibuffer-groups"
      ("org"
       (directory . "Dropbox/org"))
      ("memengine"
       (directory . "tim/memengine"))
      ("spark"
       (directory . "tim/spark")))))
 '(ibuffer-saved-filters
   '(("programming"
      (or
       (derived-mode . prog-mode)
       (mode . ess-mode)
       (mode . compilation-mode)))
     ("text document"
      (and
       (derived-mode . text-mode)
       (not
        (starred-name))))
     ("TeX"
      (or
       (derived-mode . tex-mode)
       (mode . latex-mode)
       (mode . context-mode)
       (mode . ams-tex-mode)
       (mode . bibtex-mode)))
     ("web"
      (or
       (derived-mode . sgml-mode)
       (derived-mode . css-mode)
       (mode . javascript-mode)
       (mode . js2-mode)
       (mode . scss-mode)
       (derived-mode . haml-mode)
       (mode . sass-mode)))
     ("gnus"
      (or
       (mode . message-mode)
       (mode . mail-mode)
       (mode . gnus-group-mode)
       (mode . gnus-summary-mode)
       (mode . gnus-article-mode)))))
 '(ivy-count-format "(%d/%d) ")
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate 'full)
 '(js-indent-level 2)
 '(json-reformat:indent-width 2)
 '(ledger-reconcile-default-commodity nil t)
 '(ledger-reports
   '(("last-month-balance" "ledger [[ledger-mode-flags]] -f /home/twist/Dropbox/org/ledger/ledger.dat --monthly bal ^expenses -X UAH -p \"last month\"")
     ("last-month-expenses" "ledger [[ledger-mode-flags]] -f /home/twist/Dropbox/org/ledger/ledger.dat reg ^expenses -X UAH -p \"last month\" --monthly")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(lsp-flycheck-live-reporting t t)
 '(lsp-ui-doc-enable t t)
 '(lsp-ui-doc-include-signature t t)
 '(lsp-ui-doc-position 'top t)
 '(lsp-ui-doc-use-childframe t t)
 '(lsp-ui-flycheck-enable t t)
 '(lsp-ui-flycheck-list-position 'right t)
 '(lsp-ui-flycheck-live-reporting t t)
 '(lsp-ui-imenu-enable t t)
 '(lsp-ui-imenu-kind-position 'top t)
 '(lsp-ui-peek-enable t t)
 '(lsp-ui-peek-list-width 60 t)
 '(lsp-ui-peek-peek-height 25 t)
 '(lsp-ui-sideline-code-actions-prefix "💡" t)
 '(lsp-ui-sideline-enable t t)
 '(lsp-ui-sideline-ignore-duplicate t t)
 '(lsp-ui-sideline-show-code-actions t t)
 '(lsp-ui-sideline-show-diagnostics t t)
 '(lsp-ui-sideline-show-hover t t)
 '(lsp-ui-sideline-show-symbol t t)
 '(org-agenda-files
   '("~/Dropbox/org/goals.org" "~/Dropbox/org/ucu-summer-school.org" "~/Dropbox/org/consume.org" "~/Dropbox/org/talks.org" "~/Dropbox/org/orgzly.org" "~/Dropbox/org/gcal/sport.org" "~/Dropbox/org/gcal/romex.org" "~/Dropbox/org/gcal/personal.org" "~/Dropbox/org/gcal/tim.org" "~/Dropbox/org/tim.org" "~/Dropbox/org/ucu-scala.org" "~/Dropbox/org/ideas.org" "~/Dropbox/org/music.org" "~/Dropbox/org/work.org" "~/Dropbox/org/psycho.org" "~/Dropbox/org/ptashka.org" "~/Dropbox/org/employment.org" "~/Dropbox/org/sport.org" "~/Dropbox/org/health.org" "~/Dropbox/org/food.org" "~/Dropbox/org/personal.org" "~/Dropbox/org/inbox.org" "~/Dropbox/org/emacs.org" "~/Dropbox/org/car.org" "~/Dropbox/org/blog.org"))
 '(org-agenda-tags-column -120)
 '(org-default-priority 67)
 '(org-extend-today-until 2)
 '(org-gcal-down-days 7)
 '(org-gcal-up-days 7)
 '(org-habit-graph-column 60)
 '(org-habit-show-all-today nil)
 '(org-highest-priority 65)
 '(org-journal-date-format "%A, %d %B %Y" t)
 '(org-journal-dir "~/Dropbox/org/journal/" t)
 '(org-journal-enable-agenda-integration t t)
 '(org-journal-file-type 'weekly t)
 '(org-lowest-priority 68)
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m org-expiry org-notify))
 '(org-tags-column -100)
 '(package-selected-packages
   '(auto-complete parseedn dashboard-mode company-quickhelp ox-hugo ox-huge term-cursor quelpa shell-switcher systemd systemd-mode eshell-git-prompt dired lsp-metals edit-server xclip sudo-edit pinentry gist org-gcal org-timeline org-plus-contrib company-lsp flycheck-ledger evil-ledger org-alert w3m origami hl-todo yasnippet-snippets which-key wgrep-ag wgrep shrink-path scala-mode sbt-mode request-deferred paredit org-mru-clock org-journal memoize makey ivy-rich flx evil-surround evil-mc evil-magit evil-leader evil-collection evil-cleverparens emms elfeed-org elfeed doom-modeline discover-my-major dired-subtree dired-rainbow dired-open dired-narrow dired-hacks-utils dired-filter dired-collapse dired-avfs deferred csv-mode counsel-projectile bui annalist all-the-icons-ivy all-the-icons ag ejc-sql bug-hunter ripgrep bash-mode typescript-mode projectile evil-org gruvbox-theme flycheck 2048-game company-box aws-snippets posframe php-mode ox-reveal org-tree-slide major-mode-hydra dashboard ivy-hydra counsel diff-hl helpful plantuml-mode magit-gh-pulls github-pullrequest super-save theme-changer dracula-theme nimbus-theme git-gutter-mode emacs-terraform-mode company-terraform docker groovy-mode docker-tramp docker-compose-mode org-jira calfw-gcal calfw-ical calfw-org calfw hydra htmlize dockerfile-mode org-pomodoro dired-ranger ranger dired-atool rainbow-delimiters multiple-cursors avy ace-jump-mode indent-guide mode-icons pyenv-mode elpy markdown-preview-mode yaml-mode exec-path-from-shell avk-emacs-themes atom-one-dark-theme markdown-mode use-package smooth-scroll smartparens popup-imenu play-routes-mode magit highlight-symbol git-timemachine git-gutter expand-region))
 '(projectile-completion-system 'ivy)
 '(projectile-project-search-path '("~/Documents"))
 '(safe-local-variable-values
   '((org-hugo-footer . "

[//]: # \"Exported with love from a post written in Org mode\"
[//]: # \"- https://github.com/kaushalmodi/ox-hugo\"")
     (checkdoc-minor-mode . t)
     (flycheck-disabled-checkers emacs-lisp-checkdoc)
     (eval visual-line-mode t)))
 '(tab-always-indent 'complete)
 '(treemacs-fringe-indicator-mode t)
 '(which-key-add-column-padding 3)
 '(which-key-allow-evil-operators t)
 '(which-key-idle-delay 0.5)
 '(which-key-max-description-length 50)
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:background "#333355" :foreground "blue3" :width extra-expanded))))
 '(diff-hl-delete ((t (:inherit diff-removed :foreground "red3" :width extra-expanded))))
 '(diff-hl-insert ((t (:inherit diff-added))))
 '(fringe ((t (:background "#282828" :weight extra-bold :height 2.0 :width ultra-expanded))))
 '(markdown-code-face ((t (:inherit fixed-pitch :background "gray25"))))
 '(region ((t (:extend t :background "dark slate blue")))))
