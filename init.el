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

;; fonts
(set-face-attribute 'default nil :font "Source Code Pro Medium")
(set-fontset-font t 'latin "Noto Sans")
;; something for icons?
(setq inhibit-compacting-font-caches t)

;; modes
;;(electric-indent-mode 0)
;; omg how could I live without this - to remove selection (if active) when inserting text
(delete-selection-mode 1)
(menu-bar-mode 1)
(scroll-bar-mode 1)
(tool-bar-mode -1)
(page-break-lines-mode 1)
(desktop-save-mode 1)

(global-unset-key (kbd "C-z"))

;; redefine mouse-2 ?
;;(define-key key-translation-map (kbd "<s-mouse-1>") (kbd "<mouse-2>"))

;; define binding lookup for init.el
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

;; define binding for init.el
(global-set-key (kbd "C-c I") 'find-user-init-file)

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
    ("melpa-milkbox" . "http://melpa.milkbox.net/packages/")
    ("marmalade" . "https://marmalade-repo.org/packages/")
    ("melpa-stable" . "http://stable.melpa.org/packages/"))
  ;; prefer stable unless explicit dep on melpa
  package-archive-priorities '(("melpa-stable" . 1))
)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Enable defer and ensure by default for use-package
;; Keep auto-save/backup files separate from source code
(setq
  use-package-always-defer t
  use-package-always-ensure t
  auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "auto-save/") t))
  backup-directory-alist `(("." . ,(expand-file-name (concat user-emacs-directory "backups"))))
)

;; EasyPG encryption
(require 'epa-file)
(epa-file-enable)

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
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;(use-package smooth-scroll
;;  :config
;;  (smooth-scroll-mode -1)
;;  (setq smooth-scroll/vscroll-step-size 2)
;;  )

;;;;;;; DASHBOARD ;;;;;;;;;
(use-package dashboard
  :after all-the-iconds
  :if (< (length command-line-args) 2)
  :preface
  (defun dashboard-load-packages (list-size)
    (insert (make-string (ceiling (max 0 (- dashboard-banner-length 38)) 5) ? )
            (format "%d packages loaded in %s" (length package-activated-list) (emacs-init-time))))
  :custom
  (dashboard-banner-logo-title "With Great Power Comes Great Responsibility")
  (dashboard-center-content t)
  (dashboard-items '((packages)
                     (agenda)
                     (projects . 5)))
  (dashboard-navigator-buttons
   `(
     (,(and (display-graphic-p)
            (all-the-icons-faicon "gitlab" :height 1.2 :v-adjust -0.1))
      "Homepage"
      "Browse Homepage"
      (lambda (&rest _) (browse-url homepage)))
     (,(and (display-graphic-p)
            (all-the-icons-material "update" :height 1.2 :v-adjust -0.24))
      "Update"
      "Update emacs"
      (lambda (&rest _) (auto-package-update-now)))))
  (dashboard-set-file-icons t)
  (dashboard-set-heading-icons t)
  (dashboard-set-init-info nil)
  (dashboard-set-navigator t)
  (dashboard-startup-banner 'logo)
  :config
  (add-to-list 'dashboard-item-generators '(packages . dashboard-load-packages))
  (dashboard-setup-startup-hook))
;;;;;;;;;;;;;;;;

;;; CSV-MODE ;;;
(use-package csv-mode)
;;;;;;;;;;;;;;;;

(use-package which-key
  :config (which-key-mode))

(if (window-system) (progn (global-hl-line-mode 1) ;;; highlight current line
          ;;(set-face-background hl-line-face "gray87")
          ))

;; show indents in all modes
(use-package indent-guide
  :config (indent-guide-global-mode 1))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'subword-mode)

(require 'whitespace)
(setq whitespace-line-column 120) ;; limit line length
(setq whitespace-style '(face)) ;; lines-tail

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  :bind ("C-c h" . highlight-symbol))


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

(use-package expand-region
  :commands 'er/expand-region)

(sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
(sp-local-pair 'scala-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))

(defun sp-restrict-c (sym)
  "Smartparens restriction on `SYM' for C-derived parenthesis."
  (sp-restrict-to-pairs-interactive "{([" sym))

(bind-key "s-{" 'sp-rewrap-sexp smartparens-mode-map)

;;; FLYCHECK ;;;;;
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


(use-package projectile
  :pin melpa
  :init   (setq projectile-use-git-grep t)
  :config
  (use-package counsel-projectile
    :config (counsel-projectile-mode))
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :custom (projectile-completion-system 'ivy)
)


(use-package treemacs
  :ensure t
  :defer t
  :pin melpa
  :bind (:map global-map ("C-x t t"   . treemacs))
  :commands treemacs-modify-theme
)

(use-package treemacs-evil
  :after treemacs evil
  :pin melpa
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :pin melpa
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :pin melpa
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-persp
  :after treemacs persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))


(use-package all-the-icons
  :defer t
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

;; smex ;;
;;(use-package smex
;;  :bind (
;;         ("M-x" . smex)
;;         ;; use C-h f, M-., C-h w - while in command mode
;;         ("M-X" . smex-major-mode-commands)
;;         ;; old M-x
;;         ("C-c C-x M-x" . execute-extended-command)
;;         )
;;  )
;;;;;;;;;;

;;;;;;;;;;; IVY ;;;;;;;;;;;;
(use-package flx
  :ensure t)

(use-package wgrep
  :ensure t)

(use-package wgrep-ag
  :ensure t)

(use-package counsel
  :after ivy
  :defer 1
  :config (counsel-mode)
  :bind (
         ("M-x" . counsel-M-x)
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
         ("M-y" . counsel-yank-pop)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h l" . counsel-find-library)
         ("C-x C-f" . counsel-find-file)
         ))

(use-package ivy
  :defer 0.1
  :diminish
  :bind (
         ("C-c C-r" . ivy-resume)
         ("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode)
  (setq ivy-re-builders-alist
        '(
          (ivy-switch-buffer . ivy--regex-fuzzy)
          (counsel-ag        . ivy--regex-plus)
          (counsel-git-grep  . ivy--regex-plus)
          (swiper            . ivy--regex-plus)
          (t                 . ivy--regex-fuzzy)))
  ;; all fuzzy init
  ;;(setq ivy-initial-inputs-alist nil)
  ;;(setq ivy-use-virtual-buffers t)
  ;;(setq ivy-count-format "(%d/%d) ")
  ;;(global-set-key (kbd "C-c i") 'counsel-info-lookup-symbol)
  ;;(global-set-key (kbd "C-c s u") 'counsel-unicode-char)
  ;;(global-set-key (kbd "C-c s v") 'counsel-set-variable)
  ;;(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  ;;(global-set-key (kbd "C-c v") 'ivy-push-view)
  ;;(global-set-key (kbd "C-c V") 'ivy-pop-view)
)

(use-package ivy-hydra
  :ensure t
  :after ivy)

(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(use-package ag
  :ensure t
  :custom
  (ag-highlight-search t)
  (ag-reuse-buffers t)
  :config
  (add-to-list 'ag-arguments "--word-regexp"))

(use-package all-the-icons-ivy
  :ensure t
  :pin melpa)

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
(use-package helpful
  :ensure t
  :bind ("C-h f" . helpful-callable)
        ("C-h v" . helpful-variable)
        ("C-h k" . helpful-key))

;; discover-my-major ;;
(use-package discover-my-major
  :ensure t
  :commands (discover-my-major)
  ;; this one conflicts with help+
  :bind ("C-h C-m" . discover-my-major)
  :config
  (add-to-list 'evil-emacs-state-modes 'makey-key-mode)
)

(use-package avy
  :ensure t
  :bind (("C-'" . avy-goto-char-2)
         ("C-\"" . avy-goto-char-timer)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
)

;;;;;;;;;;;;; EVIL MODE ;;;;;;;;;;;;;;
(use-package evil
  :pin melpa
  :config
  (evil-mode 1)
  ;; disable evil in help mode (emacs by default)
  (evil-set-initial-state 'Info-mode 'emacs)
  (evil-set-initial-state 'special-mode 'emacs)
  (evil-set-initial-state 'messages-major-mode 'emacs)
)

(use-package evil-leader
  :pin melpa
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
      "b" 'switch-to-buffer
      "w" 'save-buffer
      "f" 'find-file
      "v" 'er/expand-region)
)

(use-package evil-org
  :pin melpa
  :after evil org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode (lambda() (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-define-key 'motion org-agenda-mode-map "ZK" 'org-habit-toggle-habits)
  (evil-define-key 'motion org-agenda-mode-map "ZD" 'org-agenda-toggle-deadlines)
)

(use-package evil-mc
  :pin melpa
  :after evil
  :ensure t)

;; evil surround - https://github.com/emacs-evil/evil-surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;;;; DIRED+ ;;;;;
(add-to-list 'load-path (expand-file-name "dired-plus" user-emacs-directory))
(require 'dired+)
;;;;;;;;;;;;;;;;;

(use-package company
  :diminish company-mode
  :commands company-mode
  :init
  (setq
   company-dabbrev-ignore-case nil
   company-dabbrev-code-ignore-case nil
   company-dabbrev-downcase nil
   company-idle-delay 0
   company-minimum-prefix-length 4)
  :config
  ;; disables TAB in company-mode, freeing it for yasnippet
  ;(define-key company-active-map [tab] nil)
  ;(define-key company-active-map (kbd "TAB") nil)
  (global-company-mode 1)
)

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1)
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  ;; save on find-file
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
)


;;;;;;; THEMES ;;;;;;;;
;; (load-theme 'dracula t)
;; (load-theme 'atom-one-dark t)
;; (load-theme 'avk-dark-blue-yellow t)
;; (load-theme 'nimbus-theme t)
;; (load-theme 'dracula-theme t)
;; (load-theme 'solarized-theme t)
;; (load-theme 'zenburn t)
;;(load-theme 'gruvbox t)
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


;;;;; org-mode ;;;;;
(require 'org)

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cL" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cog" 'org-gcal-fetch)
(define-key global-map "\C-coG" 'org-gcal-sync)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-c\C-x\C-j" 'org-clock-goto)
;; unset - C-tab used for window cycling
(define-key org-mode-map [(control tab)] nil)

(setq org-directory "~/Dropbox/org")
;;(setq org-agenda-files (quote ("~/org/agendas.org")))

(setq org-capture-templates
      '(
        ("i" "Todo [inbox]" entry (file "~/Dropbox/org/inbox.org" ) "* TODO %i%?")
        ("p" "Project" entry (file "~/Dropbox/org/inbox.org")
         "* PROJECT *%^{Project title}* :%^G:project:\n:PROPERTIES:\n:CREATED: %U\n:END:\n  %^{Project description}\n  *goals*\n  %^{Project goals}\n** TODO %?\n** TODO review %^{Project title}\nSCHEDULED: <%<%Y-%m-%d %a .+14d>>\n** _IDEAS_\n" :clock-in t :clock-resume t)
        ("h" "Habit" entry (file+headline "~/Dropbox/org/personal.org" "*habits*")
         "* NEXT %?\nSCHEDULED: <%<%Y-%m-%d %a .+1d>>\n:PROPERTIES:\n:CREATED: %U\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:LOGGING: DONE(!)\n:ARCHIVE: %%_archive::* Habits\n:END:\n%U\n")
        ("B" "Budget entry" entry (file+olp "~/Dropbox/org/personal.org" "*finance*" "*budgeting*" "finance Oct 2019")
         "* %^{Entry description}\n  :PROPERTIES:\n  :AMOUNT:   %^{Amount}\n  :CURRENCY: UAH\n  :DATETIME:  %U\n  :CATEGORY:  %^{Category}\n  :TYPE:     CASH\n  :END:\n")
        ("a" "Appointment" entry (file  "~/Dropbox/org/gcal.org" ) "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
        ("e" "Word [english]" entry (file "~/Dropbox/org/english.org" ) "* %i%?")
        ("o" "Org idea" entry (file+olp "~/Dropbox/org/org.org" "ideas" "org ideas") "*** TODO %i%?")
        ("b" "Buylist" entry (file+olp "~/Dropbox/org/personal.org" "*buylist*") "** TODO %i%?")
        ("t" "Personal task" entry (file+olp "~/Dropbox/org/personal.org" "_TASKS_") "** NEXT %i%?\n   SCHEDULED: <%<%Y-%m-%d %a>>")
        ("I" "Idea" entry (file "~/Dropbox/org/ideas.org" ) "* %i%?")
        ("E" "Emacs todo" entry (file+headline "~/Dropbox/org/emacs.org" "ideas / todo") "* TODO %i%?")
        ))

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

(setq org-log-done t)

(setq org-todo-keywords
      '(
        (sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "WAITING(w@/!)" "DELEGATED(e@/!)" "ON-HOLD(h@/!)" "|")
        (sequence "MAYBE(m)" "SOMEDAY(s)" "DISCOVERY(D)" "PROJECT(p)" "|")
        (sequence "|" "DONE(d!)" "CLOSED(c@/!)" "CANCELLED(C@/!)")
        )
)

;; Setting Colours (faces) for todo states to give clearer view of work 
;; lookup by M-x list-colors-display
(setq org-todo-keyword-faces
      '(
        ("PROJECT" . "maroon2")
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
        ("cs" "Stuck Projects" ((stuck "")) nil nil)
        ("ca" "Areas" ((tags "+area")) nil nil)
        ("cb" "Buylist" ((tags-todo "+buy")) nil nil)
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
         (tags-todo "+project+PRIORITY=\"A\"" ((org-agenda-overriding-header "Projects") (org-agenda-max-entries 15)))
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

;; ` allows to use , to evaluate only that part of expr
(setq org-refile-targets `(
                           (nil :maxlevel . 9)
                           ((,(concat org-directory "/english.org"),(concat org-directory "/org.org")) :maxlevel . 9)
                           (org-agenda-files :maxlevel . 5)
                           ))
(setq org-outline-path-complete-in-steps nil)          ; Refile in a single go
(setq org-refile-use-outline-path 'file)               ; Show full paths for refiling - trick to refile in 0 level
(setq org-refile-allow-creating-parent-nodes 'confirm) ; create new parent on the fly

;; inheritance
(setq org-tags-exclude-from-inheritance (quote ("project" "area")))

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
   (plantuml . t)))

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
(defun org-archive-done-in-file
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file)
)
(define-key org-mode-map "\C-cC-xs" 'org-archive-done-in-file)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; todo - capture templates
;; (setq org-capture-templates
;; '(("a" "Appointment" entry (file  "~/Dropbox/orgfiles/gcal.org" )
;; "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
;; ("l" "Link" entry (file+headline "~/Dropbox/orgfiles/links.org" "Links")
;; "* %? %^L %^g \n%T" :prepend t)
;; ("b" "Blog idea" entry (file+headline "~/Dropbox/orgfiles/i.org" "Blog Topics:")
;; "* %?\n%T" :prepend t)
;; ("t" "To Do Item" entry (file+headline "~/Dropbox/orgfiles/i.org" "To Do")
;; "* TODO %?\n%u" :prepend t)
;; ("n" "Note" entry (file+headline "~/Dropbox/orgfiles/i.org" "Note space")
;; "* %?\n%u" :prepend t)
;; ("j" "Journal" entry (file+datetree "~/Dropbox/journal.org")
;; "* %?\nEntered on %U\n  %i\n  %a")
;; ("s" "Screencast" entry (file "~/Dropbox/orgfiles/screencastnotes.org")
;; "* %?\n%i\n")))

;;;; persist history of clock-in clock-out between emacs shutdowns
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(use-package org-mru-clock
  :ensure t
  :bind* (("C-c C-x i" . org-mru-clock-in)
          ("C-c C-x j" . org-mru-clock-select-recent-task))
  :init
  (setq org-mru-clock-how-many 20
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

(use-package org-pomodoro
  :ensure t
  :commands (org-pomodoro)
  :config
    (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))))

;; Needs terminal-notifier (brew install terminal-notifier)
(defun notify-osx (title message)
  (if (eq system-type 'darwin)
    (call-process "terminal-notifier"
                nil 0 nil
                "-group" "Emacs"
                "-title" title
                "-sender" "org.gnu.Emacs"
                "-message" message)))
                
;; org-pomodoro mode hooks
(add-hook 'org-pomodoro-finished-hook
          (lambda ()
          (notify-osx "Pomodoro completed!" "Time for a break.")))

(add-hook 'org-pomodoro-break-finished-hook
          (lambda ()
            (notify-osx "Pomodoro Short Break Finished" "Ready for Another?")
            (interactive)  
            (org-pomodoro '(16))))


(add-hook 'org-pomodoro-long-break-finished-hook
          (lambda ()
            (notify-osx "Pomodoro Long Break Finished" "Ready for Another?")))

(add-hook 'org-pomodoro-break-finished-hook
  (lambda ()
    (interactive)  
    (org-pomodoro '(16))))  

(add-hook 'org-pomodoro-killed-hook
          (lambda ()
          (notify-osx "Pomodoro Killed" "One does not simply kill a pomodoro!")))

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
  :ensure t
  :config
  (setq org-gcal-client-id "263074072231-eki1erdqom0jjd37b40m9nc71s811fgo.apps.googleusercontent.com"
  org-gcal-client-secret "qNDd3ekB-r6pNp-O12HOaS29"
  org-gcal-file-alist '(
                        ("twist.522@gmail.com" .  "~/Dropbox/org/gcal.org")
                        ("3fq436g1h8aigd0k0k5jtrv4po@group.calendar.google.com" .  "~/Dropbox/org/gcal_sport.org")
                        ; these two are noisy in agenda view
                        ;("0saojhu0tmsuhvii1vccddgvvk@group.calendar.google.com" .  "~/Dropbox/org/gcal_routine.org")
                        ;("d9tv5thudt39po9amct0m1jrag@group.calendar.google.com" .  "~/Dropbox/org/gcal_nutrition.org")
                        ;("y.ostapchuk@rickerlyman.com" .  "~/Dropbox/org/gcal_rlr.org")
                        ("yostapchuk@romexsoft.com" .  "~/Dropbox/org/gcal_romex.org")
                        ))
  ;; TODO
  ;;(add-to-list 'org-gcal-fetch-event-filters 'filter-gcal-event-maybe)
)

;(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
;(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))


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
  (setq jiralib-url "https://jira.com")
  (setq jiralib-user-login-name "yurii.ostapchuk")
)
;; confluence support
(require 'ox-confluence)
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

;;;;; CALFW ;;;;;;
;; example - https://cestlaz.github.io/posts/using-emacs-26-gcal/#.WIqBud9vGAk
;; should use ical link - it works only if calendar is public

(use-package calfw-org
  :ensure t
)
(use-package calfw
  :ensure t
  :config
  (require 'calfw)
  (require 'calfw-org)
  (setq cfw:org-overwrite-default-keybinding t)
)

;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'yaml-mode-hook
        (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;;;;;; GIT ;;;;;;;
(use-package magit
  :ensure t
  :commands magit-status magit-blame
  :init (setq
         magit-revert-buffers nil)
  :config
         (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :bind (("C-c g g" . magit-status)
         ("C-c g b" . magit-blame)
         ("C-c g c" . magit-clone)
         ("C-c g f" . magit-file-popup)
         ))

(use-package git-timemachine
  :ensure t
  :bind ("C-c g t" . git-timemachine))


(use-package magit-gh-pulls
  :ensure t
  :after magit
  :hook (magit-mode . turn-on-magit-gh-pulls))

(use-package evil-magit
  :after evil magit
  :init
  (setq evil-magit-state 'motion)
  :pin melpa)

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

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
  :pin melpa
  :ensure t)

(use-package ivy-yasnippet
  :after yasnippet
  :pin melpa)

(use-package aws-snippets
  :after yasnippet
  :pin melpa-milkbox)

(defun yas/org-very-safe-expand ()
          "Safe TAB in 'org-mode' (see 'org-mode' conflicting packages documentation)."
          (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)))


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

;;;;;; MARKDOWN ;;;;;;
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;;;;; merge / diff tooling ;;;;;;
;;(defun my-enable-smerge-maybe ()
;;  (when (and buffer-file-name (vc-backend buffer-file-name))
;;    (save-excursion
;;      (goto-char (point-min))
;;      (when (re-search-forward "^<<<<<<< " nil t)
;;        (smerge-mode +1)))))

;;(add-hook 'buffer-list-update-hook #'my-enable-smerge-maybe)

;;(setq smerge-command-prefix "\C-cv")

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

;;;; HYDRA ;;;;
(use-package hydra
  :ensure t
  :bind (
         ;;("C-c M" . hydra-merge/body)
         ("C-c S" . sbt-hydra)
         ("C-c b" . hydra-btoggle/body)
         ;;("C-c f" . hydra-flycheck/body)
         ;;("C-c m" . hydra-magit/body)
         ;;("C-c o" . hydra-org/body)
         ;;("C-c p" . hydra-projectile/body)
         ;;("C-c w" . hydra-windows/body)
         ;;("C-c w" . hydra-windows/body)
         ;;("C-c B" . hydra-bugger-menu/body)
         ;;("C-c z" . hydra-zoom/body)
         ;;("C-c A" . hydra-org-agenda/body)
         ))

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
    ("F" flyspell-mode "flyspell" :toggle t)
    ("f" flycheck-mode "flycheck" :toggle t)
    ("l" lsp-mode "lsp" :toggle t)
    ("s" smartparens-mode "smartparens" :toggle t))
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
   (("<" flycheck-previous-error "previous" :color pink)
    (">" flycheck-next-error "next" :color pink)
    ("f" flycheck-buffer "check")
    ("l" flycheck-list-errors "list"))
   "Other"
   (("M" flycheck-manual "manual")
    ("v" flycheck-verify-setup "verify setup"))))

(pretty-hydra-define hydra-magit
  (:hint nil :color teal :quit-key "q" :title (with-alltheicon "git" "Magit" 1 -0.05))
  ("Action"
   (("b" magit-blame "blame")
    ("c" magit-clone "clone")
    ("i" magit-init "init")
    ("l" magit-log-buffer-file "commit log (current file)")
    ("L" magit-log-current "commit log (project)")
    ("s" magit-status "status"))))

(pretty-hydra-define hydra-windows
  (:hint nil :forein-keys warn :quit-key "q" :title (with-faicon "windows" "Windows" 1 -0.05))
  ("Window"
   (("b" balance-windows "balance")
    ("i" enlarge-window "heighten")
    ("j" shrink-window-horizontally "narrow")
    ("k" shrink-window "lower")
    ("l" enlarge-window-horizontally "widen")
    ("s" switch-window-then-swap-buffer "swap" :color teal))
   "Zoom"
   (("-" text-scale-decrease "out")
    ("+" text-scale-increase "in")
    ("=" (text-scale-increase 0) "reset"))))

(pretty-hydra-define hydra-projectile
  (:hint nil :color teal :quit-key "q" :title (with-faicon "rocket" "Projectile" 1 -0.05))
  ("Buffers"
   (("b" counsel-projectile-switch-to-buffer "list")
    ("k" projectile-kill-buffers "kill all")
    ("S" projectile-save-project-buffers "save all"))
   "Find"
   (("d" counsel-projectile-find-dir "directory")
    ("D" projectile-dired "root")
    ("f" counsel-projectile-find-file "file")
    ("p" counsel-projectile-switch-project "project"))
   "Other"
   (("i" projectile-invalidate-cache "reset cache"))
   "Search"
   (("r" projectile-replace "replace")
    ("R" projectile-replace-regexp "regexp replace")
    ("s" counsel-rg "search"))))

(pretty-hydra-define hydra-org
  (:hint nil :color teal :quit-key "q" :title (with-fileicon "org" "Org" 1 -0.05))
  ("Action"
   (("A" my/org-archive-done-tasks "archive")
    ("a" org-agenda "agenda")
    ("c" org-capture "capture")
    ("d" org-decrypt-entry "decrypt")
    ("i" org-insert-link-global "insert-link")
    ("j" my/org-jump "jump-task")
    ("k" org-cut-subtree "cut-subtree")
    ("o" org-open-at-point-global "open-link")
    ("r" org-refile "refile")
    ("s" org-store-link "store-link")
    ("t" org-show-todo-tree "todo-tree"))))

(defhydra hydra-zoom (global-map "C-c z")
  "zoom"
  ("k" text-scale-increase "in")
  ("j" text-scale-decrease "out"))

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

(define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)
;;;;;;;;;;;;;;;

;;;; hydra org ;;;;
;; Hydra for org agenda (graciously taken from Spacemacs)
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

;; todo - this has to be lazy loaded after agenda load
(add-hook 'org-agenda-mode-hook (lambda () (define-key org-agenda-mode-map (kbd "s-,") 'hydra-org-agenda/body)))
;;;;;;;;;;;;;;;;;;;

;;;;; Scala ;;;;;
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\|c\\)$"
  :config
  (bind-key "C-S-<tab>" 'dabbrev-expand scala-mode-map)
  (bind-key "s-<delete>" (sp-restrict-c 'sp-kill-sexp) scala-mode-map)
  (bind-key "s-<backspace>" (sp-restrict-c 'sp-backward-kill-sexp) scala-mode-map)
  (bind-key "s-<home>" (sp-restrict-c 'sp-beginning-of-sexp) scala-mode-map)
  (bind-key "s-<end>" (sp-restrict-c 'sp-end-of-sexp) scala-mode-map)
)

(require 'play-routes-mode)

(add-hook 'play-routes-mode-hook
               (lambda ()
                (font-lock-add-keywords nil
                 '(("\\<\\(FIXME\\|TODO\\|fixme\\|todo\\):" 1 font-lock-warning-face t)))))

(add-hook 'scala-mode-hook '(lambda()

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

  (setq
    company-dabbrev-ignore-case nil
    company-dabbrev-code-ignore-case nil
    company-dabbrev-downcase nil
    company-idle-delay 0
    company-minimum-prefix-length 4)
    ;; disables TAB in company-mode, freeing it for yasnippet
  (define-key company-active-map [tab] nil)
  (define-key company-active-map (kbd "TAB") nil)
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
  :pin melpa
  :init
  (setq lsp-prefer-flymake nil)
  (setq lsp-keymap-prefix "C-l")
  :hook ((scala-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (lsp-mode . lsp-lens-mode))
  :config (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred)
  :demand t)

(use-package lsp-ui
  :pin melpa
  :custom
    (lsp-ui-doc-enable t)
    (lsp-ui-flycheck-enable t)
    (lsp-ui-sideline-enable t)
    (lsp-ui-sideline-mode 1)
    (lsp-ui-sideline-ignore-duplicate t)
    (lsp-ui-sideline-show-symbol t)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-sideline-show-diagnostics t)
    (lsp-ui-sideline-show-code-actions t)
    (lsp-ui-sideline-code-actions-prefix "💡")
    (lsp-ui-imenu-enable t)
    (lsp-ui-imenu-kind-position 'top)
  :bind (:map lsp-mode-map ("C-l m" . lsp-ui-imenu))
  :hook (lsp-mode . lsp-ui-mode)
  :commands (lsp-ui-mode)
  :after lsp-mode)

(use-package lsp-ivy
  :pin melpa
  :after lsp-mode
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :pin melpa
  :ensure t
  :after (lsp-mode treemacs)
  :config
  (lsp-metals-treeview-enable t)
  (lsp-treemacs-sync-mode 1)
  (setq lsp-metals-treeview-show-when-views-received t)
  :commands (lsp-treemacs-errors-list lsp-treemacs-references)
)

(use-package dap-mode
  :config
  (dap-mode t)
  (dap-ui-mode t)
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(use-package dap-ui
  :ensure nil
  :after dap-mode
  :config
  (dap-ui-mode 1))

(use-package posframe)

;; java config taken from https://blog.jmibanez.com/2019/03/31/emacs-as-java-ide-revisited.html
(use-package lsp-java
  :pin melpa
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
  :demand t
  :after (lsp-mode dap-mode))

;; Lsp completion
(use-package company-lsp
  :pin melpa
  :after lsp-mode company
  :custom
  (company-lsp-cache-candidates t) ;; auto, t(always using a cache), or nil
  (company-lsp-async t)
  (company-lsp-enable-snippet t)
  (company-lsp-enable-recompletion t)
  :commands company-lsp)
;;;;;;;;;;;;;;;;;;;;

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; docker ;;
(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package docker-tramp)
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))
;;;;;;;;;;;;

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
 '(counsel-projectile-mode t nil (counsel-projectile))
 '(custom-enabled-themes '(gruvbox))
 '(custom-safe-themes
   '("850213aa3159467c21ee95c55baadd95b91721d21b28d63704824a7d465b3ba8" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" default))
 '(diredp-hide-details-initially-flag nil)
 '(global-display-line-numbers-mode t)
 '(guess-language-languages '(en nl) t)
 '(inhibit-startup-screen nil)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate 'full)
 '(js-indent-level 2)
 '(json-reformat:indent-width 2)
 '(lsp-ui-doc-enable t)
 '(lsp-ui-flycheck-enable t t)
 '(lsp-ui-imenu-enable t)
 '(lsp-ui-imenu-kind-position 'top)
 '(lsp-ui-sideline-code-actions-prefix "" t)
 '(lsp-ui-sideline-enable nil)
 '(lsp-ui-sideline-ignore-duplicate t)
 '(lsp-ui-sideline-mode 1 t)
 '(lsp-ui-sideline-show-code-actions t)
 '(lsp-ui-sideline-show-diagnostics nil)
 '(lsp-ui-sideline-show-hover t)
 '(lsp-ui-sideline-show-symbol t)
 '(org-agenda-files
   '("~/Dropbox/org/orgzly.org" "~/Dropbox/org/gcal_sport.org" "~/Dropbox/org/gcal_romex.org" "~/Dropbox/org/gcal.org" "~/Dropbox/org/kredobank.txt" "~/Dropbox/org/learn.org" "~/Dropbox/org/tim.org" "~/Dropbox/org/ucu-scala.org" "~/Dropbox/org/ideas.org" "~/Dropbox/org/band.org" "~/Dropbox/org/work.org" "~/Dropbox/org/reading-list.org" "~/Dropbox/org/psycho.org" "~/Dropbox/org/ptashka.org" "~/Dropbox/org/employment.org" "~/Dropbox/org/sport.org" "~/Dropbox/org/health.org" "~/Dropbox/org/food.org" "~/Dropbox/org/personal.org" "~/Dropbox/org/inbox.org" "~/Dropbox/org/hivecell.org" "~/Dropbox/org/emacs.org" "~/Dropbox/org/car.org" "~/Dropbox/org/blog.org"))
 '(org-agenda-tags-column -120)
 '(org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS")
 '(org-default-priority 67)
 '(org-extend-today-until 2)
 '(org-gcal-down-days 7)
 '(org-gcal-up-days 7)
 '(org-habit-graph-column 70)
 '(org-habit-show-all-today nil)
 '(org-highest-priority 65)
 '(org-journal-date-format "%A, %d %B %Y")
 '(org-journal-dir "~/Dropbox/org/journal/")
 '(org-journal-enable-agenda-integration t)
 '(org-journal-file-type 'weekly)
 '(org-lowest-priority 68)
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-eww org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m))
 '(org-tags-column -100)
 '(package-selected-packages
   '(company-box aws-snippets evil-magit ivy-yasnippet treemacs treemacs-persp posframe lsp-treemacs php-mode ox-reveal org-tree-slide major-mode-hydra dashboard ivy-hydra all-the-icons-ivy counsel diff-hl helpful plantuml-mode yasnippet-snippets magit-gh-pulls github-pullrequest super-save theme-changer dracula-theme nimbus-theme git-gutter-mode emacs-terraform-mode company-terraform docker groovy-mode docker-tramp docker-compose-mode org-jira calfw-gcal calfw-ical calfw-org calfw hydra htmlize dockerfile-mode org-pomodoro dired-ranger ranger dired-atool rainbow-delimiters multiple-cursors avy ace-jump-mode indent-guide mode-icons pyenv-mode elpy markdown-preview-mode yaml-mode exec-path-from-shell avk-emacs-themes atom-one-dark-theme markdown-mode use-package smooth-scroll smartparens popup-imenu play-routes-mode magit highlight-symbol help-mode+ help-fns+ help+ git-timemachine git-gutter flymake-json expand-region evil-leader))
 '(projectile-completion-system 'ivy)
 '(projectile-tags-command "/usr/bin/ctags -Re -f \"%s\" %s")
 '(safe-local-variable-values
   '((flycheck-disabled-checkers emacs-lisp-checkdoc)
     (eval visual-line-mode t)))
 '(tab-always-indent 'complete)
 '(which-key-add-column-padding 3)
 '(which-key-allow-evil-operators t)
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
 '(markdown-code-face ((t (:inherit fixed-pitch :background "gray25"))))
 '(region ((t (:background "gray37")))))
