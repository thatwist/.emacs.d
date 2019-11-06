(setq user-full-name "Yurii Ostapchuk"
      user-mail-address "twist522@gmail.com")

;; global variables
(setq
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.5
 use-package-always-ensure t
 sentence-end-double-space nil)

;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4)

;; modes
;;(electric-indent-mode 0)
;; omg how could I live without this - to remove selection (if active) when inserting text
(delete-selection-mode 1)

;; global keybindings
(global-unset-key (kbd "C-z"))

;; redefine mouse-2
(define-key key-translation-map (kbd "<s-mouse-1>") (kbd "<mouse-2>"))

;; define binding lookup for init.el
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

;; define binding for init.el
(global-set-key (kbd "C-c I") 'find-user-init-file)

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

;; package manager
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-milkbox" . "http://melpa.milkbox.net/packages/")
                    ("marmalade" . "https://marmalade-repo.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
;; a lot of stuff unavailable in stable
 package-archive-priorities '(("melpa-stable" . 1))
)

(setq package-list '(evil
                     evil-leader
                     use-package
                     which-key
                     ))

(package-initialize)

; Update your local package index
(unless package-archive-contents
  (package-refresh-contents))

; Install all missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'use-package)

(menu-bar-mode 1)       ;;; no menu bar
(scroll-bar-mode 1)     ;;; no scrollbar
(tool-bar-mode -1) 

(use-package smooth-scroll
  :config
  (smooth-scroll-mode -1)
  (setq smooth-scroll/vscroll-step-size 4)
  )

;; Scrolling. - laggy
;; (pixel-scroll-mode)
;; (setq pixel-dead-time 0)
;; (setq pixel-resolution-fine-flag t)
;; (setq mouse-wheel-scroll-amount '(1))
;; (setq fast-but-imprecise-scrolling t)
;; (setq jit-lock-defer-time 0)
;; (setq mouse-wheel-progressive-speed nil)

;;; csv-mode ;;;
(use-package csv-mode)
;;;;;;;;;;;;;;;;

;; smex ;;
(use-package smex
  :bind (
         ("M-x" . smex)
         ;; use C-h f, M-., C-h w - while in command mode
         ("M-X" . smex-major-mode-commands)
         ;; old M-x
         ("C-c C-x M-x" . execute-extended-command)
         )
  )
;;;;;;;;;;

;; git-gutter ;;
(use-package git-gutter
  :config
  (global-git-gutter-mode +1)
  ;; check if it actually works
  (git-gutter:linum-setup)
  :bind
  ("C-x C-g" . git-gutter)
  )
;;;;;;;;;;;;;;;;

;;;;; FLYCHECK ;;;;;
(use-package flycheck
:ensure t
:init (global-flycheck-mode))


;;;;;;;;;;;; ENSIME ;;;;;;;;;;;;
(use-package ensime
  :ensure t
  :pin melpa-stable)

(setq
  ensime-sbt-command "/usr/share/sbt/bin/sbt"
  sbt:program-name "/usr/share/sbt/bin/sbt")

(setq ensime-startup-notification nil)

(use-package projectile
  :demand
  :init   (setq projectile-use-git-grep t)
  :config (projectile-global-mode t)
  :bind   (("s-f" . projectile-find-file)
           ("s-F" . projectile-grep)
           ("s-D" . projectile-find-dir)
           ("s-p" . projectile-command-map)
           ("C-c p" . projectile-command-map)
           ("s-P" . projectile-switch-project)))


;; treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'epa-file)
(epa-file-enable)

(desktop-save-mode 1)

(which-key-mode)

(if (window-system)
        (progn
          (global-hl-line-mode 1) ;;; highlight current line
          (set-face-background hl-line-face "gray87")
          ))

;; icons in menu
(use-package mode-icons
  :config (mode-icons-mode -1))

;; show indents in all modes
(use-package indent-guide
  :config (indent-guide-global-mode 1))

;;;;;;;;;;; IVY ;;;;;;;;;;;;
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h l") 'counsel-find-library)
(global-set-key (kbd "C-c i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "C-c u") 'counsel-unicode-char)
(global-set-key (kbd "C-c s v") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)
;; Ivy-based interface to shell and system tools
(global-set-key (kbd "C-c s c") 'counsel-compile)
(global-set-key (kbd "C-c s g") 'counsel-git)
(global-set-key (kbd "C-c s j") 'counsel-git-grep)
(global-set-key (kbd "C-c s L") 'counsel-git-log)
(global-set-key (kbd "C-c s k") 'counsel-rg)
(global-set-key (kbd "C-c s m") 'counsel-linux-app)
(global-set-key (kbd "C-c s n") 'counsel-fzf)
(global-set-key (kbd "C-c s l") 'counsel-locate)
(global-set-key (kbd "C-c s J") 'counsel-file-jump)
;; Ivy-resume and other commands
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-c s b") 'counsel-bookmark)
(global-set-key (kbd "C-c s D") 'counsel-descbinds)
(global-set-key (kbd "C-c s o") 'counsel-outline)
(global-set-key (kbd "C-c s t") 'counsel-load-theme)
(global-set-key (kbd "C-c s f") 'counsel-org-file)
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
;;;;;;;;;;;;;;;;;;;;


(setq browse-url-browser-function 'browse-url-default-browser)


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
;;;;

(use-package avy
  :ensure t
  :bind (("C-'" . avy-goto-char-2)
         ("C-\"" . avy-goto-char-timer)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
)
; todo - avy-org-goto-heading-timer - bind for org-mode only

;; multi-cursors
(use-package evil-mc
  :ensure t)
(global-evil-mc-mode t)
;;   26 Jun 19 - doesn't work, trying evil-mc
;; (use-package multiple-cursors
;;   :bind (("C-S-c C-S-c" . mc/edit-lines)
;;          ("C->" . mc/mark-next-like-this)
;;          ("C-<" . mc/mark-previous-like-this)
;;          ("C-c C-<" . mc/mark-all-like-this)))

(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;;;;;;;;;;;; EVIL MODE ;;;;;;;;;;;;;;
(require 'evil)
(evil-mode t)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "b" 'switch-to-buffer
  "w" 'save-buffer
  "f" 'find-file
  "v" 'er/expand-region)

(add-to-list 'load-path "~/.emacs.d/evil-org-mode")
(use-package evil-org
  :ensure t
  :after (evil org)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode
	    (lambda() (
		       evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-define-key 'motion org-agenda-mode-map
    "ZK" 'org-habit-toggle-habits
  )
)

;; disable evil in help mode (emacs by default)
(evil-set-initial-state 'Info-mode 'emacs)
(evil-set-initial-state 'special-mode 'emacs)
(evil-set-initial-state 'messages-major-mode 'emacs)

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

(setq auto-save-file-name-transforms
          `((".*" ,(concat user-emacs-directory "auto-save/") t)))

(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1)
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  ;; save on find-file
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
)

(require 'play-routes-mode)

(add-hook 'play-routes-mode-hook
               (lambda ()
                (font-lock-add-keywords nil
                 '(("\\<\\(FIXME\\|TODO\\|fixme\\|todo\\):" 1 font-lock-warning-face t)))))

(require 'whitespace)
(setq whitespace-line-column 120) ;; limit line length
(setq whitespace-style '(face ;; lines-tail
                              ))

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
  ;;(git-gutter-mode)
  (company-mode)
  (ensime-mode)
  (scala-mode:goto-start-of-code))
)

;;;;;;; THEMES ;;;;;;;;
;; (load-theme 'dracula t)
;; (load-theme 'atom-one-dark t)
;; (load-theme 'avk-dark-blue-yellow t)
;; (load-theme 'nimbus-theme t)
;; (load-theme 'dracula-theme t)
;; (load-theme 'solarized-theme t)
;; (load-theme 'zenburn t)
;; (load-theme 'gruvbox t)
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

(use-package flx-ido
  :demand
  :init
  (setq
   ido-enable-flex-matching t
   ;; C-d to open directories
   ;; C-f to revert to find-file
   ido-show-dot-for-dired nil
   ido-enable-dot-prefix t)
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1))

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  :bind ("s-h" . highlight-symbol))

(use-package goto-chg
  :commands goto-last-change
  ;; complementary to
  ;; C-x r m / C-x r l
  ;; and C-<space> C-<space> / C-u C-<space>
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

(use-package popup-imenu
  :commands popup-imenu
  :bind ("M-i" . popup-imenu))

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
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cg" 'org-gcal-fetch)
(define-key global-map "\C-cG" 'org-gcal-sync)
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
         "* PROJECT *%^{Project title}* :%^G:project:\n:PROPERTIES:\n:CREATED: %U\n:END:\n  %^{Project description}\n  *goals*\n  %^{Project goals}\n** TODO %?\n** TODO review\nSCHEDULED: <%<%Y-%m-%d %a .+14d>>\n** _IDEAS_\n" :clock-in t :clock-resume t)
        ("h" "Habit" entry (file+headline "~/Dropbox/org/personal.org" "*habits*")
         "* NEXT %?\nSCHEDULED: <%<%Y-%m-%d %a .+1d>>\n:PROPERTIES:\n:CREATED: %U\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:LOGGING: DONE(!)\n:ARCHIVE: %%_archive::* Habits\n:END:\n%U\n")
        ("B" "Budget entry" entry (file+olp "~/Dropbox/org/personal.org" "*finance*" "*budgeting*" "finance Oct 2019")
         "* %^{Entry description}\n  :PROPERTIES:\n  :AMOUNT:   %^{Amount}\n  :CURRENCY: UAH\n  :DATETIME:  %U\n  :CATEGORY:  %^{Category}\n  :TYPE:     CASH\n  :END:\n")
        ("a" "Appointment" entry (file  "~/Dropbox/org/gcal.org" ) "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
        ("e" "Word [english]" entry (file "~/Dropbox/org/english.org" ) "* %i%?")
        ("o" "Org idea" entry (file+olp "~/Dropbox/org/org.org" "ideas" "org ideas") "*** TODO %i%?")
        ("b" "Buylist" entry (file+olp "~/Dropbox/org/personal.org" "*buylist*") "** TODO %i%?")
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

;; fontify code in code blocks
(setq org-src-fontify-natively t)

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
(setq org-plantuml-jar-path
      (expand-file-name "~/plantuml.jar"))
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

;; fontify code in code blocks
(setq org-src-fontify-natively t)

;;;; persist history of clock-in clock-out between emacs shutdowns
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(use-package org-mru-clock
  :ensure t
  :bind* (("C-c C-x i" . org-mru-clock-in)
          ("C-c C-x j" . org-mru-clock-select-recent-task))
  :init
  (setq org-mru-clock-how-many 20
        org-mru-clock-completing-read #'ivy-completing-read)
)


;;; org-journal ;;;
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

;;;;; org-pomodoro ;;;;;;
(use-package org-pomodoro
  :ensure t
  :commands (org-pomodoro)
  :bind (("C-x g" . magit-status))
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
  :commands magit-status magit-blame
  :init (setq
         magit-revert-buffers nil)
  :bind (("C-x g" . magit-status)
         ("s-b" . magit-blame)))

(use-package git-timemachine)

(add-hook 'git-timemachine-mode-hook (lambda () (ensime-mode 0)))

(use-package magit-gh-pulls
  :ensure t
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
)

(use-package yasnippet
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :config (yas-reload-all))


;;(setq yas-snippet-dirs
;;      (expand-file-name "snippets" user-emacs-directory))
;;(append yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory)))
;;                  ;; personal snippets
;;(setq yas-snippet-dirs
;;      (append yas-snippet-dirs (expand-file-name "yasnippet/yasnippet-snippets/snippet" user-emacs-directory)))
;;                  ;; the default collection
;;        ))

;;(use-package yasnippet-snippets)

(setq yas-snippet-dirs '(
                         "~/.emacs.d/snippets"
                         ;;"~/.emacs.d/yasnippet/yasnippet-snippets/snippets"
                         ))

(yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.

;; safe TAB in org-mode (see org-mode conflicting packages documentation)
(defun yas/org-very-safe-expand ()
          (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)))


;;;;;;;; SMARTPARENS ;;;;;;;;
;; if M-<backspace> annoys - see this - https://github.com/Fuco1/smartparens/pull/861/files
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


(use-package etags-select
  :commands etags-select-find-tag)

(require 'scala-mode)

(global-set-key (kbd "C-c C-d d") 'ensime-db-attach)

;;;;; added scala speedbar support ;;;;;
;;(speedbar-add-supported-extension ".scala")
;(setq speedbar-use-imenu-flag nil)
;;(setq speedbar-fetch-etags-command "ctags")

;;(setq speedbar-fetch-etags-arguments '("-e" "-f -"))
;;(add-to-list 'speedbar-fetch-etags-parse-list
;;            '("\\.scala" . speedbar-parse-c-or-c++tag))

(defun ensime-edit-definition-with-fallback ()
  "Variant of `ensime-edit-definition' with ctags if ENSIME is not available."
  (interactive)
  (unless (and (ensime-connection-or-nil)
               (ensime-edit-definition))
    (projectile-find-tag)))

(use-package expand-region
  :commands 'er/expand-region
  :bind ("C-=" . er/expand-region))

(require 'ensime-expand-region)

(bind-key "M-." 'ensime-edit-definition-with-fallback ensime-mode-map)
(bind-key "C-S-<tab>" 'dabbrev-expand scala-mode-map)

(sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
(sp-local-pair 'scala-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))

(defun sp-restrict-c (sym)
  "Smartparens restriction on `SYM' for C-derived parenthesis."
  (sp-restrict-to-pairs-interactive "{([" sym))

(bind-key "s-<delete>" (sp-restrict-c 'sp-kill-sexp) scala-mode-map)
(bind-key "s-<backspace>" (sp-restrict-c 'sp-backward-kill-sexp) scala-mode-map)
(bind-key "s-<home>" (sp-restrict-c 'sp-beginning-of-sexp) scala-mode-map)
(bind-key "s-<end>" (sp-restrict-c 'sp-end-of-sexp) scala-mode-map)

(bind-key "s-{" 'sp-rewrap-sexp smartparens-mode-map)


;;;;;;; CUSTOM DEFINITIONS ;;;;;;;
(defun close-and-kill-next-pane ()
  "If there are multiple windows, then close the other pane and kill the buffer in it also."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(defun close-and-kill-current-pane ()
  "Kill current buffer and close the pane, works differently to kill-buffer-and-window as it checks whether there are other windows at all"
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
(global-set-key (kbd "M-.") 'projectile-find-tag)
(global-set-key (kbd "M-,") 'pop-tag-mark)
(global-set-key (kbd "C-;") 'comment-region)
(global-set-key (kbd "C-:") 'uncomment-region)
(global-set-key (kbd "C-x 4 1") 'close-and-kill-next-pane)
(global-set-key (kbd "s-!") 'close-and-kill-next-pane)
;; doesn't work
;;(global-set-key (kbd "s-SPC") 'toggle-input-method)
(global-set-key (kbd "C-c w") 'toggle-truncate-lines); wrap

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


;;;; jsonlint
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(require 'flymake-json)
(global-set-key (kbd "C-c j v") 'flymake-json-load)
(add-hook 'json-mode 'flymake-json-load)
(add-hook 'js-mode-hook 'flymake-json-maybe-load)
(add-hook 'find-file-hook 'flymake-json-maybe-load)

;;;;;; PYTHON ;;;;;;;;
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)
(elpy-enable)
(defun elpy-goto-definition-or-rgrep ()
  "Go to the definition of the symbol at point, if found. Otherwise, run `elpy-rgrep-symbol'."
    (interactive)
    (ring-insert find-tag-marker-ring (point-marker))
    (condition-case nil (elpy-goto-definition)
        (error (elpy-rgrep-symbol
                   (concat "\\(def\\|class\\)\s" (thing-at-point 'symbol) "(")))))
(define-key elpy-mode-map (kbd "s-.") 'elpy-goto-definition-or-rgrep)

(use-package pyvenv
    :ensure t
    :init
    (setenv "WORKON_HOME" "~/.pyenv/versions") ; can be /var/local/plone/buildouts/
    (pyvenv-mode 1)
    (pyvenv-tracking-mode 1)
    )

;; TODO - distinguish pyvenv and pyenv and make this buffer aware
;;(pyenv-mode)
;;;;;;;;;;;;;;;;;;;;;;

;;; RESIZE BUFFERS ;;;
(global-set-key (kbd "M-S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-S-C-<down>") 'shrink-window)
(global-set-key (kbd "M-S-C-<up>") 'enlarge-window)

;;;;;;;;;;;;;;;;;;;;;;
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
(use-package hydra :ensure t)

(defhydra hydra-zoom (global-map "s-+")
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


;;;;; LSP JAVA ;;;;;
;; taken from https://blog.jmibanez.com/2019/03/31/emacs-as-java-ide-revisited.html
(use-package lsp-mode
  :init
  (setq lsp-prefer-flymake nil)
  :demand t
  :after my-init-platform-paths)

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t)
  (define-key lsp-ui-mode-map
    [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map
    [remap xref-find-references] #'lsp-ui-peek-find-references)

  :after lsp-mode)

(use-package dap-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(setq my/lombok-jar (expand-file-name "~/lombok/lombok.jar"))
;;(setq my/java-format-settings-file (expand-file-name "~/projects/defaultFormatterProfile.xml"))

(use-package lsp-java
  :init
  (defun my/java-mode-config ()
    (toggle-truncate-lines 1)
    (setq-local tab-width 4)
    (setq-local c-basic-offset 4)
    (lsp))

  :config
  ;; Enable dap-java
  (require 'dap-java)

  (setq lsp-java-vmargs
        (list "-noverify"
              "-Xmx2G"
              "-XX:+UseG1GC"
              "-XX:+UseStringDeduplication"
              (concat "-javaagent:" my/lombok-jar)
              (concat "-Xbootclasspath/a:" my/lombok-jar))
        lsp-file-watch-ignored
        '(".idea" ".ensime_cache" ".eunit" "node_modules"
          ".git" ".hg" ".fslckout" "_FOSSIL_"
          ".bzr" "_darcs" ".tox" ".svn" ".stack-work"
          "build")

        lsp-java-import-order '["" "java" "javax" "#"]
        ;; Don't organize imports on save
        lsp-java-save-action-organize-imports nil

        ;; Formatter profile
        lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
        )
        ;;(concat "file://" my/java-format-settings-file))

  :hook (java-mode . my/java-mode-config)

  :demand t
  :after (lsp lsp-mode dap-mode))

;; not sure if it doesn't duplicate above
(add-hook 'java-mode-hook #'lsp)

;; these two from java-lsp snippet
(use-package company-lsp :ensure t)
(push 'company-lsp company-backends)
(require 'dap-java)
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


;;;; jira ;;;;
(use-package org-jira
  :config
  (setq jiralib-url "https://jira.com")
  (setq jiralib-user-login-name "yurii.ostapchuk")
)
;; confluence support
(require 'ox-confluence)
;;;;;;;;;;;;;;

;; docker ;;
(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package docker-tramp)
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))
;;;;;;;;;;;;

;; groovy ;;
(use-package groovy-mode)
;;;;;;;;;;;;

;; terraform ;;
(use-package terraform-mode)
(use-package company-terraform
  :config
  (company-terraform-init)
)
;;;;;;;;;;;;;;;
;;(custom-set-faces
 ;;'(region ((t (:background "LightSalmon1" :distant-foreground "gtk_selection_fg_color")))))

;; custom ;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (gruvbox)))
 '(custom-safe-themes
   (quote
    ("1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" default)))
 '(diredp-hide-details-initially-flag nil)
 '(global-display-line-numbers-mode t)
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/gcal_sport.org" "~/Dropbox/org/gcal_romex.org" "~/Dropbox/org/gcal.org" "~/Dropbox/org/kredobank.txt" "~/Dropbox/org/learn.org" "~/Dropbox/org/tim.org" "~/Dropbox/org/ucu-scala.org" "~/Dropbox/org/ideas.org" "~/Dropbox/org/band.org" "~/Dropbox/org/work.org" "~/Dropbox/org/reading-list.org" "~/Dropbox/org/psycho.org" "~/Dropbox/org/ptashka.org" "~/Dropbox/org/employment.org" "~/Dropbox/org/sport.org" "~/Dropbox/org/health.org" "~/Dropbox/org/food.org" "~/Dropbox/org/personal.org" "~/Dropbox/org/inbox.org" "~/Dropbox/org/hivecell.org" "~/Dropbox/org/emacs.org" "~/Dropbox/org/car.org" "~/Dropbox/org/blog.org")))
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
 '(org-journal-file-type (quote weekly) t)
 '(org-lowest-priority 68)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-eww org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(org-tags-column -100)
 '(package-selected-packages
   (quote
    (counsel helpful org-journal plantuml-mode yasnippet-snippets magit-gh-pulls github-pullrequest super-save org-mru-clock theme-changer dracula-theme nimbus-theme git-gutter-mode smex emacs-terraform-mode company-terraform docker groovy-mode docker-tramp docker-compose-mode org-jira calfw-gcal calfw-ical calfw-org calfw treemacs dap-mode hydra evil-surround evil-mc htmlize evil-org dockerfile-mode org-pomodoro org-plus-contrib dired-ranger ranger dired-atool rainbow-delimiters multiple-cursors avy ace-jump-mode indent-guide mode-icons which-key pyenv-mode elpy csv-mode markdown-preview-mode yaml-mode exec-path-from-shell avk-emacs-themes atom-one-dark-theme markdown-mode use-package smooth-scroll smartparens projectile popup-imenu play-routes-mode magit highlight-symbol help-mode+ help-fns+ help+ git-timemachine git-gutter flymake-json expand-region evil-leader etags-select ensime)))
 '(projectile-tags-command "/usr/local/bin/ctags -Re -f \"%s\" %s")
 '(safe-local-variable-values
   (quote
    ((flycheck-disabled-checkers emacs-lisp-checkdoc)
     (eval visual-line-mode t))))
 '(which-key-add-column-padding 3)
 '(which-key-allow-evil-operators t)
 '(which-key-max-description-length 50)
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
