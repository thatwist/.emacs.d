;;https://github.com/redguardtoo/emacs.d/blob/master/init.el

(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)

;;(setq user-init-file (or load-file-name (buffer-file-name)))
;; (setq user-emacs-directory "/home/twist/.config/emacs/")
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
;; for this to work it has to be in init.el
(setq inhibit-startup-echo-area-message (user-login-name))
;;(fset 'display-startup-echo-area-message 'ignore)

;; doesn't work
;;(setq initial-buffer-choice "*Messages*")

(setq initial-scratch-message nil)

(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (horizontal-scroll-bars)
        (vertical-scroll-bars)))

(setq make-backup-files nil)


;; speed up slow file loading caused by regex matching caused by default value of this variable
(let* ((file-name-handler-alist nil))
  (require ' package)
  (add-to-list 'package-archives '("melpa". "https://melpa.org/packages/"))
  ;;(setq package-quickstart t) ;; I don't really understand how this works
  ;; Minimal initialization
  (setq package-enable-at-startup nil)
  ;; I could do this and than package-initialize but this is not convenient
  ;;(setq package-load-list '((gruvbox-theme t) (autothemer t) (dash t)))
  (package-initialize 'no-activate)

  ;; (unless (package-installed-p 'use-package)
  ;;   (package-refresh-contents)
  ;;   (package-install 'use-package))
  ;; (require 'use-package)
  ;; (setq use-package-always-defer t)
  ;; (use-package doom-themes :ensure)

  ;; (add-to-list 'load-path (concat user-emacs-directory "elpa/doom-themes-2.3.0"))
  ;; (load "doom-themes-autoloads")
  ;; (load-theme 'doom-one t)
  ;; (doom-themes-org-config)

  ;; (add-to-list 'load-path (concat user-emacs-directory "elpa/dash-2.19.1"))
  ;; (add-to-list 'load-path (concat user-emacs-directory "elpa/autothemer-0.2.18"))
  ;; (add-to-list 'load-path (concat user-emacs-directory "elpa/gruvbox-theme-20240615.432"
  ;;                                 ;; "elpa/gruvbox-theme-1.30.1"
  ;;                                 ))
  (package-activate 'gruvbox-theme)
  ;;(require 'gruvbox-theme)
  (load-theme 'gruvbox-dark-hard t)


  (set-face-attribute 'default nil :font "Fira Code" :height 122)


  (require 'tab-line)
  (set-face-attribute 'tab-line nil :foreground "#fdf4c1" :background "#1d2021"
                      :distant-foreground "#fdf4c1" :distant-foreground 'unspecified
                      :family "Fira Sans Condensed 18" :height 1.0 :box nil) ;; background behind tabs
  (set-face-attribute 'tab-line-tab nil :inherit 'tab-line :box nil :background "#1d2021") ;; active tab in another window
  (set-face-attribute 'tab-line-tab-current nil :inverse-video t :box nil :background "#1d2021") ;; active tab in current window
  (set-face-attribute 'tab-line-tab-inactive nil :background "#3c3836") ;; inactive tab
  (set-face-attribute 'tab-line-highlight nil :box nil :background "#4c4846" :foreground 'unspecified) ;; mouseover
  (setq tab-line-new-button-show nil)
  (setq tab-line-close-button-show nil)
  (global-tab-line-mode)



  ;; (require 'tab-bar)
  ;; (setq tab-bar-tab-hints t)
  ;; (setq tab-bar-close-button-show nil)
  ;; (setq tab-bar-close-button-show nil)
  ;; (setq tab-bar-select-tab-modifiers '(meta))
  ;; (tab-bar-mode)


  (set-face-attribute 'default nil :font "Fira Code" :height 122)

  (package-activate 'telephone-line)
  ;;(add-to-list 'load-path (concat user-emacs-directory "elpa/telephone-line-20240109.2021"))
  (require 'telephone-line)
  (require 'telephone-line-segments)
  (defun telephone-line-modal-face (active)
    (cond ((not active) 'mode-line-inactive)
          ((and meow-normal-mode (region-active-p)) 'telephone-line-evil-visual)
          (meow-normal-mode 'telephone-line-evil-normal)
          (meow-insert-mode 'telephone-line-evil-insert)
          (meow-motion-mode 'telephone-line-evil-emacs)
          (meow-keypad-mode 'telephone-line-evil-operator)
          (meow-beacon-mode 'telephone-line-evil-replace)))
  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (modal  . (telephone-line-meow-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-project-buffer-segment))))
  (telephone-line-mode 1)




  ;; (add-to-list 'load-path (concat user-emacs-directory "elpa/vertico-1.8"))
  ;; Enable vertico
  (package-activate 'vertico)
  ;; (require 'vertico)
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 16)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)

  ;; Persist history over Emacs restarts. Vertico sorts by history position.
  (require 'savehist)
  (savehist-mode)

  ;; A few more useful configurations...
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p)



  (package-activate 'consult)



  ;; (add-to-list 'load-path (concat user-emacs-directory "elpa/orderless-1.1"))
  ;; Optionally use the `orderless' completion style.
  ;; (require 'orderless)
  (package-activate 'orderless)
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(orderless-flex orderless-initialism)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq orderless-matching-styles '(orderless-flex orderless-prefixes))
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))




  ;; (add-to-list 'load-path (concat user-emacs-directory "elpa/marginalia-1.6"))
  ;; Enable rich annotations using the Marginalia package
  (package-activate 'marginalia)
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  ;; :bind (:map minibuffer-local-map
  ;;             ("M-A" . marginalia-cycle))
  (marginalia-mode)



  (package-activate 'embark)
  (define-key global-map (kbd "C-.") 'embark-act)
  (define-key global-map (kbd "C-;") 'embark-dwim)
  (define-key global-map (kbd "C-h B") 'embark-bindings)
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;; Consult users will also want the embark-consult package.
  (package-activate 'embark-consult)
  (add-hook 'embark-collect-mode #'consult-preview-at-point-mode)


  ;; (add-to-list 'load-path (concat user-emacs-directory "elpa/evil-1.15.0"))
  ;; (package-activate 'evil)
  ;; (require 'evil)
  ;; (evil-mode)

  ;; (add-to-list 'load-path (concat user-emacs-directory "elpa/meow-1.4.5"))
  (package-activate 'meow)

  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (defun find-config-org () (interactive)
           (find-file (file-truename (concat user-emacs-directory "config.org"))))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     '("w |" . split-window-right)
     '("w _" . split-window-below)
     '("d" . dired-jump)
     '("f" . find-file)
     '("u i" . find-config-org)
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (defun quit-window-kill-buffer ()
      (interactive)
      (quit-window t))
    (dolist (mode '(insert normal motion))
      (meow-define-keys mode
                        '("M-o" . other-window)
                        '("M-s" . save-buffer)
                        '("M-d" . delete-window)
                        '("M-j" . quit-window)
                        '("M-k" . quit-window-kill-buffer)
                        '("M-h" . tab-line-switch-to-prev-tab)
                        '("M-l" . tab-line-switch-to-next-tab)
                        ;; '("s-j" . scroll-up-command)
                        ;; '("s-k" . scroll-down-command)
                        )
      )
    (defun meow-visual-rectangle-mode ()
      "Toggle rectangle-mark-mode and enter visual state."
      (interactive)
      (if rectangle-mark-mode
          (rectangle-mark-mode -1)
        (rectangle-mark-mode 1))
      (meow-visual-mode 1))

    (defun meow-rectangle-toggle ()
      "Toggle rectangle-mark-mode and switch to motion state."
      (interactive)
      (if rectangle-mark-mode
          (rectangle-mark-mode -1)
        (rectangle-mark-mode 1))
      (meow-motion-mode 1))

    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("V" . meow-visual-rectangle-mode)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  (require 'meow)
  (meow-setup)
  (meow-global-mode 1)


  ;; (package-activate 'boon)
  ;; (require 'boon-qwerty)
  ;; (require 'boon-hl)
  ;; (boon-mode)
  


  (add-hook 'prog-mode-hook #'subword-mode)



  (require 'dired)

  (setq dired-listing-switches "-AGhlv --group-directories-first --time-style=long-iso")
  (setq delete-by-moving-to-trash t)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (define-key dired-mode-map "l" 'dired-find-file)
  (define-key dired-mode-map "h" 'dired-up-directory)



  )

(defun my-cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  67108864) ; 64M
  (setq gc-cons-percentage 0.1) ; original value
  (garbage-collect))

(run-with-idle-timer 4 nil #'my-cleanup-gc)

(message "*** Emacs loaded in %s with %d garbage collections."
         (format "%.2f seconds"
                 (float-time (time-subtract after-init-time before-init-time)))
         gcs-done)
