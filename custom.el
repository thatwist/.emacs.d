;;; custom.el --- custom settings     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Yurii Ostapchuk

;; Author: Yurii Ostapchuk <twist522@gmail.com>
;; Keywords: local

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
   '("2b9dc43b786e36f68a9fd4b36dd050509a0e32fe3b0a803310661edb7402b8b6" "b583823b9ee1573074e7cbfd63623fe844030d911e9279a7c8a5d16de7df0ed0" "8e797edd9fa9afec181efbfeeebf96aeafbd11b69c4c85fa229bb5b9f7f7e66c" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "850213aa3159467c21ee95c55baadd95b91721d21b28d63704824a7d465b3ba8" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" default))
 '(dashboard-banner-logo-title "With Great Power Comes Great Responsibility")
 '(dashboard-center-content t)
 '(dashboard-set-file-icons t)
 '(dashboard-set-heading-icons t)
 '(dashboard-set-init-info t)
 '(dashboard-set-navigator t)
 '(dashboard-startup-banner 'official)
 '(diredp-hide-details-initially-flag nil)
 '(eshell-history-size 12800)
 '(evil-collection-setup-minibuffer t)
 '(evil-collection-want-unimpaired-p nil)
 '(flycheck-global-modes '(not org-mode))
 '(google-translate-show-phonetic t)
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
   '(("this month budget" "ledger budget expenses -p \"this month\" -X UAH")
     ("last-month-balance" "ledger [[ledger-mode-flags]] -f ~/Dropbox/org/ledger/ledger.dat --monthly bal ^expenses -X UAH -p \"last month\"")
     ("last-month-expenses" "ledger [[ledger-mode-flags]] -f ~/Dropbox/org/ledger/ledger.dat reg ^expenses -X UAH -p \"last month\" --monthly")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(lsp-flycheck-live-reporting t t)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-doc-include-signature t t)
 '(lsp-ui-doc-position 'top t)
 '(lsp-ui-doc-use-childframe t t)
 '(lsp-ui-flycheck-enable t t)
 '(lsp-ui-flycheck-list-position 'right t)
 '(lsp-ui-flycheck-live-reporting t t)
 '(lsp-ui-imenu-enable t)
 '(lsp-ui-imenu-kind-position 'top)
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
   '("~/Dropbox/org/content.org" "~/Dropbox/org/goals.org" "~/Dropbox/org/consume.org" "~/Dropbox/org/talks.org" "~/Dropbox/org/orgzly.org" "~/Dropbox/org/gcal/family.org" "~/Dropbox/org/gcal/sport.org" "~/Dropbox/org/gcal/personal.org" "~/Dropbox/org/tim.org" "~/Dropbox/org/ideas.org" "~/Dropbox/org/music.org" "~/Dropbox/org/work.org" "~/Dropbox/org/ptashka.org" "~/Dropbox/org/employment.org" "~/Dropbox/org/sport.org" "~/Dropbox/org/health.org" "~/Dropbox/org/food.org" "~/Dropbox/org/personal.org" "~/Dropbox/org/inbox.org" "~/Dropbox/org/emacs.org" "~/Dropbox/org/car.org"))
 '(org-agenda-tags-column -140)
 '(org-extend-today-until 2)
 '(org-gcal-down-days 7)
 '(org-gcal-up-days 7)
 '(org-habit-graph-column 75)
 '(org-habit-show-all-today nil)
 '(org-journal-date-format "%A, %d %B %Y" t)
 '(org-journal-dir "~/Dropbox/org/journal/" t)
 '(org-journal-enable-agenda-integration t t)
 '(org-journal-file-type 'weekly t)
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m org-expiry org-notify))
 '(org-pomodoro-audio-player "mplayer" t)
 '(org-pomodoro-finished-sound "/usr/share/sounds/freedesktop/stereo/complete.oga" t)
 '(org-pomodoro-finished-sound-args "-af volume=5" t)
 '(org-pomodoro-format "%s" t)
 '(org-pomodoro-long-break-format "~~%s~~" t)
 '(org-pomodoro-long-break-sound "/usr/share/sounds/freedesktop/stereo/window-attention.oga" t)
 '(org-pomodoro-long-break-sound-args "-af volume=5" t)
 '(org-pomodoro-short-break-format "%s" t)
 '(org-pomodoro-short-break-sound "/usr/share/sounds/freedesktop/stereo/window-attention.oga" t)
 '(org-pomodoro-short-break-sound-args "-af volume=5" t)
 '(org-pomodoro-start-sound "/usr/share/sounds/freedesktop/stereo/complete.oga" t)
 '(org-pomodoro-start-sound-args "-af volume=5" t)
 '(org-priority-default 67)
 '(org-priority-highest 65)
 '(org-priority-lowest 68)
 '(org-reveal-highlight-css "%r/plugin/highlight/zenburn.css" t)
 '(org-reveal-reveal-js-version 4 t)
 '(org-reveal-root "~/reveal.js" t)
 '(org-roam-directory "~/Dropbox/org/" t)
 '(org-tags-column -100)
 '(package-selected-packages
   '(eshell-syntax-highlighting xah-find good-scroll good-scroll-mode peep-dired dashboard-elfeed hydra-posframe doom-themes neotree telega elfeed-web auto-indent-mode evil-smartparens mu4e go-translate general elfeed-score elfeed-dashboard elfeed-goodies google-translate frame-cmds frame-fns zoom-frm help-fns+ rainbow-mode sqlformat pass password-store org-agenda docopt ob-async ein-jupyterhub auto-complete parseedn sql-presto ukrainian-holidays blacken py-autopep8 matrix-client quelpa-use-package ivy-posframe ein org-roam org-bullets org-sidebar evil-nerd-commenter exwm esup i3wm-config-mode gnuplot org-pretty-tags olivetti olivetti-mode mixed-pitch dashboard-mode company-quickhelp ox-hugo ox-huge term-cursor quelpa shell-switcher systemd systemd-mode eshell-git-prompt dired lsp-metals edit-server xclip sudo-edit pinentry gist org-gcal org-timeline org-plus-contrib company-lsp flycheck-ledger evil-ledger org-alert w3m origami hl-todo yasnippet-snippets which-key wgrep-ag wgrep shrink-path scala-mode sbt-mode paredit org-mru-clock org-journal memoize makey ivy-rich flx evil-surround evil-mc evil-magit evil-leader evil-collection evil-cleverparens emms elfeed-org elfeed doom-modeline discover-my-major dired-subtree dired-rainbow dired-open dired-narrow dired-hacks-utils dired-filter dired-collapse dired-avfs deferred csv-mode counsel-projectile bui annalist all-the-icons-ivy all-the-icons ag ejc-sql bug-hunter ripgrep bash-mode typescript-mode projectile evil-org gruvbox-theme 2048-game aws-snippets posframe php-mode ox-reveal org-tree-slide major-mode-hydra dashboard ivy-hydra counsel diff-hl helpful plantuml-mode magit-gh-pulls github-pullrequest super-save theme-changer dracula-theme nimbus-theme git-gutter-mode emacs-terraform-mode company-terraform docker groovy-mode docker-tramp docker-compose-mode org-jira calfw-gcal calfw-ical calfw-org calfw hydra htmlize dockerfile-mode org-pomodoro dired-ranger ranger dired-atool rainbow-delimiters multiple-cursors avy ace-jump-mode indent-guide mode-icons pyenv-mode elpy markdown-preview-mode yaml-mode exec-path-from-shell avk-emacs-themes atom-one-dark-theme use-package smooth-scroll smartparens popup-imenu play-routes-mode magit highlight-symbol git-timemachine git-gutter expand-region))
 '(plantuml-default-exec-mode 'jar t)
 '(plantuml-jar-path "~/plantuml/plantuml.jar" t)
 '(projectile-completion-system 'ivy t)
 '(projectile-project-search-path '("~/Documents") t)
 '(request-log-level 'debug t)
 '(safe-local-variable-values
   '((eval setq org-cycle-include-plain-lists 'integrate)
     (sbt-hydra:projects \"memengine\")
     (doom-modeline-buffer-file-name-style . truncate-with-project)
     (doom-modeline-buffer-file-name-style quote truncate-with-project)
     (org-hugo-footer . "

[//]: # \"Exported with love from a post written in Org mode\"
[//]: # \"- https://github.com/kaushalmodi/ox-hugo\"")
     (checkdoc-minor-mode . t)
     (flycheck-disabled-checkers emacs-lisp-checkdoc)
     (eval visual-line-mode t)))
 '(tab-always-indent 'complete)
 '(tramp-syntax 'simplified nil (tramp))
 '(treemacs-fringe-indicator-mode t)
 '(which-key-add-column-padding 3)
 '(which-key-allow-evil-operators t)
 '(which-key-idle-delay 0.5)
 '(which-key-max-description-length 50)
 '(which-key-mode t)
 '(whitespace-line-column 170)
 '(whitespace-style
   '(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:background "#333355" :foreground "blue3" :width extra-expanded))))
 '(diff-hl-delete ((t (:inherit diff-removed :foreground "red3" :width extra-expanded))))
 '(diff-hl-insert ((t (:inherit diff-added))))
 '(fringe ((t (:background "#282828" :weight extra-bold :height 3.0 :width extra-expanded))))
 '(markdown-code-face ((t (:inherit fixed-pitch :background "gray25"))))
 '(region ((t (:extend t :background "dark slate blue")))))
