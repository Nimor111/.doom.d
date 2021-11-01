;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Georgi Bojinov"
      user-mail-address "georgi.bojinov@hotmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "mononoki Nerd Font Mono" :size 20 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "mononoki Nerd Font Mono" :size 18))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Nextcloud/org")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! org-brain
  :init
  (setq org-brain-path "~/Nextcloud/org/brain")
  ;; For Evil users
  (with-eval-after-load 'evil
    (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
  :config
  (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/doom-emacs/.org-id-locations")
  (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
  ;; (push '("b" "Brain" plain (function org-brain-goto-end)
  ;;         "* %i%?" :empty-lines 1)
  ;;       org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 255)
  (setq org-brain-include-file-entries nil
        org-brain-file-entries-use-title nil))

(use-package! org-journal
  :config
  (setq org-journal-dir "~/Nextcloud/org/journal"))

(use-package! fennel-mode
  :config
  (defun run-lisp-love ()
    "Run a love repl with run lisp"
    (interactive)
    (run-lisp "love ."))

  (map! :leader
        :prefix ("r" . "run")
        :desc "Run a love repl"
        "l" #'run-lisp-love))

(use-package org-roam
  :init
  (setq my/daily-note-filename "%<%Y-%m-%d>.org"
        my/daily-note-header "#+title: %<%Y-%m-%d %a>\n#+filetags:daily\n\n[[roam:%<%Y-%B>]]\n\n")
  :config
  (setq org-roam-directory "~/Nextcloud/org/roam")
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-completion-everywhere t)

  ;; credits: https://org-roam.discourse.group/t/using-consult-ripgrep-with-org-roam-for-searching-notes/1226
  (defun my/org-roam-rg-search ()
    "Search org-roam directory using consult-ripgrep. With live-preview."
    (interactive)
    (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
      (consult-ripgrep org-roam-directory)))

  (setq org-roam-dailies-capture-templates
    `(("d" "default" plain
      "* %?"
      :if-new (file+head ,my/daily-note-filename
                         ,my/daily-note-header)
      :empty-lines 1)

     ("t" "task" plain
      "** TODO %i%? \n SCHEDULED: %^T"
      :if-new (file+head+olp ,my/daily-note-filename
                             ,my/daily-note-header
                             ("Tasks"))
      :empty-lines 1)

     ;; for links to notes from reading, random commonplace thoughts, basically somewhere to put stuff
     ("l" "log" plain
      "** %<%I:%M %p> \n%?"
      :if-new (file+head+olp ,my/daily-note-filename
                             ,my/daily-note-header
                             ("Daily Log"))
      :empty-lines 1)

     ("j" "journal" plain
      "** %<%I:%M %p>  :journal:\n\n%?\n\n"
      :if-new (file+head+olp ,my/daily-note-filename
                             ,my/daily-note-header
                             ("Journal"))
      :empty-lines 1)
     ("m" "meeting" entry
      "** %<%I:%M %p> - %^{Meeting Title}  :meeting:\n\n%?\n\n"
      :if-new (file+head+olp ,my/daily-note-filename
                             ,my/daily-note-header
                             ("Meetings"))
      :empty-lines 1)))

  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)

          ("b" "book notes" plain
           "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)

          ("c" "video/article notes" plain
           "\n* Source\n\nLink: %^{Link}\nTitle: ${title}\n\n* Fleeting Notes\n\n%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)

          ("l" "literature note" plain
           "\n* Notes\n\n* Resources\n\n%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: literature#+SETUPFILE: ")
           :unnarrowed t)

          ("z" "zettel" plain
           "\n* Content\n\n* See also\n\n%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: zettel\n#+SETUPFILE: ")
           :unnarrowed t)

          ("w" "wiki" plain
           "\n* Content\n\n* See also\n\n%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: wiki\n#+SETUPFILE: ")
           :unnarrowed t)
          ))

  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(map! :leader
      "x" nil
      (:prefix ("x" . "split")
       :desc "window ops and scratching"
       "k"   #'kill-buffer
       "s s" #'doom/open-scratch-buffer
       "s r" #'split-window-right
       "s b" #'split-window-below))

(setq display-time-format "%H:%M %a,%d %b %Y")
(setq display-time-default-load-average nil)
(display-time)

(global-auto-revert-mode 1)

(map! :map org-mode-map
      :localleader
      "A" #'org-archive-hierarchically)

(use-package ox-altacv
  :init (require 'ox-altacv)
  :config
  (setq org-latex-compiler "pdflatex"))

(use-package mu4e
  :config
  (setq mu4e-update-interval 300)
  (setq mu4e-get-mail-command "$(which mbsync) -Va")
  (setq mu4e-contexts
    `( ,(make-mu4e-context
          :name "Gmail"
          :enter-func (lambda () (mu4e-message "Entering Gmail context"))
          :match-func (lambda (msg)
                        (when msg
                          (string-match-p "^/gmail" (mu4e-message-field msg :maildir))))
          :vars '( ( user-mail-address        . "nimor784@gmail.com" )
                   ( user-full-name           . "Georgi Bozhinov")
                   ( mu4e-sent-folder         . "/gmail/[Gmail]/Sent Mail")
                   ( mu4e-trash-folder        . "/gmail/[Gmail]/Trash")
                   ( mu4e-drafts-folder       . "/gmail/[Gmail]/Drafts")
                   (smtpmail-smtp-server      . "smtp.gmail.com")
                   (smtpmail-smtp-service     . 587)
                   (smtpmail-stream-type      . starttls)
                   (smtpmail-debug-info       . t)))
       ,(make-mu4e-context
          :name "Outlook"
          :enter-func (lambda () (mu4e-message "Entering Outlook context"))
          :match-func (lambda (msg)
                        (when msg
                          (string-match-p "^/outlook" (mu4e-message-field msg :maildir))))
          :vars '( ( user-mail-address    . "georgi.bojinov@hotmail.com" )
                   ( user-full-name       . "Georgi Bozhinov")
                   ( mu4e-sent-folder     . "/outlook/Sent")
                   ( mu4e-trash-folder    . "/outlook/Deleted")
                   ( mu4e-drafts-folder   . "/outlook/Drafts")
                   (smtpmail-smtp-server  . "smtp.office365.com")
                   (smtpmail-smtp-service . 587)
                   (smtpmail-stream-type  . starttls)
                   (smtp-debug-info       . t))))))

(use-package crux
  :config
  (map! :leader
        "r s" #'crux-create-scratch-buffer))

(use-package ledger
  :mode (("\\.journal\\'" . ledger-mode)))
