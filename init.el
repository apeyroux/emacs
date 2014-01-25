;; Package configuration

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("org" . "http://orgmode.org/elpa/")))

;; Package location
(let ((default-directory (file-name-directory (or load-file-name (buffer-file-name)))))
  (add-to-list 'load-path default-directory)
  (add-to-list 'load-path (concat default-directory "/packages/zenburn"))
  (add-to-list 'load-path (concat default-directory "/packages/mu4e")))

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

;; ########## Require ##########
(require 'tramp)
(require 'color-theme)
(require 'zenburn)
(require 'mu4e)
(require 'smtpmail)
(require 'calfw)
(require 'org)
(require 'calfw-org)
(require 'org-mu4e)
(require 'org-feed)
(require 'org-crypt)
(require 'ox-reveal)
(require 'google-translate)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'edit-server)
(require 'undo-tree)

;; ########## EditServer ##########
(edit-server-start)

;; ########## AutoComplete ##########
;; dirty fix for having AC everywhere
(ac-config-default)
(global-auto-complete-mode t)
(auto-complete-mode t)

(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                         (auto-complete-mode 1))
                       ))
real-global-auto-complete-mode

;; ########## Haskell ##########
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; hslint on the command line only likes this indentation mode;
;; alternatives commented out below.
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;; Ignore compiled Haskell files in filename completions
(add-to-list 'completion-ignored-extensions ".hi")

(defun flymake-haskell-init ()
  "When flymake triggers, generates a tempfile containing the
  contents of the current buffer, runs `hslint` on it, and
  deletes file. Put this file path (and run `chmod a+x hslint`)
  to enable hslint: https://gist.github.com/1241073"
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "hslint" (list local-file))))

(defun flymake-haskell-enable ()
  "Enables flymake-mode for haskell, and sets <C-c d> as command
  to show current error."
  (when (and buffer-file-name
             (file-writable-p
              (file-name-directory buffer-file-name))
             (file-writable-p buffer-file-name))
    (local-set-key (kbd "C-c d") 'flymake-display-err-menu-for-current-line)
    (flymake-mode t)))

;; Forces flymake to underline bad lines, instead of fully
;; highlighting them; remove this if you prefer full highlighting.
(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))

(require 'hs-lint)    ;; https://gist.github.com/1241059
(require 'haskell-ac) ;; https://gist.github.com/1241063

(defun my-haskell-mode-hook ()
  "hs-lint binding, plus autocompletion and paredit."
  (local-set-key "\C-c c" 'hs-lint)
  (setq ac-sources
        (append '(ac-source-yasnippet
                  ac-source-abbrev
                  ac-source-words-in-buffer
                  my/ac-source-haskell)
                ac-sources))
  (dolist (x '(haskell literate-haskell))
    (add-hook
     (intern (concat (symbol-name x)
                     "-mode-hook"))
     'turn-on-paredit)))

(eval-after-load 'haskell-mode
  '(progn
     (require 'flymake)
     (push '("\\.l?hs\\'" flymake-haskell-init) flymake-allowed-file-name-masks)
     (add-hook 'haskell-mode-hook 'flymake-haskell-enable)
     (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)))

(add-hook 'haskell-mode-hook 'auto-complete-mode)
(add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion)

;; ########## Org-mode #########
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
     
(setq org-crypt-key "92B3B329")
;; GPG key to use for encryptionq
;; Either the Key ID or set to nil to use symmetric encryption.
     
(setq auto-save-default nil)

(setq org-agenda-include-diary t)
(setq org-log-done t)
;(setq org-agenda-files (list "~/org/work.org"
;			     "~/org/home.org"))
(setq org-agenda-files (list "~/org/home.org"))

;; Org RSS
(setq org-feed-alist
	  '(("Planet SBCL"
	     "http://planet.sbcl.org/rss20.xml"
	     "~/org/feeds.org" "Planet SBCL")
          ("Slashdot"
              "http://rss.slashdot.org/Slashdot/slashdot"
              "~/org/feeds.org" "Slashdot Entries")
	  ("Planet Haskell"
	     "http://planet.haskell.org/rss20.xml"
	     "~/org/feeds.org" "Planet Haskell")
	  ("Planet Lisp"
	     "http://planet.lisp.org/rss20.xml"
	     "~/org/feeds.org" "Planet Lisp")
	  ("Rosedu"
	     "http://techblog.rosedu.org/rss.xml"
	     "~/org/feeds.org" "Rosedu")
))

;; Org Capture
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "/todo.org") "Tasks")
         "* TODO %?\n %i\n")
        ("l" "Link" plain (file (concat org-directory "/liens.org"))
         "- %?\n %x\n")
	("r" "Read it later" plain (file (concat org-directory "/ril.org"))
         "- %?\n %a %x\n")))

;; (setq org-default-notes-file (concat org-directory "/notes.org"))

;; time stamp settings
(setq-default org-display-custom-times t)

(setq org-time-stamp-custom-formats '("<%a %d/%m/%Y>" . "<%a %d/%m/%Y %H:%M>"))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; ########## eMail (mu4e) ##########
(setq mu4e-maildir "~/mail/m@j4.pe")
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
;;(setq mu4e-sent-messages-behavior 'delete)

(setq mu4e-msg2pdf "/usr/bin/msg2pdf")
(setq mu4e-html2text-command "html2text -utf8 -width 72")

(add-hook 'mu4e-compose-mode-hook
   (defun my-setup-epa-hook ()
     (epa-mail-mode)))

(add-hook 'mu4e-view-mode-hook
  (defun my-view-mode-hook ()
   (epa-mail-mode)))

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)
         ("/[Gmail].Messages envoyés"   . ?s)
         ("/[Gmail].Corbeille"       . ?t)
         ("/[Gmail].Tous les messages"    . ?a)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")
;;      mu4e-update-interval 300)

;; something about ourselves
(setq user-mail-address "m@j4.pe")
(setq user-full-name  "Jean-Alexandre Peyroux")
(setq message-signature-file "~/.signature")
(setq message-signature "j4.pe")
(setq mu4e-compose-signature-auto-include nil)

(setq message-send-mail-function 'smtpmail-send-it
    smtpmail-stream-type 'starttls
    smtpmail-default-smtp-server "smtp.gmail.com"
    smtpmail-smtp-server "smtp.gmail.com"
    smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; ########## cfw ##########

;; Month
(setq calendar-month-name-array
  ["Janvier" "Fevrier" "Mars"     "Avril"   "Mai"      "Juin"
   "Juillet"    "Août"   "Septembre" "Octobre" "Novembre" "Decembre"])

;; Week days
(setq calendar-day-name-array
      ["Dimanche" "Lundi" "Mardi" "Mercredi" "Jeudi" "Vendredi" "Samedi"])

;; First day of the week
(setq calendar-week-start-day 1) ; 0:Dimanche, 1:Lundi

;; ########## Configuration emacs ##########

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(set-default-font "Inconsolata-10")

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")
(setq inhibit-startup-message t) 
(display-time-mode t)
(display-battery-mode t)

(windmove-default-keybindings 'meta)
(global-set-key (kbd "C-z") 'ido-switch-buffer)
(ido-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1) ;; pas de menu
(setq ac-ignore-case t)
(zenburn)
(setq-default ispell-program-name "aspell") ;; fr
(desktop-save-mode 1)

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(server-start)

(global-undo-tree-mode 1)
