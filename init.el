;; Package configuration

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Package location
(let ((default-directory (file-name-directory (or load-file-name (buffer-file-name)))))
  (add-to-list 'load-path default-directory)
  (add-to-list 'load-path (concat default-directory "/packages/haskell-mode"))
  (add-to-list 'load-path (concat default-directory "/packages/auto-complete"))
  (add-to-list 'load-path (concat default-directory "/packages/s"))
  (add-to-list 'load-path (concat default-directory "/packages/f"))
  (add-to-list 'load-path (concat default-directory "/packages/dash"))
  (add-to-list 'load-path (concat default-directory "/packages/cl-lib"))
  (add-to-list 'load-path (concat default-directory "/packages/flycheck"))
  (add-to-list 'load-path (concat default-directory "/packages/color-theme"))
  (add-to-list 'load-path (concat default-directory "/packages/zenburn"))
  (add-to-list 'load-path (concat default-directory "/packages/emacs-calfw"))
  (add-to-list 'load-path (concat default-directory "/packages/mu4e"))
  (add-to-list 'load-path (concat default-directory "/packages/edit-server")))

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

;; ########## Org-mode #########
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
     
(setq org-crypt-key "92B3B329")
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
     
(setq auto-save-default nil)

(setq org-agenda-include-diary t)
(setq org-log-done t)
(setq org-agenda-files (list "~/org/work.org"
			     "~/org/home.org"))
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

(setq org-default-notes-file (concat org-directory "/notes.org"))

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
         ("/[Gmail].Sent Mail"   . ?s)
         ("/[Gmail].Trash"       . ?t)
         ("/[Gmail].All Mail"    . ?a)))

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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cfw:face-annotation ((t :foreground "RosyBrown" :inherit cfw:face-day-title)))
 '(cfw:face-day-title ((t :background "grey10")))
 '(cfw:face-default-content ((t :foreground "#bfebbf")))
 '(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
 '(cfw:face-disable ((t :foreground "DarkGray" :inherit cfw:face-day-title)))
 '(cfw:face-grid ((t :foreground "DarkGrey")))
 '(cfw:face-header ((t (:foreground "#d0bf8f" :weight bold))))
 '(cfw:face-holiday ((t :background "grey10" :foreground "#8c5353" :weight bold)))
 '(cfw:face-periods ((t :foreground "cyan")))
 '(cfw:face-saturday ((t :foreground "#8cd0d3" :background "grey10" :weight bold)))
 '(cfw:face-select ((t :background "#2f2f2f")))
 '(cfw:face-sunday ((t :foreground "#cc9393" :background "grey10" :weight bold)))
 '(cfw:face-title ((t (:foreground "#f0dfaf" :weight bold :height 2.0 :inherit variable-pitch))))
 '(cfw:face-today ((t :background: "grey10" :weight bold)))
 '(cfw:face-today-title ((t :background "#7f9f7f" :weight bold)))
 '(cfw:face-toolbar ((t :foreground "Steelblue4" :background "Steelblue4")))
 '(cfw:face-toolbar-button-off ((t :foreground "Gray10" :weight bold)))
 '(cfw:face-toolbar-button-on ((t :foreground "Gray50" :weight bold))))


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

;;(set-default-font "Inconsolata-10")

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
