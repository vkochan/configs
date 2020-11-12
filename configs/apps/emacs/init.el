;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
  (lambda ()
    (message "*** Emacs loaded in %s with %d garbage collections."
      (format "%.2f seconds"
	(float-time
	  (time-subtract after-init-time before-init-time))) 
      gcs-done)))

;; Initialize package sources
(require 'package)

(setq package-archives '(
	 ("melpa" . "https://melpa.org/packages/")
	 ("melpa-stable" . "https://stable.melpa.org/packages/")
	 ("org" . "https://orgmode.org/elpa/")
	 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
   (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
     (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t)

;; Uncomment this to get a reading on packages that get loaded at startup
;;(setq use-package-verbose t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq scroll-step            1
      scroll-conservatively  10000
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; UTF-8 as default encoding
(set-language-environment "utf-8")
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

(setq visible-bell t)             ;; Get rid of the beeps

(when (window-system)
  (tool-bar-mode 0)               ;; Toolbars were only cool with XEmacs
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (scroll-bar-mode -1))

(setq scroll-conservatively 10000
	scroll-preserve-screen-position t)

;; Always kill current buffer with "C-x k"
(defun bjm/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (delete-window))

(global-set-key (kbd "C-x k") 'bjm/kill-this-buffer)

;; save current position when closing the file
(save-place-mode t)

;; highlight current line
(global-hl-line-mode +1)

;; this will hide the initial tutorial
(setq inhibit-startup-message t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Evil
(defun dw/evil-hook ()
  (dolist (mode '(custom-mode
		   eshell-mode
		   git-rebase-mode
		   erc-mode
		   circe-server-mode
		   circe-chat-mode
		   circe-query-mode
		   sauron-mode
		   term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(defun dw/dont-arrow-me-bro ()
  (interactive)
  (message "Arrow keys are bad, you know?"))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-symbol-word-search t)
  :config
  (add-hook 'evil-mode-hook 'dw/evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Disable arrow keys in normal and visual modes
  (define-key evil-normal-state-map (kbd "<left>") 'dw/dont-arrow-me-bro)
  (define-key evil-normal-state-map (kbd "<right>") 'dw/dont-arrow-me-bro)
  (define-key evil-normal-state-map (kbd "<down>") 'dw/dont-arrow-me-bro)
  (define-key evil-normal-state-map (kbd "<up>") 'dw/dont-arrow-me-bro)
  (evil-global-set-key 'motion (kbd "<left>") 'dw/dont-arrow-me-bro)
  (evil-global-set-key 'motion (kbd "<right>") 'dw/dont-arrow-me-bro)
  (evil-global-set-key 'motion (kbd "<down>") 'dw/dont-arrow-me-bro)
  (evil-global-set-key 'motion (kbd "<up>") 'dw/dont-arrow-me-bro)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (evil-collection-init))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer my-leader-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

;; Global misc settings
(setq-default tab-width 8)
(setq-default evil-shift-width tab-width)

(setq-default indent-tabs-mode nil)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		 erc-mode-hook
		 term-mode-hook
		 eshell-mode-hook
		 vterm-mode-hook
		 neotree-mode-hook
		 telega-chat-mode-hook
		 telega-root-mode-hook
		 telega-webpage-mode-hook
		 dashboard-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package diminish)

(use-package hydra
  :defer 1)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
  :map ivy-minibuffer-map
    ("TAB" . ivy-alt-done)
    ("C-l" . ivy-alt-done)
    ("C-j" . ivy-next-line)
    ("C-k" . ivy-previous-line)
  :map ivy-switch-buffer-map
    ("C-k" . ivy-previous-line)
    ("C-l" . ivy-done)
    ("C-d" . ivy-switch-buffer-kill)
  :map ivy-reverse-i-search-map
    ("C-k" . ivy-previous-line)
    ("C-d" . ivy-reverse-i-search-kill))
  :init
    (ivy-mode 1)
  :config
    (setq ivy-use-virtual-buffers t)
    (setq ivy-wrap t)
    (setq ivy-count-format "(%d/%d) ")
    (setq enable-recursive-minibuffers t)

    ;; Use different regex strategies per completion command
    (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist) ;; This doesn't seem to work...
    (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
    (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

    ;; Set minibuffer height for different commands
    (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
    (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
    (setf (alist-get 'swiper ivy-height-alist) 15)
    (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

(use-package ivy-hydra
  :defer t
  :after hydra)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
  :map minibuffer-local-map
    ("C-r" . 'counsel-minibuffer-history))
  :config
    (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package flx  ;; Improves sorting for fuzzy-matched results
  :defer t
  :init
    (setq ivy-flx-limit 10000))

(use-package smex ;; Adds M-x recent command sorting for counsel-M-x
  :defer 1
  :after counsel)

(use-package wgrep)

(my-leader-def
  "r"   '(ivy-resume :which-key "ivy resume")
  "m"   '(mu4e :which-key "mail")
  "f"   '(:ignore t :which-key "files")
  "ff"  '(counsel-find-file :which-key "open file")
  "C-f" 'counsel-find-file
  "fr"  '(counsel-recentf :which-key "recent files")
  "fj"  '(counsel-file-jump :which-key "jump to file"))

(my-leader-def
  "b"   '(:ignore t :which-key "buffers")
  "bb"  'counsel-switch-buffer
  "bd"  'bury-buffer)

(my-leader-def
  "e"   '(:ignore t :which-key "eval")
  "eb"  '(eval-buffer :which-key "eval buffer"))

(my-leader-def
  :keymaps '(visual)
  "er" '(eval-region :which-key "eval region"))

(defun my-dired-touch (filename)
  (interactive (list (read-string "Filename: " "new")))
  (with-temp-buffer
    (write-file filename))
  (revert-buffer))


(use-package dired
  :ensure nil
  :defer 1
  :commands (dired dired-jump)
  :config
  (setq dired-listing-switches "-agho --group-directories-first"
     dired-omit-files "^\\.[^.].*"
     dired-omit-verbose nil)
  (autoload 'dired-omit-mode "dired-x")
  (add-hook 'dired-load-hook
    (lambda ()
	 (interactive)
	 (dired-collapse)))
  (add-hook 'dired-mode-hook
    (lambda ()
	 (interactive)
	 (dired-omit-mode 1)
	 (hl-line-mode 1)))
  (define-key dired-mode-map (kbd "C-c") 'my-dired-touch))

(use-package dired-rainbow
  :defer 2
  :config
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

(use-package dired-single
  :ensure t
  :defer t)

(use-package dired-ranger
  :defer t)

(use-package dired-collapse
  :defer t)

(evil-collection-define-key 'normal 'dired-mode-map
  "h" 'dired-single-up-directory
  "H" 'dired-omit-mode
  "l" 'dired-single-buffer
  "y" 'dired-ranger-copy
  "X" 'dired-ranger-move
  "p" 'dired-ranger-paste)

(defun dw/dired-link (path)
  (lexical-let ((target path))
	       (lambda () (interactive) (message "Path: %s" target) (dired target))))


(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

(use-package multi-term
  :commands multi-term-next
  :config
  (setq term-buffer-maximum-size 10000)
  (setq term-scroll-to-bottom-on-output t)
  (add-hook 'term-mode-hook
     (lambda ()
	 (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
	 (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next)))))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :after magit)

;; Add a super-convenient global binding for magit-status since
;; I use it 8 million times a day
(global-set-key (kbd "C-M-;") 'magit-status)

(my-leader-def
  "g"   '(:ignore t :which-key "git")
  "gB"  'magit-blame
  "gs"  'magit-status
  "gd"  'magit-diff-unstaged
  "gc"  'magit-branch-or-checkout
  "gl"   '(:ignore t :which-key "log")
  "glc" 'magit-log-current
  "glf" 'magit-log-buffer-file
  "gb"  'magit-branch
  "gP"  'magit-push-current
  "gp"  'magit-pull-branch
  "gf"  'magit-fetch
  "gF"  'magit-fetch-all
  "gr"  'magit-rebase)

(use-package forge
  :disabled)

(use-package magit-todos
  :defer t)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :after projectile
  :bind (
  :map projectile-command-map
    ("b" . 'counsel-projectile-switch-to-buffer)
    ("p" . 'counsel-projectile-switch-project))
)

(my-leader-def
  "pf"  'counsel-projectile-find-file
  "ps"  'counsel-projectile-switch-project
  "pp"  'counsel-projectile
  "pc"  'projectile-compile-project
  "po"  'projectile-dired
  "pg"  'projectile-grep
  "pa"  'projectile-add-known-project
  "pd"  'projectile-remove-known-project)

(defun dark/doom ()
  (interactive)
  (use-package doom-themes
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	  doom-themes-enable-italic t) ; if nil, italics is universally disabled
    (load-theme 'doom-acario-dark t)

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)

    ;; Enable custom neotree theme (all-the-icons must be installed!)
    ;; (doom-themes-neotree-config)
    ;; or for treemacs users
    ;; (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
    ;; (doom-themes-treemacs-config)

    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config))
)

(dark/doom)

(use-package message
	     :ensure nil
	     :custom (send-mail-function 'smtpmail-send-it))

(require 'mu4e)
(require 'mu4e-contrib)

(defun mail/work ()
  (use-package smtpmail
	       :ensure nil
	       :custom
	       (smtpmail-smtp-server "smtp.office365.com")
	       (smtpmail-smtp-service 587)
	       (smtpmail-smtp-user "vadym.kochan@plvision.eu")
	       (smtpmail-stream-type 'starttls))

  (setq	     mu4e-attachment-dir "~/Downloads"
	     mu4e-confirm-quit nil
	     mu4e-compose-dont-reply-to-self t
	     mu4e-compose-signature-auto-include nil
	     mu4e-get-mail-command "mbsync -a"
	     mu4e-index-update-in-background t
	     mu4e-html2text-command 'mu4e-shr2text
	     mu4e-maildir (expand-file-name "~/.mail/work")
             mu4e-compose-keep-self-cc t
	     mu4e-maildir-shortcuts
	     '( ("/Inbox" . ?i)
	       ("/Archive" . ?a)
	       ("/Drafts" . ?d)
	       ("/Deleted Items" . ?t)
	       ("/Sent Items" . ?s))
	     mu4e-update-interval 60
	     mu4e-use-fancy-chars t
	     mu4e-view-show-addresses t
	     mu4e-view-show-images t
	     message-send-mail-function 'smtpmail-send-it
	     message-kill-buffer-on-exit t
	     user-mail-address "vadym.kochan@plvision.eu"))

(defun mail/private ()
  (use-package smtpmail
	       :ensure nil
	       :custom
	       (smtpmail-smtp-server "smtp.gmail.com")
	       (smtpmail-smtp-service 587)
	       (smtpmail-smtp-user "vadim4j@gmail.com")
	       (smtpmail-stream-type 'starttls))

  (setq	     mu4e-attachment-dir "~/Downloads"
	     mu4e-confirm-quit nil
	     mu4e-compose-dont-reply-to-self t
	     mu4e-compose-signature-auto-include nil
	     mu4e-get-mail-command "mbsync -a"
	     mu4e-index-update-in-background t
	     mu4e-html2text-command 'mu4e-shr2text
	     mu4e-maildir (expand-file-name "~/.mail/private")
	     mu4e-maildir-shortcuts
	     '( ("/INBOX" . ?i)
	       ("/[Gmail].All Mail" . ?a)
	       ("/[Gmail].Trash" . ?t)
	       ("/[Gmail].Sent Mail" . ?s))
	     mu4e-update-interval 60
             mu4e-compose-keep-self-cc t
	     mu4e-use-fancy-chars t
	     mu4e-view-show-addresses t
	     mu4e-view-show-images t
	     message-send-mail-function 'smtpmail-send-it
	     message-kill-buffer-on-exit t
	     user-mail-address "vadim4j@gmail.com"))

(if (string= (getenv "MY_PROFILE") "work")
    (mail/work)
    ; else
    (mail/private))

(define-key mu4e-compose-mode-map (kbd "C-c C-x") 'mail-add-attachment)
(global-set-key (kbd "C-x m") 'mu4e)

(defun my-mail-status (n) (format "(✉ %s)" n))

(use-package mu4e-alert
	     :ensure t
	     :init
	     (mu4e-alert-enable-mode-line-display)
	     :custom
	     (mu4e-alert-modeline-formatter #'my-mail-status)
	     (mu4e-alert-interesting-mail-query
	       "flag:unread AND NOT flag:trashed"))

(require 'mu4e-actions)
(add-to-list 'mu4e-view-actions
             '("GitApply" . mu4e-action-git-apply-patch) t)
(add-to-list 'mu4e-view-actions
             '("MboxGitApply" . mu4e-action-git-apply-mbox) t)

(use-package doom-modeline
	     :defer 0.5
	     :config
	     (setq doom-modeline-mu4e t)
	     (doom-modeline-mode))

;; TODO: Mode this to another section
(setq-default fill-column 80)

;; Turn on indentation and auto-fill mode for Org files
(defun my/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (diminish org-indent-mode))

(use-package org
	     :defer t
	     :hook (org-mode . my/org-mode-setup)
	     :config
	     (setq org-ellipsis " ▾"
		   org-hide-emphasis-markers t
		   org-src-fontify-natively t
		   org-src-tab-acts-natively t
		   org-edit-src-content-indentation 0
		   org-hide-block-startup nil
		   org-src-preserve-indentation nil
		   org-startup-folded 'content
		   org-cycle-separator-lines 2
		   org-export-with-sub-superscripts nil)

	     ;; Taken and adapted from org-colored-text
	     (org-add-link-type
	       "color"
	       (lambda (path)
		 "No follow action.")
	       (lambda (color description backend)
		 (cond
		   ((eq backend 'latex)                  ; added by TL
		    (format "{\\color{%s}%s}" color description)) ; added by TL
		   ((eq backend 'html)
		    (let ((rgb (assoc color color-name-rgb-alist))
			  r g b)
		      (if rgb
			(progn
			  (setq r (* 255 (/ (nth 1 rgb) 65535.0))
				g (* 255 (/ (nth 2 rgb) 65535.0))
				b (* 255 (/ (nth 3 rgb) 65535.0)))
			  (format "<span style=\"color: rgb(%s,%s,%s)\">%s</span>"
				  (truncate r) (truncate g) (truncate b)
				  (or description color)))
			(format "No Color RGB for %s" color)))))))

	     (setq org-refile-targets '((nil :maxlevel . 3)
					(org-agenda-files :maxlevel . 3)))
	     (setq org-outline-path-complete-in-steps nil)
	     (setq org-refile-use-outline-path t)

	     (evil-define-key '(normal insert visual) org-mode-map (kbd "C-j") 'org-next-visible-heading)
	     (evil-define-key '(normal insert visual) org-mode-map (kbd "C-k") 'org-previous-visible-heading)

	     (evil-define-key '(normal insert visual) org-mode-map (kbd "M-j") 'org-metadown)
	     (evil-define-key '(normal insert visual) org-mode-map (kbd "M-k") 'org-metaup)

	     (org-babel-do-load-languages
	       'org-babel-load-languages
	       '((emacs-lisp . t)
		 (ledger . t)))

	     (push '("conf-unix" . conf-unix) org-src-lang-modes))

(use-package ox-pandoc)

(use-package ov)

(defun next-color-link (limit)
  (when (re-search-forward
	  "color:[a-zA-Z]\\{2,\\}" limit t)
    (forward-char -2)
    (let* ((next-link (org-element-context))
	   color beg end post-blanks)
      (if next-link
	(progn
	  (setq color (org-element-property :path next-link)
		beg (org-element-property :begin next-link)
		end (org-element-property :end next-link)
		post-blanks (org-element-property :post-blank next-link))
	  (set-match-data
	    (list beg
		  (- end post-blanks)))
	  (ov-clear beg end 'color)
	  (ov beg
	      (- end post-blanks)
	      'color t
	      'face
	      `((:foreground ,color)))
	  (goto-char end))
	(goto-char limit)
	nil))))

(add-hook 'org-mode-hook
	  (lambda ()
	    (font-lock-add-keywords
	      nil
	      '((next-color-link (0 'org-link t)))
	      t)))

;; Since we don't want to disable org-confirm-babel-evaluate all
;; of the time, do it around the after-save-hook
(defun my/org-babel-tangle-dont-ask ()
  ;; Dynamic scoping to the rescue
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'my/org-babel-tangle-dont-ask
					      'run-at-end 'only-in-org-mode)))

(use-package org-bullets
	     :after org
	     :hook (org-mode . org-bullets-mode)
	     :custom
	     (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
			'(("^ *\\([-]\\) "
			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(require 'org-indent)

(setq org-directory "~/notes")

(defun my/org-path (path)
  (expand-file-name path org-directory))

(defun my/search-org-files ()
  (interactive)
  (counsel-rg "" "~/notes" nil "Search Notes: "))

(use-package evil-org
	     :after org
	     :hook ((org-mode . evil-org-mode)
		    (org-agenda-mode . evil-org-mode)
		    (evil-org-mode . (lambda () (evil-org-set-key-theme '(navigation todo insert textobjects additional)))))
	     :config
	     (require 'evil-org-agenda)
	     (evil-org-agenda-set-keys))

(my-leader-def
  "o"   '(:ignore t :which-key "org mode")

  "oi"  '(:ignore t :which-key "insert")
  "oil" '(org-insert-link :which-key "insert link")

  "on"  '(org-toggle-narrow-to-subtree :which-key "toggle narrow")

  "os"  '(my/search-org-files :which-key "search notes")

  "oa"  '(org-agenda :which-key "status")
  "oc"  '(org-capture t :which-key "capture")
  "ox"  '(org-export-dispatch t :which-key "export"))

(use-package git-gutter
	     :ensure git-gutter-fringe
	     :hook ((prog-mode . git-gutter-mode)
		    (org-mode . git-gutter-mode)))

(add-to-list 'org-latex-packages-alist '("" "listings" nil))
(add-to-list 'org-latex-packages-alist '("" "lmodern" t))
(add-to-list 'org-latex-packages-alist '("" "color" t))

(setq org-latex-listings-options '(("breaklines" "true")))
(setq org-latex-listings t)

;;(setq-default tab-always-indent 'complete)

;; example of a function that just insert a tab char
(defun my-insert-tab-char ()
  "insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t")
)

(global-set-key (kbd "TAB") 'my-insert-tab-char)
(global-set-key (kbd "<tab>") 'my-insert-tab-char)

;(require 'cc-mode)
;(add-to-list 'c-mode-common-hook
;	     (lambda () (setq c-syntactic-indentation nil)))

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    ;; Add kernel style
	    (c-add-style
	      "linux-tabs-only"
	      '("linux" (c-offsets-alist
			  (arglist-cont-nonempty
			    c-lineup-gcc-asm-reg
			    c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
	  (lambda ()
	    (let ((filename (buffer-file-name)))
	      ;; Enable kernel mode for the appropriate files
		(setq indent-tabs-mode t)
		(setq show-trailing-whitespace t)
		(c-set-style "linux-tabs-only"))))
