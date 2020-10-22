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

;; Makes *scratch* empty.
;; (setq initial-scratch-message "")

;; Removes *scratch* from buffer after the mode has been set.
;; (defun remove-scratch-buffer ()
;;   (if (get-buffer "*scratch*")
;;       (kill-buffer "*scratch*")))
;; (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

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

;; Theming
;; (use-package spacegray-theme :defer t)
;; (use-package doom-themes :defer t)
;; (load-theme 'doom-palenight t)

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
  :after projectile)

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
