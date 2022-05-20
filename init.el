(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(use-package emacs
  :init
    (require 'use-package)
    (setq ring-bell-function 'ignore)
    (setq inhibit-startup-message t)
    (setq initial-scratch-message nil))
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (global-visual-line-mode 1)
    (setq backup-by-copying t      ; don't clobber symlinks
          backup-directory-alist '(("." . "~/.emacs.d/.saves/"))    ; don't litter my fs tree
          delete-old-versions t
          kept-new-versions 6
          kept-old-versions 2
          version-control t)       ; use versioned backups
    (setq auto-save-file-name-transforms
          `((".*" "~/.emacs.d/.saves" t)))

(use-package exwm
  :ensure t
  :config
  (start-process-shell-command "setxkbmap" nil "setxkbmap -option ctrl:swapcaps")
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 5)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
    '(?\C-x
      ?\C-u
      ?\C-h
      ?\M-x))

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
	`(
	  ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
	  ([?\s-r] . exwm-reset)

	  ;; Move between windows
	  ([?\s-h] . windmove-left)
	  ([?\s-l] . windmove-right)
	  ([?\s-k] . windmove-up)
	  ([?\s-j] . windmove-down)

	  ;; Launch applications via shell command
	  ([?\s-&] . (lambda (command)
		       (interactive (list (read-shell-command "$ ")))
		       (start-process-shell-command command nil command)))

	  ;; Switch workspace
	  ([?\s-w] . exwm-workspace-switch)
	  ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

	  ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
	  ,@(mapcar (lambda (i)
		      `(,(kbd (format "s-%d" i)) .
			(lambda ()
			  (interactive)
			  (exwm-workspace-switch-create ,i))))
		    (number-sequence 0 9))))

  (exwm-enable))

(use-package org
  :ensure t
  :config
    (setq org-agenda-files 
      '("~/org/"))
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)
    (setq org-agenda-start-with-log-mode t)
    (setq org-use-property-inheritance t)
    (org-babel-do-load-languages
      'org-babel-load-languages
      '((lisp . t)
        (emacs-lisp . t)))
    (setq org-startup-indented t))

(use-package gruvbox-theme
  :ensure t
  :init
    (load-theme 'gruvbox-dark-soft t))

(use-package evil
  :ensure t
  :init 
    (evil-mode 1))

(use-package magit
  :ensure t
  :init)

(use-package vertico
  :ensure t  
  :init
    (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package savehist
  :ensure t 
  :init
   (savehist-mode))

(use-package pdf-tools
  :ensure t)

(use-package nov
  :ensure t
  :config
    (setq nov-unzip-program (executable-find "/usr/bin/unzip")))

(use-package slime
  :ensure t
  :config
    (setq inferior-lisp-program "sbcl"))
