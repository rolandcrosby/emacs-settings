;; This is the Aquamacs Preferences file.
;; Add Emacs-Lisp code here that should be executed whenever
;; you start Aquamacs Emacs. If errors occur, Aquamacs will stop
;; evaluating this file and print errors in the *Messags* buffer.
;; Use this file in place of ~/.emacs (which is loaded as well.)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;;; from purcell/emacs.d
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(package-initialize)

(dolist (x '(
	     evil
	     evil-surround
	     solarized-theme
	     powerline-evil
	     flycheck
	     company))
  (require-package x))

(setq evil-search-module 'evil-search
      evil-want-C-u-scroll t
      evil-want-C-w-in-emacs-state t)

(require 'evil)
(evil-mode t)
(require 'evil-surround)
(global-evil-surround-mode 1)
(require 'powerline-evil)

(require 'linum)
(setq linum-format " %d ")

(setq current-theme 'solarized-dark)
(load-theme current-theme)
(defun solarized-toggle-theme ()
  (interactive)
  (if (string= current-theme 'solarized-dark)
    (setq current-theme 'solarized-light)
    (setq current-theme 'solarized-dark))
  (load-theme current-theme))

(tool-bar-mode 0)
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'after-init-hook 'global-company-mode)
(define-key evil-normal-state-map "Q" 'close-window)
; todo: ctrl-tab, ctrl-shift-tab to move between tabs
