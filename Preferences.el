;; This is the Aquamacs Preferences file.
;; Add Emacs-Lisp code here that should be executed whenever
;; you start Aquamacs Emacs. If errors occur, Aquamacs will stop
;; evaluating this file and print errors in the *Messags* buffer.
;; Use this file in place of ~/.emacs (which is loaded as well.)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

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
	     evil-snipe
	     solarized-theme
	     flycheck
	     company
	     magit))
  (require-package x))

(setq evil-search-module 'evil-search
      evil-want-C-u-scroll t
      evil-want-C-w-in-emacs-state t)

(require 'evil)
(evil-mode t)
(require 'evil-surround)
(global-evil-surround-mode 1)
(require 'evil-snipe)
(evil-snipe-mode 1)
(evil-snipe-override-mode 1)
(setq evil-snipe-repeat-keys t)
(setq evil-snipe-scope 'whole-visible)
(setq evil-snipe-repeat-scope 'whole-buffer)
(define-key evil-normal-state-map (kbd "<SPC>") 'evil-snipe-s)
(define-key evil-normal-state-map (kbd "s") 'evil-substitute)

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
(global-set-key [C-tab] 'next-tab-or-buffer)
(global-set-key [C-S-tab] 'previous-tab-or-buffer)
; todo: unmap ctrl-c, use esc to close emacs prompts
; ZZ to be cmd-s cmd-w
; ZQ cmd-w don't save
; comment/uncomment
; script or alias to open from cmd line

(setq-default indicate-empty-lines nil)
(set-fringe-style nil)
(setq magit-last-seen-setup-instructions "1.4.0") ; suppress magit warning at startup
(evil-ex-define-cmd "git" 'magit-status)

(global-visual-line-mode 1)
(auto-fill-mode -1)
