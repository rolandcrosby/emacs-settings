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
	     magit
	     pos-tip
	     flycheck-pos-tip))
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

; graphical tooltips for flycheck errors
(when (display-graphic-p (selected-frame))
    (require 'pos-tip)
    (require 'flycheck-pos-tip)
    (eval-after-load 'flycheck
	'(custom-set-variables
		'(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))

(setq-default indicate-empty-lines nil)
(set-fringe-style nil)
(setq magit-last-seen-setup-instructions "1.4.0") ; suppress magit warning at startup
(evil-ex-define-cmd "git" 'magit-status)

(global-visual-line-mode 1)
(auto-fill-mode -1)

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)


; cursor colors matching solarized vim theme
(setq evil-emacs-state-cursor '("#cb4b16" box)) ; orange
(setq evil-normal-state-cursor '("#268bd2" box)) ; blue
(setq evil-visual-state-cursor '("#d33682" box)) ; magenta
(setq evil-insert-state-cursor '("#859900" bar)) ; green
(setq evil-replace-state-cursor '("#859900" bar)) ; green
(setq evil-operator-state-cursor '("#268bd2" hollow)) ; blue

(blink-cursor-mode -1)

(defun reveal-in-finder ()
  (interactive)
  (start-process "finder" nil "open"  "--reveal" buffer-file-name))
(define-key evil-normal-state-map "gof" 'reveal-in-finder)

(defun open-terminal-here ()
  (interactive)
  (start-process "iterm" nil "open" "-a" "iTerm" (file-name-directory buffer-file-name)))
(define-key evil-normal-state-map "got" 'open-terminal-here)

; todo: unmap ctrl-c
; ZZ to be cmd-s cmd-w
; ZQ cmd-w don't save
; comment/uncomment
; disable non-graphical tooltips in flycheck
; disable weird indentation
; figure out modeline in aquamacs
