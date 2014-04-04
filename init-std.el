;; Global customization
;;;;;;;;;;;;;;;;;;;;;;;

(setq
 initial-scratch-message ""                ;; Empty scratch buffer
 initial-major-mode 'fundamental-mode      ;;   ... in fundamental-mode
 inhibit-splash-screen t                   ;; No fancy splash screen
 set-mark-command-repeat-pop t             ;; Easily cycle through the Mark Ring
 visible-bell t)                           ;; Visible bell
(define-coding-system-alias 'UTF-8 'utf-8)



;; Global minor modes
(show-paren-mode 1)                        ;; Parenthesis matching
(column-number-mode 1)                     ;; Show line and column numbers
(line-number-mode 1)                       ;;
(winner-mode 1)                            ;; Navigate through window layouts with C-c <arrows>
(browse-kill-ring-default-keybindings)     ;; Browse the kill ring using M-y



;; Custom key bindings
(global-set-key (kbd "M-g")     'goto-line)        ;; better keybinding for goto-line
(global-set-key (kbd "C-c q")   'join-line)        ;; join this line and the previous one
(global-set-key (kbd "C-z")     nil)               ;; don't suspend emacs on C-z (but C-x C-z still works)
(global-set-key (kbd "<f5>")    'recompile)        ;; re-run last compilation command

;;; Isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)



;; Useful standard minor modes
(defun ff/highlight-line ()
  "Turn on and setup hl-line-mode"
  (hl-line-mode 1))

(defun ff/truncate-lines ()
  "Truncate long lines"
  (toggle-truncate-lines 1))



;; Use S-<arrows> to switch between windows
(use-package windmove
  :init   (windmove-default-keybindings)
  :config (setq windmove-wrap-around t))

(use-package org
  :defer  t
  :config (progn
            ;; Reclaim S-<arrows> keys in org-mode
            (define-key org-mode-map (kbd "S-<left>")  nil) ;; Left/Right are directly reclaimed
            (define-key org-mode-map (kbd "S-<right>") nil)
            (add-hook 'org-shiftup-final-hook   'windmove-up)     ;; Up/Down are kept for places where
            (add-hook 'org-shiftdown-final-hook 'windmove-down))) ;; they are useful (dates and such)

(use-package org-agenda
  :defer  t
  :config (progn
            ;; Reclaim S-<arrows> keys in org-agenda-mode
            (define-key org-agenda-mode-map (kbd "S-<left>")  nil)
            (define-key org-agenda-mode-map (kbd "S-<right>") nil)
            (define-key org-agenda-mode-map (kbd "S-<up>")    nil)
            (define-key org-agenda-mode-map (kbd "S-<down>")  nil)))



;; Uniquify buffer names
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward
		uniquify-separator         ":"))



;; Better buffers list
(use-package ibuffer
  :commands ibuffer
  :init     (defalias 'list-buffers 'ibuffer)
  :config   (load "setup-ibuffer"))



;; Directory listing
(use-package dired
  :defer  t
  :bind   ("C-x C-j" . dired-jump)
  :config (progn
	    (add-hook 'dired-mode-hook 'ff/highlight-line)
	    (add-hook 'dired-mode-hook 'ff/truncate-lines)))



;; Recent files

(use-package recentf
  :defer  t
  :idle   (with-timer "Initializing recentf"
            (recentf-mode 1))
  :config (progn
            (setq recentf-max-saved-items 1000)
            (setq recentf-auto-cleanup    60)
            (setq recentf-save-file (ff/variable-file "recentf"))
            (use-package sync-recentf
              :load-path "share/elisp/sync-recentf")))



;; Ido mode
(use-package ido
  :init (progn
          (ido-mode 1)
          (ido-everywhere 1)

          ;; find recent files using C-x C-r
          (global-set-key
           (kbd "C-x C-r")
           (defun ido-recentf-open ()
             "Use `ido-completing-read' to \\[find-file] a recent file"
             (interactive)
             (recentf-mode 1)
             (if (find-file (ido-completing-read "Find recent file: " recentf-list))
                 (message "Opening file...")
               (message "Aborting")))))

  :config (setq ido-enable-flex-matching               t
                ido-auto-merge-work-directories-length -1
                ido-create-new-buffer                  'always
                ido-use-filename-at-point              'guess
                ido-default-buffer-method              'selected-window))



;; Auto-revert-mode for version-controlled files
(defadvice vc-find-file-hook (after ff/auto-revert-mode-for-vc activate)
  "Activate `auto-revert-mode' for vc-controlled files"
  (when vc-mode (auto-revert-mode 1)))



;; Hide-show mode
(use-package hideshow
  :defer   t
  :init    (defun ff/turn-on-hideshow ()
             "Turn on Hide-Show mode"
             (hs-minor-mode 1))
  :config  (progn
             (defun ff/hs-show-block-nonrecursive ()
               "Show current block non-recursively (i.e. sub-blocks remain hidden)"
               (interactive)
               (hs-show-block)
               (hs-hide-level 0))
             (defun ff/hs-show (&optional arg)
               "Show block at point or if ARG is present, all blocks."
               (interactive "P")
               (if arg
                   (hs-show-all)
                 (hs-show-block)))
             (define-key hs-minor-mode-map (kbd "M-<right>") 'ff/hs-show-block-nonrecursive)
             (define-key hs-minor-mode-map (kbd "M-<left>")  'hs-hide-block)
             (define-key hs-minor-mode-map (kbd "M-<up>")    'hs-hide-all)
             (define-key hs-minor-mode-map (kbd "M-<down>")  'ff/hs-show)))



;; ANSI terminal
(use-package term
  :defer  t
  :config (progn
            (setq term-buffer-maximum-size 100000)
            (setq ansi-term-color-vector ;; ANSI Term colors
                  [unspecified "#000000" "#b21818" "#18b218" "#BE5F00"
                               "#6D85BA" "#b218b2" "#18b2b2" "#b2b2b2"])

            (defmacro ff/term-send-raw (name binding string)
              (let ((fun-symbol (intern (format "ff/term-send-%s" name))))
                `(define-key term-raw-map (kbd ,binding)
                   (defun ,fun-symbol ()
                     ,(format "Send \"%s\" as raw characters to the terminal process." string)
                     (interactive)
                     (term-send-raw-string ,string)))))
            (ff/term-send-raw "Cright" "C-<right>"     "\e[1;5C")
            (ff/term-send-raw "Cleft"  "C-<left>"      "\e[1;5D")
            (ff/term-send-raw "Cw"     "C-<backspace>" "\C-w")))

;;; Frequently used programs in ansi-term
(defun python-term ()
  "Open a python terminal."
  (interactive)
  (ansi-term "/usr/bin/ipython" "Python"))

(defvar ssh-term-hosts nil
  "Frequently used ssh hosts.")
(defun ssh-term (argp)
  "Connect to a remote host by SSH.
Frequently used host names can be interactively completed from `ssh-term-hosts'."
  (interactive "P")
  (let ((host (completing-read "Host: " ssh-term-hosts))
        (user (if argp (read-from-minibuffer "User: ") ""))
        (port (if argp (read-from-minibuffer "Port: ") "")))
    (let ((switches (list host)))
      (when (not (equal user ""))
        (setq switches (cons "-l" (cons user switches))))
      (when (not (equal port ""))
        (setq switches (cons "-p" (cons port switches))))
      (switch-to-buffer (apply 'make-term (concat "ssh " host) "ssh" nil switches))
      (term-mode)
      (term-char-mode))))



;; Compilation mode
(use-package compile
  :defer  t
  :config (load "setup-compile"))



;; GNU/global
(use-package gtags
  :defer  t
  :config (define-key gtags-mode-map (kbd "M-,") 'gtags-find-rtag))

(defun ff/turn-on-gtags ()
  "Turn `gtags-mode' on if a global tags file has been generated.

This function asynchronously runs 'global -u' to update global
tags. When the command successfully returns, `gtags-mode' is
turned on."
  (let ((process (start-process "global -u"
                                "*global output*"
                                "global" "-u"))
        (buffer  (current-buffer)))
    (set-process-sentinel
     process
     `(lambda (process event)
        (when (and (eq (process-status process) 'exit)
                   (eq (process-exit-status process) 0))
          (with-current-buffer ,buffer
            (message "Activating gtags-mode")
            (gtags-mode 1)))))))



;; Spell check
(use-package flyspell
  :defer  t
  :config (progn
            ;; Arrange for the entire buffer to be checked when `flyspell-mode' is activated
            (defun ff/flyspell-buffer-after-activation ()
              "Run `flyspell-buffer' after `flyspell-mode' is activated."
              (when flyspell-mode
                (flyspell-buffer)))
            (add-hook 'flyspell-mode-hook 'ff/flyspell-buffer-after-activation)

            ;; Use <F1> to correct the word at point
            (define-key flyspell-mode-map (kbd "<f1>") 'flyspell-correct-word-before-point)))

(defun ff/turn-on-flyspell ()
  "Turn `flyspell-mode' on."
  (flyspell-mode 1))



;; Isend
(use-package isend
  :load-path "share/elisp/isend"
  :commands  isend
  :config    (progn
               (add-hook 'isend-mode-hook 'isend-default-shell-setup)
               (add-hook 'isend-mode-hook 'isend-default-ipython-setup)))



;; SLURM
(use-package slurm
  :load-path "share/elisp/slurm")



;; Miscellaneous functions
(use-package ff-misc
  :commands (count-words ff/count-lines-region)
  :bind     (("C-S-<up>"   . move-line-up)
             ("C-S-<down>" . move-line-down)
             ("H-<left>"   . rotate-windows)
             ("H-<right>"  . rotate-windows-backwards))
  :init     (define-key global-map [remap count-lines-region] 'ff/count-lines-region))

;; Set mark before scrolling
(defadvice scroll-up (before set-mark activate)
  "Set the mark before scrolling"
  (push-mark))
(defadvice scroll-down (before set-mark activate)
  "Set the mark before scrolling"
  (push-mark))




;; Mode-specific customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hide-show for programming modes
(add-hook 'c-mode-common-hook    'ff/turn-on-hideshow)
(add-hook 'lisp-mode-hook        'ff/turn-on-hideshow)
(add-hook 'emacs-lisp-mode-hook  'ff/turn-on-hideshow)
(add-hook 'python-mode-hook      'ff/turn-on-hideshow)
(add-hook 'sh-mode-hook          'ff/turn-on-hideshow)
(add-hook 'octave-mode-hook      'ff/turn-on-hideshow)



;; C/C++
(use-package "cc-mode"
  :defer  t
  :config (progn
            ;; switch between header and implementation files
            (define-key c-mode-map   (kbd "C-c o") 'ff-find-other-file)
            (define-key c++-mode-map (kbd "C-c o") 'ff-find-other-file)

            ;; GNU/global
            (add-hook 'c-mode-common-hook 'ff/turn-on-gtags)))



;; LaTeX
(use-package latex
  :defer  t
  :config (progn
            (setq reftex-label-alist '(AMSTeX)) ;; Use \eqref for equation references
            (setq-default TeX-PDF-mode t)       ;; Use pdflatex by default

            ;; Fix incompatibility between latex-preview and gs versions
            (setq preview-gs-options '("-q" "-dNOSAFER" "-dNOPAUSE" "-DNOPLATFONTS"
                                       "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4"))

            (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
            (add-hook 'LaTeX-mode-hook (defun ff/turn-on-latex-math-mode ()
                                         (LaTeX-math-mode 1)))
            (add-hook 'LaTeX-mode-hook (defun ff/turn-on-TeX-PDF-mode ()
                                         (TeX-PDF-mode 1)))))



;; Gnuplot-mode
(use-package gnuplot
  :defer  t
  :config (progn
            ;; don't display the gnuplot window
            (setq gnuplot-display-process nil)))



;; Octave / Matlab
(use-package octave-mod
  :mode        ("\\.m\\'" . octave-mode)
  :interpreter ("octave"  . octave-mode))


(provide 'init-std)
