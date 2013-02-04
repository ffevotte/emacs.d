
;; Global customization
(setq
 initial-scratch-message ""           ;; Empty scratch buffer
 initial-major-mode 'fundamental-mode ;;   ... in fundamental-mode
 inhibit-splash-screen t              ;; No fancy splash screen
 set-mark-command-repeat-pop t        ;; Easily cycle through the Mark Ring
 visible-bell t)                      ;; Visible bell
(show-paren-mode 1)                   ;; Parenthesis matching
(column-number-mode 1)                ;; Show line and column numbers
(line-number-mode 1)                  ;;
(winner-mode 1)                       ;; Navigate through window layouts with C-c <arrows>
(define-coding-system-alias 'UTF-8 'utf-8)



;; Custom key bindings
(global-set-key (kbd "M-g")     'goto-line)        ;; better keybinding for goto-line
(global-set-key (kbd "C-c q")   'join-line)        ;; join this line and the previous one
(global-set-key (kbd "C-x C-r") 'ido-recentf-open) ;; find recent files using C-x C-r
(global-set-key (kbd "C-z")     nil)               ;; don't suspend emacs on C-z (but C-x C-z still works)
(global-set-key (kbd "<f5>")    'recompile)        ;; re-run last compilation command



;; Windmove (use S-<arrows> to switch between windows)
(windmove-default-keybindings)
(setq windmove-wrap-around t)

(eval-after-load "org"
  ;; Reclaim S-<arrows> keys in org-mode
  '(progn
     (define-key org-mode-map (kbd "S-<left>")  nil) ;; Left/Right are directly reclaimed
     (define-key org-mode-map (kbd "S-<right>") nil)
     (add-hook 'org-shiftup-final-hook   'windmove-up)     ;; Up/Down are kept for places where
     (add-hook 'org-shiftdown-final-hook 'windmove-down))) ;; they are useful (dates and such)

(eval-after-load "org-agenda"
  ;; Reclaim S-<arrows> keys in org-agenda-mode
  '(progn
     (define-key org-agenda-mode-map (kbd "S-<left>")  nil)
     (define-key org-agenda-mode-map (kbd "S-<right>") nil)
     (define-key org-agenda-mode-map (kbd "S-<up>")    nil)
     (define-key org-agenda-mode-map (kbd "S-<down>")  nil)))



;; Make buffer names unique
(require 'uniquify)
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator         ":")



;; Navigate in the kill ring using M-y
(browse-kill-ring-default-keybindings)



;; Isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)



;; TRAMP
(setq tramp-auto-save-directory "~/.emacs.d/tramp-auto-save")



;; Highlight-line mode
(defun ff/turn-on-highlight-line ()
  "Turn on and setup hl-line-mode"
  (hl-line-mode 1))



;; Truncate long lines
(defun ff/truncate-lines ()
  "Truncate long lines"
  (setq truncate-lines t))



;; IBuffer
(defalias 'list-buffers 'ibuffer)
(eval-after-load "ibuffer"
  '(progn
     (load "setup-ibuffer")
     (add-hook 'ibuffer-mode-hook 'ff/turn-on-highlight-line)))



;; ANSI terminal
(defun python-term ()
  "Open a python terminal."
  (interactive)
  (ansi-term "/usr/bin/ipython" "Python"))
(eval-after-load "term"
  '(progn
     (message "Setting up term...")
     (setq term-buffer-maximum-size 100000)
     (setq ansi-term-color-vector ;; ANSI Term colors
           [unspecified "#000000" "#b21818" "#18b218" "#BE5F00"
                        "#6D85BA" "#b218b2" "#18b2b2" "#b2b2b2"])
     (defun term-send-Cright () (interactive) (term-send-raw-string "\e[1;5C"))
     (defun term-send-Cleft  () (interactive) (term-send-raw-string "\e[1;5D"))
     (define-key term-raw-map (kbd "C-<right>") 'term-send-Cright)
     (define-key term-raw-map (kbd "C-<left>")  'term-send-Cleft)
     (message "Setting up term...done.")))



;; Dired
(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(add-hook 'dired-mode-hook 'ff/turn-on-highlight-line)
(add-hook 'dired-mode-hook 'ff/truncate-lines)



;; Recentf
(load "setup-recentf")



;; Ido mode
(setq
 ido-enable-flex-matching               t
 ido-auto-merge-work-directories-length -1
 ido-create-new-buffer                  'always
 ido-use-filename-at-point              'guess
 ido-default-buffer-method              'selected-window)
(ido-mode 1)
(ido-everywhere)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))



;; Auto-revert-mode for version-controlled files
(defadvice vc-find-file-hook (after ff/auto-revert-mode-for-vc activate)
  "vc-find-file-hook advice for activating auto-revert-mode"
  (when vc-mode (auto-revert-mode 1)))



;; Hide-show mode
(eval-after-load "hideshow"
  '(progn
     (message "Setting up hideshow...")
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
     (define-key hs-minor-mode-map (kbd "M-<down>")  'ff/hs-show)
     (message "Setting up hideshow...done.")))
(defun ff/turn-on-hideshow ()
  "Turn on Hide-Show mode"
  (hs-minor-mode 1))



;; Compilation mode
(setq compilation-scroll-output 'first-error) ;; scroll compilation buffer until first error
(eval-after-load "compile"
  '(load "setup-compile"))



;; gtags-mode
(eval-after-load "gtags"
  '(progn
     (define-key gtags-mode-map (kbd "M-,") 'gtags-find-rtag)))
(defun ff/turn-on-gtags ()
  "Turn `gtags-mode' on if a global tags file has been generated.

This function asynchronously runs 'global -u' to update global
tags. When the command successfully returns, `gtags-mode' is
turned on."
  (interactive)
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



;; Set mark before scrolling
(defadvice scroll-up (before set-mark activate)
  "Set the mark before scrolling"
  (push-mark))
(defadvice scroll-down (before set-mark activate)
  "Set the mark before scrolling"
  (push-mark))



;; Flyspell-mode
(eval-after-load "flyspell"
  '(progn
     ;; Arrange for the entire buffer to be highlighted when `flyspell-mode' is activated
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



;; ISend mode
(add-to-list 'load-path "~/.emacs.d/packages/isend")
(require 'isend nil t)



;; Slurm mode
(add-to-list 'load-path "~/.emacs.d/packages/slurm")
(require 'slurm nil t)



;; a2ps-multibyte (support for UTF-8 in a2ps)
(add-to-list 'load-path "~/.emacs.d/packages/a2ps-multibyte")
(require 'a2ps-multibyte)




;; Mode-specific customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hide-show for programming modes
(add-hook 'c-mode-common-hook    'ff/turn-on-hideshow)
(add-hook 'lisp-mode-hook        'ff/turn-on-hideshow)
(add-hook 'emacs-lisp-mode-hook  'ff/turn-on-hideshow)
(add-hook 'python-mode-hook      'ff/turn-on-hideshow)
(add-hook 'sh-mode-hook          'ff/turn-on-hideshow)
(add-hook 'octave-mode-hook      'ff/turn-on-hideshow)



;; C-like modes
(eval-after-load "cc-mode"
  '(progn
     ;; switch between header and implementation files
     (define-key c-mode-map   (kbd "C-c o") 'ff-find-other-file)
     (define-key c++-mode-map (kbd "C-c o") 'ff-find-other-file)))
(add-hook 'c-mode-common-hook 'ff/turn-on-gtags)



;; LaTeX

(setq reftex-label-alist '(AMSTeX)) ;; Use \eqref for equation references
(setq-default TeX-PDF-mode t)       ;; Use pdflatex by default

;; Turn on `reftex-mode', `LaTeX-math-mode' and `TeX-PDF-mode'
(defun turn-on-latex-math-mode () (LaTeX-math-mode 1))
(defun turn-on-TeX-PDF-mode () (TeX-PDF-mode 1))
(add-hook 'LaTeX-mode-hook 'turn-on-latex-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-TeX-PDF-mode)

;; Fix incompatibility between latex-preview and gs versions
(setq preview-gs-options '("-q" "-dNOSAFER" "-dNOPAUSE" "-DNOPLATFONTS"
                           "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4"))



;; Gnuplot-mode
(setq gnuplot-display-process nil)  ;; don't display the gnuplot window



;; Octave-mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
