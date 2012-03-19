;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs configuration file ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Useful variables and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d" 'append)
(setq homeDir (getenv "HOME"))
(setq fullhostname (system-name))
(setq hostname
      (substring fullhostname 0 (progn
				  (string-match "\\." (concat fullhostname ".domain"))
				  (- (match-end 0) 1))))

(defun ff/load-file-if-exists (f)
  "Load a file only if it exists"
  (if (file-exists-p f)
      (load-file f)))

(defun ff/add-hooks (hooks functions)
  "Add each function in FUNCTIONS to all hooks in HOOKS.

Example:
  (ff/add-hooks (list 'myhook1 'myhook2)
                (list 'myfunction1 'myfunction2)) "
  (mapcar (lambda (function)
            (mapcar (lambda (hook)
                      (add-hook hook function))
                    hooks))
          functions))


;; Extensions management
(defun ff/require-or-install (p)
  "Require a package."
  (require p nil 'noerror))
(when (load (expand-file-name "~/.emacs.d/elpa/package.el") 'noerror)
  (add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize)
  (defun ff/require-or-install (p)
    "Require a package or install it."
    (if (require p nil 'noerror)
        (message "Loaded package %s" p)
      (progn
        (message "Trying to install package %s" p)
        (condition-case ex
            (package-install p)
          ('error (progn
                    (message (format "%s" ex))
                    nil)))))))


;; This seems to be needed to avoid errors
(when (not (boundp 'warning-suppress-types))
  (setq warning-suppress-types nil))


;; Load local rc file
(ff/load-file-if-exists (concat homeDir "/.etc/emacs." hostname))




;; Global customization
;;;;;;;;;;;;;;;;;;;;;;;

(menu-bar-mode   -1)                ;; Bare GUI
(tool-bar-mode   -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
(setq-default frame-title-format    ;; Window title
              (list "%b Emacs"))
(mouse-wheel-mode 1)                ;; Enable mouse scrolling
(show-paren-mode  1)                ;; Parenthesis matching
(setq show-paren-style 'mixed)
(setq-default hilight-paren-expression t)
(setq visible-bell t)               ;; Visible bell
(setq set-mark-command-repeat-pop t);; Easily cycle through the Mark Ring
(setq shift-select-mode nil)        ;; No shift selection
(setq mouse-yank-at-point t)        ;; Yank at point (like in a tty)
(transient-mark-mode 1)             ;; Show active region
(column-number-mode 1)              ;; Show line and column numbers
(line-number-mode 1)
(global-font-lock-mode 1)           ;; Syntax highlighting
(setq font-lock-maximum-decoration t)
(setq-default indent-tabs-mode nil) ;; Indent with spaces
(setq whitespace-line-column 80     ;; Better whitespace-mode defaults
      whitespace-style '(face trailing lines-tail tabs))
(set-default 'imenu-auto-rescan t)  ;; Imenu shoud always rescan the buffers
(windmove-default-keybindings)      ;; use S-<arrows> to switch between windows
(winner-mode 1)                     ;; Navigate through window layouts with C-c <arrows>
(add-hook 'after-save-hook          ;; Automatically make shebang-ed scripts executable
          'executable-make-buffer-file-executable-if-script-p)
(setq diff-switches "-u")           ;; Unified diffs
(file-cache-add-directory "~/.etc") ;; Add '.etc' to file cache (C-x C-f C-<tab>)
(custom-set-variables               ;; Bookmarks file
 '(bookmark-default-file "~/.emacs.d/bookmarks"))
(setq custom-file "~/.emacs.d/custom.el") ;; Separate custom file
(load custom-file 'noerror)
(defalias 'yes-or-no-p 'y-or-n-p)   ;; Don't bother typing 'yes'


;; Enable some "forbidden" commands
(put 'set-goal-column 'disabled nil)     ;; set-goal-column (C-x C-n)
(put 'ido-exit-minibuffer 'disabled nil)


;; Custom key bindings
(global-set-key (kbd "M-g")   'goto-line) ;; better keybinding for goto-line
(global-set-key (kbd "C-x C-i") 'imenu)   ;; jump to function definition
(global-set-key (kbd "C-h a") 'apropos)   ;; search everything, not just commands
(global-set-key (kbd "C-c q") 'join-line) ;; join this line and the previous one
(global-set-key (kbd "M-y")   'browse-kill-ring) ;; navigate in the kill ring
(global-set-key (kbd "M-j")   'next-line) ;; VI-like movement with M-{h,j,k,l}
(global-set-key (kbd "M-k")   'previous-line)
(global-set-key (kbd "M-h")   'backward-char)
(global-set-key (kbd "M-l")   'forward-char)
(define-key isearch-mode-map (kbd "C-o")  ;; occur-mode for Isearch
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))


;; Make buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")


;; No semantic.cache cluttering
(let ((mySemanticRep (concat "/tmp/semantic.cache-" (getenv "USER"))))
  (progn
    (if (not (file-accessible-directory-p mySemanticRep))
	(make-directory mySemanticRep))
    (setq semanticdb-default-save-directory mySemanticRep)))


;; Color-theme
(defun ff/set-color-theme-hook (frame)
  (select-frame frame)
  (ff/set-color-theme))
(defun ff/set-color-theme ()
  (when (window-system)
    (let ((color-theme-is-global nil))
      (when (ff/require-or-install 'color-theme-tango)
	(color-theme-tango)))))
(add-hook 'after-make-frame-functions 'ff/set-color-theme-hook)
(ff/set-color-theme)


;; Change behaviour of exchange-point-and-mark
(defun ff/exchange-point-and-mark (&optional arg)
  "Exchange point and mark.

Inverse standard behaviour of `exchange-point-and-mark' regarding
the prefix argument: a prefix ARG activates the region."
  (interactive "P")
  (if arg
      (exchange-point-and-mark nil)
    (exchange-point-and-mark t)))
(global-set-key (kbd "C-x C-x") 'ff/exchange-point-and-mark)


;; IBuffer
(ff/load-file-if-exists (concat homeDir "/.etc/emacs-ibuffer.el"))


;; Dired
(require 'dired-x)
(add-hook 'dired-mode-hook 'ff/turn-on-highlight-line)


;; ANSI terminal
(setq ansi-term-color-vector ;; ANSI Term colors
      [unspecified "#000000" "#b21818" "#18b218" "#BE5F00"
                   "#6D85BA" "#b218b2" "#18b2b2" "#b2b2b2"])
(defun python-term ()        ;; Custom function to open a python process
  "Open a python terminal"
  (interactive)
  (ansi-term "/usr/bin/python" "Python"))
(defun term-send-Cright () (interactive) (term-send-raw-string "\e[1;5C"))
(defun term-send-Cleft  () (interactive) (term-send-raw-string "\e[1;5D"))
(add-hook 'term-mode-hook 'ff/term-mode-map-Cdirections)
(defun ff/term-mode-map-Cdirections ()
  (define-key term-raw-map (kbd "C-<right>") 'term-send-Cright)
  (define-key term-raw-map (kbd "C-<left>")  'term-send-Cleft))


;; Ido-mode
(setq ido-enable-flex-matching t
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-everywhere t
      ido-default-buffer-method 'selected-window)
(ido-mode 1)
(global-set-key (kbd "C-x C-r") 'ido-recentf-open) ;; Find recent files using C-x C-r
(recentf-mode 1)
(setq recentf-max-saved-items 1000)
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))


;; Auto-revert-mode for version-controlled files
(defadvice vc-find-file-hook (after auto-revert-mode-for-vc activate)
  "vc-find-file-hook advice for activating auto-revert-mode"
  (when vc-mode (auto-revert-mode 1)))


;; Hide-show mode
(defun ff/hs-show-block-nonrecursive ()
  "Show current block non-recursively (i.e. sub-blocks remain hidden)"
  (interactive)
  (hs-show-block)
  (hs-hide-level 0))
(eval-after-load "hideshow"
  '(progn
     (define-key hs-minor-mode-map (kbd "M-<right>") 'ff/hs-show-block-nonrecursive)
     (define-key hs-minor-mode-map (kbd "M-<left>")  'hs-hide-block)
     (define-key hs-minor-mode-map (kbd "M-<up>")    'hs-hide-all)
     (define-key hs-minor-mode-map (kbd "M-<down>")  'hs-show-block)))
(defun ff/turn-on-hideshow ()
  "Turn on Hide-Show mode"
  (hs-minor-mode 1))


;; Highlight-line mode
(defun ff/turn-on-highlight-line ()
  "Turn on and setup hl-line-mode"
  (hl-line-mode 1))




;; Non standard extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ido-ubiquitous
(when (ff/require-or-install 'ido-ubiquitous)
  (ido-ubiquitous t))


;; Anything
(when (ff/require-or-install 'anything)
  (ff/require-or-install 'anything-match-plugin)
  (global-set-key (kbd "C-x C-a") 'anything))


;; Smex
(when (ff/require-or-install 'smex)
  (smex-initialize)
  ;; Enhanced M-x
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; Traditional M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))


;; Auto-complete
(defvar ff/auto-complete-ac-dict nil
  "Path to the auto-complete dictionnary")
(when (ff/require-or-install 'auto-complete-config)
  (when ff/auto-complete-ac-dict
    (add-to-list 'ac-dictionary-directories ff/auto-complete-ac-dict))
  (ac-config-default))


;; Yasnippet
(eval-after-load "yasnippet"
  '(progn
     (message "setup yasnippet")
     (yas/initialize)
     (yas/load-directory ff/yasnippet-directory)))
(defvar ff/yasnippet-is-installed nil
  "set this to non-nil if the yasnippet extension is installed")
(defun ff/turn-on-yasnippet ()
  "Don't do anything when yasnippet is not installed"
  (message "Yasnippet does not seem to be installed"))
(when ff/yasnippet-is-installed
  (defun ff/turn-on-yasnippet ()
    "Turn on yasnippet minor mode."
    (yas/minor-mode 1)))


;; Autopair
(defun ff/turn-on-autopair ()
  "Don't do anything when autopair is not installed")
(when (ff/require-or-install 'autopair)
  (defun ff/turn-on-autopair ()
    "Turn on autopair minor mode"
    (autopair-mode 1)))


;; Org-mode
(defvar ff/use-org nil
  "Set this to non-nil to use org-mode")
(when ff/use-org
  (when (ff/require-or-install 'org)
    (ff/load-file-if-exists (concat homeDir "/.etc/emacs-org.el")))
  (when (require 'org-shortcuts nil t) ; Org-clock-in shortcuts
    (add-hook 'org-clock-before-select-task-hook 'org-clock-insert-shortcuts)))


;; ISend-mode
(ff/load-file-if-exists (concat homeDir "/.etc/isend.el"))




;; Mode-specific customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Common helper function to be used for hooks
(defun ff/setup-todo-keywords ()
  "Highlight keywords like FIXME or TODO"
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|FIX\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(defun ff/remap-newline-indent ()
  "Remap <RET> to `newline-and-indent'."
  (local-set-key (kbd "RET") 'newline-and-indent))

(defun ff/setup-compile ()
  "Map <F5> to `recompile'."
  (local-set-key (kbd "<f5>") 'recompile))


;; Text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)


;; Easy compilation for C/C++ and LaTeX projects
(ff/add-hooks '(c-mode-common-hook LaTeX-mode-hook makefile-mode-hook compilation-mode-hook)
              '(ff/setup-compile))


;; Common features for programming modes
(ff/add-hooks (list 'c-mode-common-hook 'lisp-mode-hook 'emacs-lisp-mode-hook 'python-mode-hook
                    'sh-mode-hook 'octave-mode-hook 'LaTeX-mode-hook)
              (list 'ff/setup-todo-keywords 'ff/turn-on-hideshow 'ff/remap-newline-indent
                    'ff/turn-on-autopair))
(ff/add-hooks '(c-mode-common-hook LaTeX-mode-hook)
              '(ff/turn-on-yasnippet))


;; C-like modes
(eval-after-load "cc-mode"
  '(progn
     (define-key c-mode-map   (kbd "C-c o") 'ff-find-other-file)
     (define-key c++-mode-map (kbd "C-c o") 'ff-find-other-file)))


;; LaTeX-mode
(setq reftex-label-alist '(AMSTeX)) ;; Use \eqref for equation references
(add-hook 'LaTeX-mode-hook 'ff/latex-mode-hook)
(defun ff/latex-mode-hook ()
  (setq-default fill-column 100) ;; Larger fill column
  (turn-on-reftex)               ;; Enter reftex-mode
  (local-set-key (kbd "œ")       ;; Replace special characters
                 (lambda () 
                   (interactive)
                   (insert-char 123 1)(insert-char  92 1)(insert-char 111 1)
                   (insert-char 101 1)(insert-char 125 1))))


;; Gnuplot-mode
(setq gnuplot-display-process nil) ;; dont display the gnuplot window


;; Octave-mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
