;; Useful variables and functions
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


;; Extensions
(defun ff/require-or-install (p)
  "Require a package."
  (require p nil t))
(when (load (expand-file-name "~/.emacs.d/elpa/package.el"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize)
  (defun ff/require-or-install (p)
    "Require a package or install it."
    (if (require p nil t)
        (message "Loaded package %s" p)
      (progn
        (message "Trying to install package %s" p)
        (condition-case ex
            (package-install p)
          ('error (message (format "%s" ex))))))))

;; this seems to be needed to avoid errors
(when (not (boundp 'warning-suppress-types))
  (setq warning-suppress-types nil))


;; Load local rc file
(ff/load-file-if-exists (concat homeDir "/.etc/emacs." hostname))


;; Global options
(setq default-frame-alist 
      '((font-backend . "xft")      ;; Inconsolata font
        (font . "Inconsolata-11")))
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
(custom-set-variables               ;; Bookmarks file
 '(bookmark-default-file "~/.emacs.d/bookmarks"))
(setq custom-file "~/.emacs.d/custom.el") ;; Separate custom file
(load custom-file 'noerror)
(put 'set-goal-column 'disabled nil);; Enable set-goal-command (C-x C-n)
(defalias 'yes-or-no-p 'y-or-n-p)   ;; Don't bother typing 'yes'


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
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-separator ":"))


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
      (ff/require-or-install 'color-theme-tango)
      (color-theme-tango))))
(add-hook 'after-make-frame-functions 'ff/set-color-theme-hook)
(ff/set-color-theme)


;; IBuffer
(when (require 'ibuffer nil t)
  (defun ff/ibuffer-filter-by-filename (path)
    "Add ibuffer filter by filename using current buffer file name as default"
    (interactive (list
                  (let* ((buf  (ibuffer-current-buffer t))
                         (name (buffer-file-name buf)))
                    (read-from-minibuffer "Filter by path: " name))))
    (ibuffer-filter-by-filename path))

  (defun ff/ibuffer-hide-all-filters ()
    "Hide all ibuffer filter groups"
    (interactive)
    (save-excursion
      (if (not (eq major-mode 'ibuffer-mode))
          nil
        (progn
          (goto-char 0)
          (setq prev-point 0)
          (while (< prev-point (point))
            (setq prev-point (point))
            (ibuffer-forward-filter-group)
            (ff/ibuffer-hide-filter-group (point)))))))  
  (defun ff/ibuffer-show-all-filters ()
    "Show all ibuffer filter groups"
    (interactive)
    (save-excursion
      (if (not (eq major-mode 'ibuffer-mode))
          nil
        (progn
          (goto-char 0)
          (setq prev-point 0)
          (while (< prev-point (point))
            (setq prev-point (point))
            (ibuffer-forward-filter-group)
            (ff/ibuffer-show-filter-group (point)))))))
  (defun ff/ibuffer-hide-filter-group (posn)
    "Hide current filter group"
    (let ((name (get-text-property posn 'ibuffer-filter-group-name)))
      (unless (stringp name)
        (error "No filtering group name present"))
      (if (member name ibuffer-hidden-filter-groups)
          nil
        (push name ibuffer-hidden-filter-groups))
      (ibuffer-update nil t)))
  (defun ff/ibuffer-show-filter-group (posn)
    "Show current filter-group"
    (let ((name (get-text-property posn 'ibuffer-filter-group-name)))
      (unless (stringp name)
        (error "No filtering group name present"))
      (if (member name ibuffer-hidden-filter-groups)
          (setq ibuffer-hidden-filter-groups
                (delete name ibuffer-hidden-filter-groups)))
      (ibuffer-update nil t)))

  (add-hook 'ibuffer-mode-hook 'ff/ibuffer-mode-hook)
  (defun ff/ibuffer-mode-hook ()
    (ibuffer-switch-to-saved-filter-groups "default")
    (local-set-key (kbd "M-<up>")   'ff/ibuffer-hide-all-filters)
    (local-set-key (kbd "M-<down>") 'ff/ibuffer-show-all-filters)
    (local-set-key (kbd "/ f")      'ff/ibuffer-filter-by-filename))

  ;; Switching to ibuffer puts the cursor on the most recent buffer
  (defadvice ibuffer (around ibuffer-point-to-most-recent) ()
    "Open ibuffer with cursor pointed to most recent buffer name"
    (let ((recent-buffer-name (buffer-name)))
      ad-do-it
      (ff/ibuffer-hide-all-filters)
      (ibuffer-jump-to-buffer recent-buffer-name)))
  (ad-activate 'ibuffer)
  (global-set-key (kbd "C-x C-b") 'ibuffer))


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
(put 'ido-exit-minibuffer 'disabled nil)
(when (ff/require-or-install 'ido-ubiquitous)
  (ido-ubiquitous t))
(global-set-key (kbd "C-x C-r") 'ido-recentf-open) ;; Find recent files using C-x C-r
(recentf-mode 1)
(setq recentf-max-saved-items 1000)
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))


;; Anything
(when (ff/require-or-install 'anything) 
  (ff/require-or-install 'anything-match-plugin)
  (global-set-key (kbd "C-x C-a") 'anything))


;; File-cache
(file-cache-add-directory "~/.etc")


;; Smex
(when (ff/require-or-install 'smex)
  (smex-initialize)
  ;; Enhanced M-x
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; Traditional M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))


;; Auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories ff/auto-complete-ac-dict)
(ac-config-default)


;; Yasnippet
(when (ff/require-or-install 'yasnippet)
  (yas/initialize)
  (yas/load-directory "~/.emacs.d/elpa/yasnippet-0.6.1/snippets"))


;; Org-mode
(when (ff/require-or-install 'org)
  (ff/load-file-if-exists (concat homeDir "/.etc/emacs.org"))
  (ff/load-file-if-exists (concat homeDir "/.etc/emacs.org-bh")))


;; Auto-revert-mode for version-controlled files
(defadvice vc-find-file-hook (after auto-revert-mode-for-vc activate)
  "vc-find-file-hook advice for activating auto-revert-mode"
  (when vc-mode (auto-revert-mode 1)))


;; Text-mode
(add-hook 'text-mode-hook 'ff/text-mode-hook)
(defun ff/text-mode-hook ()
  (turn-on-auto-fill)            ;; Automatically wrap around
  (turn-on-flyspell))             ;; Enter flyspell-mode


;; Common features for programming modes
(defun ff/activate-todo-keywords ()
  "Highlight keywords like FIXME or TODO"
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|FIX\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))
(defun ff/activate-hideshow ()
  "Activate Hide-Show mode"
  (local-set-key (kbd "M-<right>") (lambda () (interactive)(hs-show-block)(hs-hide-level 0)))
  (local-set-key (kbd "M-<left>")  'hs-hide-block)
  (local-set-key (kbd "M-<up>")    'hs-hide-all)
  (local-set-key (kbd "M-<down>")  'hs-show-block)
  (hs-minor-mode 1))
(defun ff/activate-newline-indent ()
  "Remap <RET> to `newline-and-indent'."
  (local-set-key (kbd "RET") 'newline-and-indent))
(defun ff/activate-autopair ())
(when (ff/require-or-install 'autopair)
  (defun ff/activate-autopair ()
    (autopair-mode 1)))

(ff/add-hooks (list 'c-mode-common-hook 'lisp-mode-hook 'emacs-lisp-mode-hook 'python-mode-hook
                    'sh-mode-hook 'shell-mode-hook 'octave-mode-hook)
              (list 'ff/activate-todo-keywords 'ff/activate-hideshow 'ff/activate-newline-indent
                    'ff/activate-autopair))


(defun ff/compile ()
  "Rerun last compilation command."
  (interactive)
  (set-buffer "*compilation*")
  (compile compile-command))
(defun ff/activate-compile ()
  "Map <F5> to `ff/compile'."
  (local-set-key (kbd "<f5>") 'ff/compile))
(ff/add-hooks '(c-mode-common-hook LaTeX-mode-hook makefile-mode-hook)
              '(ff/activate-compile))


;; C-mode
(add-hook 'c-mode-common-hook 'ff/c-mode-common-hook)
(defun ff/c-mode-common-hook ()
  (local-set-key (kbd "C-c o") 'ff-find-other-file)) ;; Fast switching between header and source files


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


;; ISend-mode
(ff/load-file-if-exists (concat homeDir "/.etc/isend.el"))