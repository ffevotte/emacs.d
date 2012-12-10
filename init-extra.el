;; Useful helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ff/add-hooks (hooks functions)
  "Add each function in FUNCTIONS to all hooks in HOOKS.

Example:
  (ff/add-hooks (list 'myhook1 'myhook2)
                (list 'myfunction1 'myfunction2)) "
  (dolist (function functions)
    (dolist (hook hooks)
      (add-hook hook function))))



;; Extensions management
(defun ff/require-or-warn (p)
  "Require a package or warn the user and return nil."
  (not (unless (require p nil 'noerror)
         (message (format "WARNING: could not load package %s" p))
         t)))

(defun ff/fboundp (f)
  "Test whether a function is bound and warn if not."
  (if (fboundp f)
      t
    (message "WARNING: function %s unavailable" f)
    nil))

(if (not (load (expand-file-name "~/.emacs.d/elpa/package.el") 'noerror))
    (message "WARNING: could not load `package.el`")
  (add-to-list 'package-archives '("elpa"      . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/"))
  (package-initialize))

(defun ff/packages-upgrade ()
  (interactive)
  (package-list-packages)
  (dolist (p '(adaptive-wrap
               auto-complete popup
               autopair
               bookmark+
               helm
               ido-ubiquitous
               magit
               smex))
    (condition-case nil
        (package-install p)
      ((debug error) nil))))


;; This seems to be needed to avoid errors
(when (not (boundp 'warning-suppress-types))
  (setq warning-suppress-types nil))




;; Global customization
;;;;;;;;;;;;;;;;;;;;;;;

(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)
(pending-delete-mode 1) ;; Useful in combination with expand-region
(setq-default
 indicate-buffer-boundaries 'left
 indicate-empty-lines t
 indent-tabs-mode nil                         ;; Indent with spaces
 fill-column 100                              ;; Larger fill column
 )
(setq
 frame-title-format (list "%b - Emacs")       ;; Window title
 show-paren-style 'mixed                      ;; Show the whole expression if it is too large
 bookmark-default-file "~/.emacs.d/bookmarks" ;; Bookmarks file
 shift-select-mode nil                        ;; No shift selection
 mouse-yank-at-point t                        ;; Yank at point (like in a tty)
 whitespace-line-column fill-column           ;; Better whitespace-mode defaults
 whitespace-style '(face trailing lines-tail tabs)
 imenu-auto-rescan t                          ;; Imenu shoud always rescan the buffers
 diff-switches "-u"                           ;; Unified diffs
 a2ps-switches '("-l" "100")                  ;; Custom command-line args for a2ps
 custom-file "~/.emacs.d/custom.el"           ;; Separate custom file
 )

(load custom-file 'noerror)
(add-hook 'after-save-hook          ;; Automatically make shebang-ed scripts executable
          'executable-make-buffer-file-executable-if-script-p)
(defalias 'yes-or-no-p 'y-or-n-p)   ;; Don't bother typing 'yes'



;; Enable some "forbidden" commands
(put 'set-goal-column     'disabled nil) ;; (C-x C-n)
(put 'narrow-to-region    'disabled nil) ;; (C-x n n)
(put 'upcase-region       'disabled nil) ;; (C-x C-u)
(put 'erase-buffer        'disabled nil)
(put 'ido-exit-minibuffer 'disabled nil)



;; Custom global key bindings
(global-set-key (kbd "C-x C-i") 'imenu)         ;; jump to function definition
(global-set-key (kbd "C-h a")   'apropos)       ;; search everything, not just commands
(global-set-key (kbd "M-j")     'next-line)     ;; VI-like movement with M-{h,j,k,l}
(global-set-key (kbd "M-k")     'previous-line)
(global-set-key (kbd "M-h")     'backward-char)
(global-set-key (kbd "M-l")     'forward-char)
(global-set-key (kbd "C-x g")   'revert-buffer) ;; Revert buffer
(global-set-key (kbd "C-c v v") 'magit-status)  ;; Magit entry point
;; Switch windows using C-pgUp / C-pgDn
(global-set-key (kbd "C-<next>")  (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "C-<prior>") (lambda () (interactive) (other-window -1)))
;; Move between pages (separated with ^L) with M-pgUp / M-pgDn
(global-set-key (kbd "M-<next>")  (lambda () (interactive) (forward-page 1)(move-beginning-of-line 1)))
(global-set-key (kbd "M-<prior>") (lambda () (interactive) (forward-page -1)(move-beginning-of-line 1)))



;; File cache (C-x C-f C-<tab>)
(file-cache-add-directory "~/.etc")
(file-cache-add-directory "~/.emacs.d")



;; Desktop
(require 'desktop)
(setq desktop-save 'ask)
(defvar desktop-base-dir "~/.emacs.d/desktops/"
  "Base directory for desktop files")
(defun desktop-load (name)
  (interactive
   (list
    (completing-read "Desktop name: "
                     (remove "." (remove ".." (directory-files desktop-base-dir))))))
  (desktop-change-dir (concat desktop-base-dir name))
  (setq frame-title-format
        (list (concat "%b - Emacs ["
                      (file-name-nondirectory (directory-file-name desktop-dirname))
                      "]")))
  (desktop-save-mode 1))
(defun desktop-create ()
  (interactive)
  (when (null desktop-dirname)
    (let ((name (read-from-minibuffer "Desktop name: ")))
      (setq desktop-dirname (concat desktop-base-dir name))
      (make-directory desktop-dirname 'parents)))
  (desktop-save desktop-dirname)
  (desktop-save-mode 1))



;; Change behaviour of exchange-point-and-mark
(global-set-key (kbd "C-x C-x") 'ff/exchange-point-and-mark)
(defun ff/exchange-point-and-mark (&optional arg)
  "Exchange point and mark.

Inverse standard behaviour of `exchange-point-and-mark' regarding
the prefix argument: a prefix ARG activates the region."
  (interactive "P")
  (if arg
      (exchange-point-and-mark nil)
    (exchange-point-and-mark t)))



;; Find-file and switch-buffer in other window with a prefix arg
(global-set-key (kbd "C-x C-f") 'ff/find-file)
(defun ff/find-file (&optional argp)
  "Use prefix argument to select where to find a file.

Without prefix argument, visit the file in the current window.
With a universal prefix arg, display the file in another window.
With two universal arguments, visit the file in another window."
  (interactive "p")
  (cond ((eq argp 1)
         (call-interactively 'find-file))
        ((eq argp 4)
         (call-interactively 'ido-display-file))
        ((eq argp 16)
         (call-interactively 'find-file-other-window))))

(global-set-key (kbd "C-x b") 'ff/switch-to-buffer)
(defun ff/switch-to-buffer (&optional argp)
  "Use prefix argument to select where to switch to buffer.

Without prefix argument, switch the buffer in the current window.
With a universal prefix, display the buffer in another window.
With two universal arguments, switch the buffer in another window."
  (interactive "p")
  (cond ((eq argp 1)
         (call-interactively 'switch-to-buffer))
        ((eq argp 4)
         (call-interactively 'display-buffer))
        (t
         (call-interactively 'switch-to-buffer-other-window))))



;; ANSI terminal
(defun python-term ()
  "Open a python terminal."
  (interactive)
  (ansi-term "/usr/bin/python" "Python"))
(eval-after-load "term"
  '(progn
     (message "Setting up term...")
     (setq term-buffer-maximum-size 10000)
     (setq ansi-term-color-vector ;; ANSI Term colors
           [unspecified "#000000" "#b21818" "#18b218" "#BE5F00"
                        "#6D85BA" "#b218b2" "#18b2b2" "#b2b2b2"])
     (defun term-send-Cright () (interactive) (term-send-raw-string "\e[1;5C"))
     (defun term-send-Cleft  () (interactive) (term-send-raw-string "\e[1;5D"))
     (define-key term-raw-map (kbd "C-<right>") 'term-send-Cright)
     (define-key term-raw-map (kbd "C-<left>")  'term-send-Cleft)
     (message "Setting up term...done.")))



;; Org-mode
(defvar ff/use-org nil
  "Set this to non-nil to use org-mode")
(when ff/use-org
  (eval-after-load "org"
    '(load "setup-org")))



;; Abbrevs
(quietly-read-abbrev-file)
(defun ff/turn-on-abbrev ()
  "Turn on abbrev-mode"
  (abbrev-mode 1))



;; LISP programming

;; Eval and replace lisp code
(defun eval-and-replace (value)
  "Evaluate the sexp at point and replace it with its value"
  (interactive (list (eval-last-sexp nil)))
  (kill-sexp -1)
  (insert (format "%S" value)))
(global-set-key
 (kbd "C-x C-e")
 (lambda (&optional argp)
   (interactive "P")
   (if argp
       (call-interactively 'eval-and-replace)
     (call-interactively 'eval-last-sexp))))

;; Test macro expansion
(defmacro ff/test-macroexpand (form)
  "Display the given macro expansion in buffer *macroexpand*

Example usage:
  (ff/test-macroexpand '(ff/add-compilation-command \"run\" (kbd \"<f6>\")))
"
  `(progn
     (with-current-buffer (get-buffer-create "*macroexpand*")
       (setq buffer-read-only nil)
       (erase-buffer)
       (pp
        (macroexpand ,form)
        (current-buffer))
       (setq buffer-read-only t))
     (display-buffer "*macroexpand*")))




;; Recursive minibuffer
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)




;; Non standard extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Color-theme
(defun ff/set-color-theme-hook (frame)
  (select-frame frame)
  (ff/set-color-theme))
(defun ff/set-color-theme ()
  (when (window-system)
    (let ((color-theme-is-global nil))
      (when (ff/require-or-warn 'color-theme-tango)
	(color-theme-tango)))))
(add-hook 'after-make-frame-functions 'ff/set-color-theme-hook)
(ff/set-color-theme)



;; Ido-ubiquitous
(when (ff/require-or-warn 'ido-ubiquitous)
  (ido-ubiquitous-mode 1))



;; Helm (successor to anything)
(when (ff/require-or-warn 'helm-config)
  (global-set-key (kbd "C-x C-h") 'helm-mini)
  (global-set-key (kbd "C-x C-r") 'helm-recentf)
  (global-set-key (kbd "C-c M-x") 'helm-M-x))



;; Smex
(when (ff/require-or-warn 'smex)
  (smex-initialize)
  ;; Enhanced M-x
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; Traditional M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))



;; Auto-complete
(defvar ff/auto-complete-ac-dict nil
  "Path to the auto-complete dictionnary")
(when (ff/require-or-warn 'auto-complete-config)
  (when ff/auto-complete-ac-dict
    (add-to-list 'ac-dictionary-directories ff/auto-complete-ac-dict))
  (ac-config-default))



;; CEDET
(when (ff/require-or-warn 'cedet)
  (setq semanticdb-default-save-directory "~/.emacs.d/semanticdb")
  (setq semantic-idle-scheduler-idle-time 0.5)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  (when (>= (string-to-number cedet-version) 1.1)
    (require 'semantic/ia))
  (defun ff/semantic-ia-fast-jump ()
    "Push point to the tag marker ring before calling `semantic-ia-fast-jump'"
    (interactive)
    (ring-insert find-tag-marker-ring (point-marker))
    (when (and (boundp 'gtags-mode)
               (not (null gtags-mode)))
      (gtags-push-context))
    (call-interactively 'semantic-ia-fast-jump))
  (eval-after-load "semantic"
    '(progn
       (require 'etags)
       (define-key semantic-mode-map (kbd "C-c , ,") 'ff/semantic-ia-fast-jump))))

(defun ff/semantic-auto-completion ()
  "Activate semantic-ia source for auto-completion if available"
  (when (boundp 'ac-source-semantic)
    (add-to-list 'ac-sources 'ac-source-semantic)))



;; Yasnippet
(defun ff/turn-on-yasnippet ()
    "Locally turn on yasnippet minor mode"
    (when (ff/require-or-warn 'yasnippet-bundle)
      (yas/minor-mode 1)))
(when (fboundp 'yas/compile-bundle)
  (defun ff/yas-compile-bundle ()
    "Compile a bundle of yasnippets"
    (interactive)
    (let* ((yasnippet-file (locate-file "yasnippet.el" load-path))
           (root-dir       (file-name-directory yasnippet-file))
           (snippets-dir   (concat root-dir "snippets"))
           (dropdown-file  (locate-file "dropdown-list.el" load-path)))
      (yas/compile-bundle yasnippet-file
                          "yasnippet-bundle.el"
                          (list "~/.emacs.d/snippets" snippets-dir)
                          nil
                          dropdown-file))
    (load "yasnippet-bundle")))



;; Adaptive-wrap
(when (ff/fboundp 'adaptive-wrap-prefix-mode)
  (defadvice visual-line-mode (after ff/adaptive-wrap-prefix-mode activate)
    "Toggle `visual-line-mode' and `adaptive-wrap-prefix-mode' simultaneously."
    (if visual-line-mode
        (adaptive-wrap-prefix-mode 1)
      (adaptive-wrap-prefix-mode -1))))



;; Autopair
(defun ff/turn-on-autopair ()
  "Turn on autopair minor mode if available."
  (when (ff/require-or-warn 'autopair)
    (autopair-mode 1)))



;; Bookmark+
(when (ff/require-or-warn 'bookmark+)
  (ff/require-or-warn 'bookmark+-lit)
  (setq
   bmkp-auto-light-when-jump      'all-in-buffer
   bmkp-auto-light-when-set       'all-in-buffer
   bmkp-light-style-autonamed     'lfringe
   bmkp-light-style-non-autonamed 'rfringe)
  (global-set-key (kbd "C-x <kp-add>")      'bmkp-next-bookmark-this-file/buffer-repeat)
  (global-set-key (kbd "C-x <kp-subtract>") 'bmkp-previous-bookmark-this-file/buffer-repeat))



;; page-break-line
(setq page-break-lines-char ?_)



;; Expand-region
(add-to-list 'load-path "~/.emacs.d/packages/expand-region")
(when (ff/require-or-warn 'expand-region)
  (global-set-key (kbd "C-x SPC") 'er/expand-region))



;; Multiple cursors
(add-to-list 'load-path "~/.emacs.d/packages/multiple-cursors")
(when (ff/require-or-warn 'multiple-cursors)
  (defalias 'mc 'mc/edit-lines)
  (global-set-key (kbd "C-<") 'mc/mark-next-like-this)
  (global-set-key (kbd "C->") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-S-SPC") 'set-rectangular-region-anchor))



;; Automatically start server
(load "setup-server")



;; Home-made packages
(defun ff/update-autoloads ()
  (interactive)
  (message "Updating autoloads for home-made packages...")
  (let ((generated-autoload-file "~/.emacs.d/ff-autoloads.el"))
    (dolist (x (list "~/.emacs.d/org-tagreport.el"     ;; Reports by tag for org-mode
                     "~/.emacs.d/ff-misc.el"))         ;; Stack-overflow
      (update-file-autoloads x 'save-after)))
  (message "Updating autoloads for home-made packages...done"))
(unless (require 'ff-autoloads nil 'noerror)
  (ff/update-autoloads)
  (require 'ff-autoloads))




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

(defun ff/delete-trailing-whitespace (&optional argp)
  "Show trailing whitespaces"
  (interactive "P")
  (if argp
      (progn
        (setq show-trailing-whitespace nil)
        (setq write-file-functions
              (delete 'delete-trailing-whitespace write-file-functions))
        (message "Deactivating delete-trailing-whitespace"))
    (setq show-trailing-whitespace t)
    (make-local-variable 'write-file-functions)
    (add-to-list 'write-file-functions
                 'delete-trailing-whitespace)
    (message "Activating delete-trailing-whitespace")))



;; Text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)



;; LaTeX-mode
(defun ff/TeX-turn-on-abbrev ()
  (abbrev-mode 1)
  (setq local-abbrev-table TeX-mode-abbrev-table))
(add-hook 'TeX-mode-hook 'ff/TeX-turn-on-abbrev)

(eval-after-load "latex"
  '(define-key LaTeX-mode-map (kbd "~") 'ff/insert-tilde))
(defun ff/insert-tilde ()
  "Insert a tilde (~) character at point.

Potentially remove surrounding spaces around point, so that the
newly inserted character replaces them."
  (interactive)
  (skip-syntax-forward " ")
  (let ((end (point)))
    (skip-syntax-backward " ")
    (delete-region (point) end)
    (insert "~")))



;; C-like modes
(add-hook 'c-mode-common-hook 'ff/semantic-auto-completion)



;; Common features for programming modes
(ff/add-hooks (list 'c-mode-common-hook 'lisp-mode-hook 'emacs-lisp-mode-hook 'python-mode-hook
                    'sh-mode-hook 'octave-mode-hook 'LaTeX-mode-hook)
              (list 'ff/setup-todo-keywords 'ff/remap-newline-indent 'ff/turn-on-autopair
		    'ff/delete-trailing-whitespace))
(ff/add-hooks '(c-mode-common-hook LaTeX-mode-hook)
              '(ff/turn-on-yasnippet))
