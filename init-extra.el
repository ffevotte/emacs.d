;; Useful helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(defun ff/require-or-warn (p)
  "require a package or warn the user and return nil"
  (not (unless (require p nil 'noerror)
         (message (format "Could not load package %s" p))
         t)))
(if (not (load (expand-file-name "~/.emacs.d/elpa/package.el") 'noerror))
    (message "WARNING: could not load `package.el`")
  (add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (package-initialize))


;; This seems to be needed to avoid errors
(when (not (boundp 'warning-suppress-types))
  (setq warning-suppress-types nil))




;; Global customization
;;;;;;;;;;;;;;;;;;;;;;;

(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)
(custom-set-variables
 '(inhibit-splash-screen t)
 '(indicate-empty-lines  t)
 '(indicate-buffer-boundaries 'left)
 '(show-paren-style 'mixed)
 '(bookmark-default-file "~/.emacs.d/bookmarks") ;; Bookmarks file
 '(shift-select-mode nil)                        ;; No shift selection
 '(mouse-yank-at-point t)                        ;; Yank at point (like in a tty)
 '(indent-tabs-mode nil)                         ;; Indent with spaces
 '(fill-column 100)                              ;; Larger fill column
 '(whitespace-line-column fill-column)           ;; Better whitespace-mode defaults
 '(whitespace-style '(face trailing lines-tail tabs))
 '(imenu-auto-rescan t)                          ;; Imenu shoud always rescan the buffers
 '(diff-switches "-u")                           ;; Unified diffs
 '(custom-file "~/.emacs.d/custom.el")           ;; Separate custom file
)

(load custom-file 'noerror)
(setq-default frame-title-format    ;; Window title
              (list "%b Emacs"))
(windmove-default-keybindings)      ;; use S-<arrows> to switch between windows
(add-hook 'after-save-hook          ;; Automatically make shebang-ed scripts executable
          'executable-make-buffer-file-executable-if-script-p)
(defalias 'yes-or-no-p 'y-or-n-p)   ;; Don't bother typing 'yes'


;; Enable some "forbidden" commands
(put 'set-goal-column     'disabled nil) ;; (C-x C-n)
(put 'narrow-to-region    'disabled nil) ;; (C-x n n)
(put 'erase-buffer        'disabled nil)
(put 'ido-exit-minibuffer 'disabled nil)


;; Custom global key bindings
(global-set-key (kbd "C-x C-i") 'imenu)         ;; jump to function definition
(global-set-key (kbd "C-h a")   'apropos)       ;; search everything, not just commands
(global-set-key (kbd "M-j")     'next-line)     ;; VI-like movement with M-{h,j,k,l}
(global-set-key (kbd "M-k")     'previous-line)
(global-set-key (kbd "M-h")     'backward-char)
(global-set-key (kbd "M-l")     'forward-char)


;; File cache (C-x C-f C-<tab>)
(file-cache-add-directory "~/.etc")
(file-cache-add-directory "~/.emacs.d")


;; No semantic.cache cluttering
(let ((mySemanticRep (concat "/tmp/semantic.cache-" (getenv "USER"))))
  (if (not (file-accessible-directory-p mySemanticRep))
      (make-directory mySemanticRep))
  (custom-set-variables
   '(semanticdb-default-save-directory mySemanticRep 'now)))


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


;; Org-mode
(defvar ff/use-org nil
  "Set this to non-nil to use org-mode")
(when ff/use-org
  (when (ff/require-or-warn 'org)
    (load "setup-org"))
  (when (ff/require-or-warn 'org-shortcuts) ; Org-clock-in shortcuts
    (add-hook 'org-clock-before-select-task-hook 'org-clock-insert-shortcuts)))




;; Non standard extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CEDET
(load "setup-cedet")


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
  (ido-ubiquitous-mode t))


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


;; Yasnippet
(defun ff/turn-on-yasnippet ()
  "Yasnippet is not installed")
(when (ff/require-or-warn 'yasnippet-bundle)
  (defun ff/turn-on-yasnippet ()
    "Locally turn on yasnippet minor mode"
    (yas/minor-mode 1)))
(when (ff/require-or-warn 'yasnippet)
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
                          dropdown-file)))
  (load "yasnippet-bundle"))


;; Autopair
(defun ff/turn-on-autopair ()
  "Don't do anything when autopair is not installed")
(when (ff/require-or-warn 'autopair)
  (defun ff/turn-on-autopair ()
    "Turn on autopair minor mode"
    (autopair-mode 1)))


;; Home-made packages
(defun ff/update-autoloads ()
  (interactive)
  (message "Updating autoloads for home-made packages...")
  (let ((generated-autoload-file "~/.emacs.d/ff-autoloads.el"))
    (mapcar
     (lambda (x) (update-file-autoloads x 'save-after))
     (list "isend.el"             ;; ISend-mode (associate buffer to a terminal)
           "org-tagreport.el"     ;; Reports by tag for org-mode
           "ff-misc.el")))        ;; stack-overflow
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


;; Text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)


;; C-like modes
(add-hook 'c-mode-common-hook 'ff/semantic-auto-completion)


;; Common features for programming modes
(ff/add-hooks (list 'c-mode-common-hook 'lisp-mode-hook 'emacs-lisp-mode-hook 'python-mode-hook
                    'sh-mode-hook 'octave-mode-hook 'LaTeX-mode-hook)
              (list 'ff/setup-todo-keywords 'ff/remap-newline-indent 'ff/turn-on-autopair))
(ff/add-hooks '(c-mode-common-hook LaTeX-mode-hook)
              '(ff/turn-on-yasnippet))
