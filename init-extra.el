;;;; Useful helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ff/add-hooks (hooks functions)
  "Add each function in FUNCTIONS to all hooks in HOOKS.

Example:
  (ff/add-hooks (list 'myhook1 'myhook2)
                (list 'myfunction1 'myfunction2)) "
  (dolist (function functions)
    (dolist (hook hooks)
      (add-hook hook function))))



;;; Extensions management (package.el)
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
               git-gutter
               helm
               ido-ubiquitous
               magit
               markdown-mode
               key-chord
               page-break-lines
               smex
               wgrep
               yasnippet))
    (condition-case nil
        (package-install p)
      ((debug error) nil))))


;; This seems to be needed to avoid errors
(when (not (boundp 'warning-suppress-types))
  (setq warning-suppress-types nil))




;;;; Global customization
;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)
(pending-delete-mode 1) ;; Useful in combination with expand-region
(setq-default
 indicate-buffer-boundaries 'left
 indicate-empty-lines t
 indent-tabs-mode nil                         ;; Indent with spaces
 fill-column 80                               ;; Larger fill column
 )
(setq
 frame-title-format (list "%b - Emacs")       ;; Window title
 show-paren-style 'mixed                      ;; Show the whole expression if it is too large
 bookmark-default-file "~/.emacs.d/bookmarks" ;; Bookmarks file
 shift-select-mode nil                        ;; No shift selection
 display-buffer-reuse-frames t                ;; If a buffer is displayed in another frame, raise it
 mouse-yank-at-point t                        ;; Yank at point (like in a tty)
 whitespace-line-column fill-column           ;; Better whitespace-mode defaults
 whitespace-style '(tab-mark indentation trailing lines-tail)
 imenu-auto-rescan t                          ;; Imenu shoud always rescan the buffers
 diff-switches "-u"                           ;; Unified diffs
 a2ps-switches '("-l" "100")                  ;; Custom command-line args for a2ps
 custom-file "~/.emacs.d/custom.el"           ;; Separate custom file
 )

(load custom-file 'noerror)
(add-hook 'after-save-hook          ;; Automatically make shebang-ed scripts executable
          'executable-make-buffer-file-executable-if-script-p)
(defalias 'yes-or-no-p 'y-or-n-p)   ;; Don't bother typing 'yes'



;;; Enable some "forbidden" commands
(put 'set-goal-column     'disabled nil) ;; (C-x C-n)
(put 'narrow-to-region    'disabled nil) ;; (C-x n n)
(put 'upcase-region       'disabled nil) ;; (C-x C-u)
(put 'downcase-region     'disabled nil) ;; (C-x C-l)
(put 'erase-buffer        'disabled nil)
(put 'ido-exit-minibuffer 'disabled nil)



;;; Custom global key bindings
(define-minor-mode custom-bindings-mode
  "Install custom key bindings.

\\{custom-bindings-mode-map}"
  :global t
  :init-value t
  :keymap (make-sparse-keymap))
(defun custom-set-key (key command)
  (define-key custom-bindings-mode-map key command))
(defun promote-minor-mode-map (mode)
  (if (not (eq (car (car minor-mode-map-alist)) mode))
      (let ((mykeys (assq mode minor-mode-map-alist)))
        (assq-delete-all mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))

(custom-set-key (kbd "C-h a")   'apropos)       ;; search everything, not just commands
(custom-set-key (kbd "<print>") 'a2ps-buffer)   ;; print the current buffer
(custom-set-key (kbd "C-c v")   'magit-status)  ;; entry point for Magit
(custom-set-key (kbd "C-x g")   'revert-buffer) ;; revert buffer
(custom-set-key (kbd "H-j")     'next-line)     ;; VI-like movement with M-{h,j,k,l}
(custom-set-key (kbd "H-k")     'previous-line)
(custom-set-key (kbd "H-h")     'backward-char)
(custom-set-key (kbd "H-l")     'forward-char)
(custom-set-key (kbd "H-M-j")   'forward-paragraph)
(custom-set-key (kbd "H-M-k")   'backward-paragraph)
(custom-set-key (kbd "H-M-h")   'backward-word)
(custom-set-key (kbd "H-M-l")   'forward-word)
;; Switch windows using C-pgUp / C-pgDn
(custom-set-key (kbd "C-<next>")  (defun ff/next-window () (interactive) (other-window 1)))
(custom-set-key (kbd "C-<prior>") (defun ff/prev-window () (interactive) (other-window -1)))
;; Move between pages (separated with ^L) with M-pgUp / M-pgDn
(custom-set-key (kbd "M-<next>")  (defun ff/next-page () (interactive) (forward-page 1)(move-beginning-of-line 1)))
(custom-set-key (kbd "M-<prior>") (defun ff/prev-page () (interactive) (forward-page -1)(move-beginning-of-line 1)))

;; Key chords
(defun custom-set-chord (chord command)
  "Key chord mode is not installed; do nothing"
  nil)
(when (fboundp 'key-chord-mode)
  (key-chord-mode 1)
  (defun custom-set-chord (chord command)
    (key-chord-define custom-bindings-mode-map chord command))
  (custom-set-chord "JJ" 'beginning-of-buffer)
  (custom-set-chord "KK" 'end-of-buffer)
  (custom-set-chord "QQ" 'join-line)
  (custom-set-chord "AA" (lambda () (interactive)(join-line 1))))


;; Repeatable commands
;;   (http://stackoverflow.com/a/17310748/1225607)
(require 'repeat)
(defun make-repeatable-command (cmd)
  "Returns a new command that is a repeatable version of CMD.
The new command is named CMD-repeat.  CMD should be a quoted
command.

This allows you to bind the command to a compound keystroke and
repeat it with just the final key.  For example:

  (global-set-key (kbd \"C-c a\") (make-repeatable-command 'foo))

will create a new command called foo-repeat.  Typing C-c a will
just invoke foo.  Typing C-c a a a will invoke foo three times,
and so on."
  (fset (intern (concat (symbol-name cmd) "-repeat"))
        `(lambda ,(help-function-arglist cmd) ;; arg list
           ,(format "A repeatable version of `%s'." (symbol-name cmd)) ;; doc string
           ,(interactive-form cmd) ;; interactive form
           ;; see also repeat-message-function
           (setq last-repeatable-command ',cmd)
           (repeat nil)))
  (intern (concat (symbol-name cmd) "-repeat")))

(custom-set-key (kbd "C-c q") (make-repeatable-command 'join-line))
(custom-set-key (kbd "C-x {") (make-repeatable-command 'shrink-window-horizontally))
(custom-set-key (kbd "C-x }") (make-repeatable-command 'enlarge-window-horizontally))
;; `make-repeatable-command' doesn't seem to work with C function
(custom-set-key (kbd "C-x ^") (make-repeatable-command (defun ff/enlarge-window (size)
                                                         "Lisp wrapper around `enlarge-window'"
                                                         (interactive "p")
                                                         (enlarge-window size))))




;;; File cache (C-x C-f C-<tab>)
(file-cache-add-directory "~/.etc")
(file-cache-add-directory "~/.emacs.d")



;;; CUA rectangles
(cua-selection-mode 1)



;;; Terminal
(defun get-or-create-term (argp)
  (interactive "P")
  (let* ((term-name "ansi-term")
         (full-term-name (concat "*" term-name "*"))
         (term-program "/bin/bash"))
    (if (get-buffer full-term-name)
        (switch-to-buffer full-term-name)
      (ansi-term term-program term-name))))
(custom-set-key (kbd "<f2>") 'get-or-create-term)



;;; Desktop
(setq desktop-save 'ask)
(defvar desktop-base-dir "~/.emacs.d/desktops/"
  "Base directory for desktop files")

(defun desktop--set-frame-title ()
  (setq frame-title-format
        (list (concat "%b - Emacs ["
                      (file-name-nondirectory (directory-file-name desktop-dirname))
                      "]"))))

(defun desktop-load (name)
  (interactive
   (list
    (completing-read "Desktop name: "
                     (remove "." (remove ".." (directory-files desktop-base-dir))))))
  (desktop-change-dir (concat desktop-base-dir name))
  (desktop--set-frame-title)
  (desktop-save-mode 1))

(defun desktop-create ()
  (interactive)
  (when (or (not (boundp 'desktop-dirname))
            (null desktop-dirname))
    (let ((name (read-from-minibuffer "Desktop name: ")))
      (setq desktop-dirname (concat desktop-base-dir name))
      (make-directory desktop-dirname 'parents)))
  (desktop-save desktop-dirname)
  (desktop--set-frame-title)
  (desktop-save-mode 1))



;;; Undo in regions

(custom-set-key (kbd "C-_") 'ff/undo)
(defun ff/undo (&optional arg)
  "Call undo, improving the use of regions."
  (interactive "p")
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point)))
            (deactivate-mark nil))
        (undo arg)
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    (undo arg)))



;;; Miscellaneous commands

(defun unfill-paragraph ()
  "Unfill the paragraph at point.

This repeatedly calls `join-line' until the whole paragraph does
not contain hard line breaks any more."
  (interactive)
  (forward-paragraph 1)
  (forward-paragraph -1)
  (while (looking-at paragraph-start)
    (forward-line 1))
  (let ((beg (point)))
    (forward-paragraph 1)
    (backward-char 1)
    (while (> (point) beg)
      (join-line)
      (beginning-of-line))))

(defun rotate-windows (count)
  "Rotate your windows"
  (interactive "p")
  (let* ((non-dedicated-windows (remove-if 'window-dedicated-p (window-list)))
        (num-windows (length non-dedicated-windows))
        (i 0)
        (step (+ num-windows count)))
    (cond ((not (> num-windows 1))
           (message "You can't rotate a single window!"))
          (t
           (dotimes (counter (- num-windows 1))
             (let* ((next-i (% (+ step i) num-windows))
                    (w1 (elt non-dedicated-windows i))
                    (w2 (elt non-dedicated-windows next-i))

                    (b1 (window-buffer w1))
                    (b2 (window-buffer w2))

                    (s1 (window-start w1))
                    (s2 (window-start w2)))
               (set-window-buffer w1 b2)
               (set-window-buffer w2 b1)
               (set-window-start w1 s2)
               (set-window-start w2 s1)
               (setq i next-i)))))))
(defun rotate-windows-backwards (count)
  (interactive "p")
  (rotate-windows (- count)))

(custom-set-key (kbd "H-<right>") 'rotate-windows-backwards)
(custom-set-key (kbd "H-<left>")  'rotate-windows)

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))
(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
(custom-set-key (kbd "C-S-<up>")   'move-line-up)
(custom-set-key (kbd "C-S-<down>") 'move-line-down)

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(defun ff/insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.

  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.

  The default with no prefix is to use the relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'."
  (interactive "fInsert file name: \nP")
  (cond ((eq nil args)
         (insert (file-relative-name filename)))
        (t
         (insert filename))))
(custom-set-key (kbd "C-c f") 'ff/insert-file-name)

(defun ff/copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))
(custom-set-key (kbd "C-c d") 'ff/copy-file-name)



;;; Find-file and switch-buffer in other window with a prefix arg
(custom-set-key (kbd "C-x C-f") 'ff/find-file)
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

(custom-set-key (kbd "C-x b") 'ff/switch-to-buffer)
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



;;; Org-mode
(defvar ff/use-org nil
  "Set this to non-nil to use org-mode")
(when ff/use-org
  (eval-after-load "org"
    '(load "setup-org")))



;;; Abbrevs
(quietly-read-abbrev-file)
(defun ff/turn-on-abbrev ()
  "Turn on abbrev-mode"
  (abbrev-mode 1))



;;; Compilation
(ff/add-compilation-command "compile5" (kbd "<f5>"))
(ff/add-compilation-command "compile6" (kbd "<f6>"))
(ff/add-compilation-command "compile7" (kbd "<f7>"))
(ff/add-compilation-command "compile8" (kbd "<f8>"))
(global-set-key (kbd "C-x `") (make-repeatable-command 'next-error))
(global-set-key (kbd "C-x è") 'next-error-repeat)



;;; LISP programming

;; Eval and replace lisp code
(defun eval-and-replace (value)
  "Evaluate the sexp at point and replace it with its value"
  (interactive (list (eval-last-sexp nil)))
  (kill-sexp -1)
  (insert (format "%S" value)))
(custom-set-key
 (kbd "C-x C-e")
 (defun ff/eval-last-sexp (&optional argp)
   "Evaluate sexp before point.
With a prefix argument, replace the sexp by its evaluation."
   (interactive "P")
   (if argp
       (call-interactively 'eval-and-replace)
     (call-interactively 'eval-last-sexp))))
(custom-set-key (kbd "C-<f2>") 'eval-region)

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

(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression
               '("Sections" "^;;; \\(.+\\)$" 1) t)
  (add-to-list 'imenu-generic-expression
               '("SubSections" "^;;;; \\(.+\\)$" 1) t))
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)



;;; Recursive minibuffer
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)




;;;; Non standard extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Color-theme
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



;;; Ido-ubiquitous
(when (ff/require-or-warn 'ido-ubiquitous)
  (ido-ubiquitous-mode 1))



;;; Helm (successor to anything)
(when (ff/require-or-warn 'helm-config)
  (custom-set-key (kbd "C-x C-h") 'helm-mini)
  (custom-set-key (kbd "C-x C-r") 'helm-recentf)
  (custom-set-key (kbd "C-c M-x") 'helm-M-x)
  (custom-set-key (kbd "C-x C-i") 'helm-imenu))



;;; Smex
(when (ff/require-or-warn 'smex)
  (smex-initialize)
  ;; Enhanced M-x
  (custom-set-key (kbd "M-x") 'smex)
  (custom-set-key (kbd "M-X") 'smex-major-mode-commands))



;;; Auto-complete
(defvar ff/auto-complete-ac-dict nil
  "Path to the auto-complete dictionnary")
(when (ff/require-or-warn 'auto-complete-config)
  (when ff/auto-complete-ac-dict
    (add-to-list 'ac-dictionary-directories ff/auto-complete-ac-dict))
  (ac-config-default))



;;; CEDET
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



;;; Yasnippet
(eval-after-load "yasnippet"
  '(progn
     (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
     (yas-reload-all)))

(defun ff/turn-on-yasnippet ()
    "Locally turn on yasnippet minor mode"
    (when (ff/require-or-warn 'yasnippet)
      (yas-minor-mode 1)))



;;; Visual-line-mode and line wrapping
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(when (ff/fboundp 'adaptive-wrap-prefix-mode)
  (defun ff/activate-adaptive-wrap-prefix-mode ()
    "Toggle `visual-line-mode' and `adaptive-wrap-prefix-mode' simultaneously."
    (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))
  (add-hook 'visual-line-mode-hook 'ff/activate-adaptive-wrap-prefix-mode))

(defun ff/no-auto-fill ()
  "Disable `auto-fill-mode' when `visual-line-mode' is active"
  (if visual-line-mode
      (auto-fill-mode -1)))
(add-hook 'visual-line-mode-hook 'ff/no-auto-fill)



;;; Autopair
(defun ff/turn-on-autopair ()
  "Turn on autopair minor mode if available."
  (when (ff/require-or-warn 'autopair)
    (autopair-mode 1)))



;;; Bookmark+
(autoload 'bookmark-bmenu-list "bookmark+")
(autoload 'bookmark-jump       "bookmark+")
(autoload 'bookmark-set        "bookmark+")
(eval-after-load "bookmark+"
  '(progn
     (ff/require-or-warn 'bookmark+-lit)
     (setq
      bmkp-auto-light-when-jump      'all-in-buffer
      bmkp-auto-light-when-set       'all-in-buffer
      bmkp-light-style-autonamed     'lfringe
      bmkp-light-style-non-autonamed 'rfringe)
     (custom-set-key (kbd "C-x <kp-add>")      'bmkp-next-bookmark-this-file/buffer-repeat)
     (custom-set-key (kbd "C-x <kp-subtract>") 'bmkp-previous-bookmark-this-file/buffer-repeat)))



;;; page-break-line
(setq page-break-lines-char ?_)



;;; Expand-region
(add-to-list 'load-path "~/.emacs.d/packages/expand-region")
(when (ff/require-or-warn 'expand-region)
  (custom-set-key (kbd "C-x SPC") 'er/expand-region))



;;; Multiple cursors
(add-to-list 'load-path "~/.emacs.d/packages/multiple-cursors")
(when (ff/require-or-warn 'multiple-cursors)
  (defalias 'mc 'mc/edit-lines)
  (custom-set-key (kbd "H-<") 'mc/mark-next-like-this)
  (custom-set-key (kbd "H->") 'mc/mark-previous-like-this)
  (custom-set-key (kbd "C-c H-<") 'mc/mark-all-like-this)
  (custom-set-key (kbd "H-SPC") 'set-rectangular-region-anchor))



;;; Multi-term
(when (ff/require-or-warn 'multi-term)
  (custom-set-key
   (kbd "<f2>")
   (defun ff/multi-term (argp)
     (interactive "P")
     (prin1 argp)
     (cond ((and (listp argp)
                 (eq (car argp) 4))
            (let ((current-prefix-arg nil))
              (multi-term)))
           ((and (listp argp)
                 (eq (car argp) 16))
            (let ((current-prefix-arg '(4)))
              (multi-term)))
           (t
            (call-interactively 'multi-term-next)))))

  (setq multi-term-dedicated-select-after-open-p t)
  (custom-set-key (kbd "C-<f2>") 'multi-term-dedicated-toggle))



;;; Automatically start server
(load "setup-server")



;;; Home-made packages
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



;;; Manage trailing whitespace

(define-minor-mode auto-dtw-mode
  "Automatically delete trailing whitespace."
  :lighter    " dtw"
  :init-value nil
  (setq show-trailing-whitespace auto-dtw-mode))

(add-to-list 'write-file-functions 'ff/auto-dtw)
(defun ff/auto-dtw ()
  "Delete trailing whitespace, except on the current line if point is at EOL."
  (when auto-dtw-mode
    (let ((ws  (save-excursion
                 (if (and (eolp)
                          (looking-back "[[:space:]]"))
                     (let ((end (point))
                           (bol (- (line-beginning-position) 1)))
                       (search-backward-regexp "[^[:space:]]" nil t)
                       (when (< (point) bol)
                         (goto-char bol))
                       (buffer-substring (1+ (point)) end))
                   ""))))
      (delete-trailing-whitespace)
      (insert ws))))



;;;; Mode-specific customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Common helper function to be used for hooks
(defun ff/setup-todo-keywords ()
  "Highlight keywords like FIXME or TODO"
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|FIX\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(defun ff/remap-newline-indent ()
  "Remap <RET> to `newline-and-indent'."
  (local-set-key (kbd "RET") 'newline-and-indent))



;;; Text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)



;;; LaTeX-mode
(defun ff/TeX-turn-on-abbrev ()
  (abbrev-mode 1)
  (setq local-abbrev-table TeX-mode-abbrev-table))
(add-hook 'TeX-mode-hook 'ff/TeX-turn-on-abbrev)

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

(defun ff/tex-replace-macro (macro body)
  (interactive
   (save-excursion
     (let (m b)
       (beginning-of-line)
       (search-forward "{")
       (let ((beg (point)))
         (backward-char)
         (forward-sexp)
         (setq m (buffer-substring beg (- (point) 1))))
       (search-forward "{")
       (let ((beg (point)))
         (backward-char)
         (forward-sexp)
         (setq b (buffer-substring beg (- (point) 1))))
       (list m b))))
  (save-excursion
    (query-replace (concat body " ") (concat macro "\\ ")))
  (save-excursion
    (query-replace body macro)))

(defun ff/align-latex-table ()
  "Align columns in a latex tabular environment."
  (interactive)
  (save-excursion
    (search-backward "\\begin{tabular}")
    (forward-line 1)
    (let ((beg (point)))
      (search-forward "\\end{tabular}")
      (forward-line -1)
      (align-regexp beg (point) "\\(\\s-*\\)&" 1 1 t))))

(eval-after-load "latex"
  '(progn
     (define-key LaTeX-mode-map (kbd "~")     'ff/insert-tilde)
     (define-key LaTeX-mode-map (kbd "C-c a") 'ff/align-latex-table)))



;;; C-like modes
(eval-after-load "cc-mode"
  '(progn
     (c-add-style "my-cc-style"
                  '("gnu"
                    (c-offsets-alist . ((innamespace . [0])))))
     (setq c-default-style '((java-mode . "java")
                             (awk-mode  . "awk")
                             (c++-mode  . "my-cc-style")))))
(add-hook 'c-mode-common-hook 'ff/semantic-auto-completion)



;;; Common features for programming modes
(ff/add-hooks (list 'c-mode-common-hook 'lisp-mode-hook 'emacs-lisp-mode-hook 'python-mode-hook
                    'sh-mode-hook 'octave-mode-hook 'LaTeX-mode-hook)
              (list 'ff/setup-todo-keywords 'ff/remap-newline-indent 'ff/turn-on-autopair
		    'auto-dtw-mode))
(ff/add-hooks '(c-mode-common-hook LaTeX-mode-hook)
              '(ff/turn-on-yasnippet))



;;; Markdown-mode
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))



(promote-minor-mode-map 'custom-bindings-mode)
