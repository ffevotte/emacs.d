;; * Utilities

;; ** Path to subcomponents

(defun ff/variable-file (name)
  "Path to a variable file of given NAME
Variable files are located in the \"var\" subdirectory of `user-emacs-directory'"
  (expand-file-name (concat user-emacs-directory "var/" name)))

(defun ff/emacsd (name)
  "Path to a subdirectory of `user-emacs-directory'"
  (expand-file-name (concat user-emacs-directory name)))


;; ** Lisp utilities
(use-package cl-lib
  :load-path "share/elisp/cl-lib"
  :defer t)

(defun assq-replace (key value alist)
  (set alist
       (cons (cons key value)
             (assq-delete-all key (symbol-value alist)))))


;; ** Custom global key bindings
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


;; ** Repeatable commands
;; (http://stackoverflow.com/a/17310748/1225607)
(use-package repeat
  :init (progn
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
          ;; `make-repeatable-command' doesn't seem to work with C functions
          (custom-set-key (kbd "C-x ^") (make-repeatable-command (defun ff/enlarge-window (size)
                                                                   "Lisp wrapper around `enlarge-window'"
                                                                   (interactive "p")
                                                                   (enlarge-window size))))))


;; ** Hooks
(defun ff/add-hooks (hooks functions)
  "Add each function in FUNCTIONS to all hooks in HOOKS.

Example:
  (ff/add-hooks (list 'myhook1 'myhook2)
                (list 'myfunction1 'myfunction2)) "
  (dolist (function functions)
    (dolist (hook hooks)
      (add-hook hook function))))


;; ** Hacks
(when (not (boundp 'warning-suppress-types))
  (setq warning-suppress-types nil))



;; * User Interface

;; ** Theme

(when (window-system)
  (use-package color-theme-tango
    :load-path "color-theme"
    :config (color-theme-tango))

  (assq-replace 'font-backend "xft"                         'default-frame-alist)
  (assq-replace 'font         "Bitstream Vera Sans Mono-9"  'default-frame-alist))


;; ** Global settings
(setq-default
 indicate-buffer-boundaries 'left
 indicate-empty-lines t
 indent-tabs-mode nil                         ;; Indent with spaces
 fill-column 80                               ;; Larger fill column
 )
(setq
 frame-title-format (list "%b - Emacs")       ;; Window title
 show-paren-style 'mixed                      ;; Show the whole expression if it is too large
 shift-select-mode nil                        ;; No shift selection
 display-buffer-reuse-frames t                ;; If a buffer is displayed in another frame, raise it
 mouse-yank-at-point t                        ;; Yank at point (like in a tty)
 whitespace-line-column fill-column           ;; Better whitespace-mode defaults
 whitespace-style '(tab-mark indentation trailing lines-tail)
 diff-switches "-u"                           ;; Unified diffs
 )
(add-hook 'after-save-hook          ;; Automatically make shebang-ed scripts executable
          'executable-make-buffer-file-executable-if-script-p)
(defalias 'yes-or-no-p 'y-or-n-p)   ;; Don't bother typing 'yes'
(pending-delete-mode 1) ;; Useful in combination with expand-region

;; Separate custom file
(setq custom-file (ff/variable-file "custom.el"))
(load custom-file 'noerror)

;; Variable files
(setq url-configuration-directory (ff/variable-file "url/"))
(setq auto-save-list-file-prefix  (ff/variable-file "auto-save-list/"))


;; ** Enable "forbidden" commands
(put 'set-goal-column     'disabled nil) ;; (C-x C-n)
(put 'narrow-to-region    'disabled nil) ;; (C-x n n)
(put 'upcase-region       'disabled nil) ;; (C-x C-u)
(put 'downcase-region     'disabled nil) ;; (C-x C-l)
(put 'erase-buffer        'disabled nil)
(put 'ido-exit-minibuffer 'disabled nil)


;; ** Global key bindings
(custom-set-key (kbd "C-h a")      'apropos)       ;; search everything, not just commands
(custom-set-key (kbd "<print>")    'a2ps-buffer)   ;; print the current buffer
(custom-set-key (kbd "C-x g")      'revert-buffer) ;; revert buffer
;; VI-like movement with H-{h,j,k,l}
(custom-set-key (kbd "H-j")        'next-line)
(custom-set-key (kbd "H-k")        'previous-line)
(custom-set-key (kbd "H-h")        'backward-char)
(custom-set-key (kbd "H-l")        'forward-char)
(custom-set-key (kbd "H-M-j")      'forward-paragraph)
(custom-set-key (kbd "H-M-k")      'backward-paragraph)
(custom-set-key (kbd "H-M-h")      'backward-word)
(custom-set-key (kbd "H-M-l")      'forward-word)
;; Switch windows using C-pgUp / C-pgDn
(custom-set-key (kbd "C-<next>")   (defun ff/next-window () (interactive) (other-window 1)))
(custom-set-key (kbd "C-<prior>")  (defun ff/prev-window () (interactive) (other-window -1)))

;; *** Move between pages or section headers with M-pgUp / M-pgDn
(defun ff/move-by-heading-or-page (move-heading move-page choose default-pos)
  (goto-char (funcall choose
                      (if (and (boundp 'outline-minor-mode)
                               outline-minor-mode)
                          (save-excursion
                            (funcall move-heading)
                            (point))
                        (funcall default-pos))
                      (save-excursion
                        (funcall move-page 1)
                        (move-beginning-of-line 1)
                        (point)))))

(custom-set-key (kbd "M-<next>")   (defun ff/next-page ()
                                     (interactive)
                                     (ff/move-by-heading-or-page 'outline-next-heading
                                                                 'forward-page
                                                                 'min 'point-max)))
(custom-set-key (kbd "M-<prior>")  (defun ff/prev-page ()
                                     (interactive)
                                     (ff/move-by-heading-or-page 'outline-previous-heading
                                                                 'backward-page
                                                                 'max 'point-min)))


;; ** Recursive minibuffer
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)


;; ** TRAMP

(setq tramp-persistency-file-name (ff/variable-file "tramp"))


;; ** Find-file and switch-buffer in other window with a prefix arg
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


;; ** Save & restore sessions

(use-package revive
  :load-path "share/elisp/revive"
  :defer     t)

(use-package desktop+
  :load-path "share/elisp/desktop+"
  :commands  (desktop-create desktop-load)
  :config  (progn
             (setq desktop-base-dir (ff/variable-file "desktops/"))
             (setq desktop-save 'ask)
             (use-package "setup-desktop")))


;; ** smex
(use-package smex
  :load-path "share/elisp/smex"
  :commands  smex
  :init      (custom-set-key (kbd "M-x") 'smex)
  :config    (smex-initialize))


;; ** ido-ubiquitous
(use-package ido-ubiquitous
  :load-path "share/elisp/ido-ubiquitous"
  :config    (ido-ubiquitous-mode 1))


;; ** helm
(use-package helm-config
  :load-path "share/elisp/helm"
  :commands  (helm-mini helm-recentf helm-M-x helm-imenu)
  :init    (progn
             (custom-set-key (kbd "C-x C-h") 'helm-mini)
             (custom-set-key (kbd "C-x C-r") 'helm-recentf)
             (custom-set-key (kbd "C-x M-x") 'helm-M-x)
             (custom-set-key (kbd "C-x C-i") 'helm-imenu)))


;; ** bookmarks

(use-package bookmark+
  :load-path "share/elisp/bookmark+"
  :commands  (bookmark-bmenu-list bookmark-jump bookmark-set)
  :init    (progn
             (setq bookmark-default-file (ff/variable-file "bookmarks.el"))
             (setq bmkp-last-as-first-bookmark-file nil))
  :config  (progn
             (require 'bookmark+-lit)
             (setq
              bmkp-auto-light-when-jump      'all-in-buffer
              bmkp-auto-light-when-set       'all-in-buffer
              bmkp-light-style-autonamed     'lfringe
              bmkp-light-style-non-autonamed 'rfringe)
             (custom-set-key (kbd "C-x <kp-add>")
                             'bmkp-next-bookmark-this-file/buffer-repeat)
             (custom-set-key (kbd "C-x <kp-subtract>")
                             'bmkp-previous-bookmark-this-file/buffer-repeat)))



;; * Text editing


;; ** Manipulate file names
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


;; ** Undo in regions
(custom-set-key
 (kbd "C-_")
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
     (undo arg))))


;; ** Move to BOL or indentation
(defun ff/move-beginning-of-line (arg)
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
;; remap C-a to `ff/move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'ff/move-beginning-of-line)


;; ** abbrev
(use-package abbrev
  :config (quietly-read-abbrev-file))
(defun ff/turn-on-abbrev () (abbrev-mode 1))


;; ** outline with outshine
(use-package outline
  :commands outline-minor-mode
  :diminish outline-minor-mode
  :config   (use-package outshine
              :load-path "share/elisp/outshine"
              :config
	      (progn
		;; Always use modern style for Emacs Lisp
		(defun outshine-modern-header-style-in-elisp-p () t)

		;; Activate outshine
		(add-hook 'outline-minor-mode-hook
			  'outshine-hook-function)

		;; Use S-<tab> to cycle buffer visibility
		(define-key outline-minor-mode-map
		  (kbd "<backtab>") 'outshine-cycle-buffer)
		(define-key outline-mode-map
		  (kbd "<backtab>") 'outshine-cycle-buffer)

                ;; Unwanted key bindings
                (mapc (lambda (key)
                        (define-key outline-minor-mode-map key nil))
                      (list (kbd "L")
                            (kbd "J")
                            (kbd "M-<down>")
                            (kbd "M-<up>"))))))


;; ** auto-complete
(use-package popup
  :defer     t
  :load-path "share/elisp/popup")
(use-package auto-complete-config
  :load-path "share/elisp/auto-complete"
  :commands  ac-config-default
  :init    (progn
             (setq ac-comphist-file (ff/variable-file "ac-comphist.dat"))
             (ac-config-default)))


;; ** YAsnippet
(use-package yasnippet
  :load-path "share/elisp/yasnippet"
  :commands  (yas-reload-all yas-minor-mode-on yas-recompile-all)
  :config  (progn
             (setq yas-snippet-dirs `(,(ff/emacsd "snippets")))
             (yas-reload-all)))


;; ** Visual lines and adaptive wrapping
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(use-package adaptive-wrap
  :load-path "share/elisp/adaptive-wrap"
  :config (progn
            (defun ff/activate-adaptive-wrap-prefix-mode ()
              "Toggle `visual-line-mode' and `adaptive-wrap-prefix-mode' simultaneously."
              (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))
            (add-hook 'visual-line-mode-hook 'ff/activate-adaptive-wrap-prefix-mode)))

(defun ff/no-auto-fill ()
  "Disable `auto-fill-mode' when `visual-line-mode' is active"
  (if visual-line-mode
      (auto-fill-mode -1)))
(add-hook 'visual-line-mode-hook 'ff/no-auto-fill)


;; ** Expand region
(use-package expand-region
  :load-path "share/elisp/expand-region"
  :commands  er/expand-region
  :init      (custom-set-key (kbd "C-x SPC") 'er/expand-region))


;; ** Multiple cursors
(use-package multiple-cursors
  :load-path "share/elisp/multiple-cursors"
  :commands (mc/mark-next-like-this
             mc/mark-previous-like-this
             mc/mark-all-like-this
             set-rectangular-region-anchor)
  :init    (progn
             (setq mc/list-file (ff/variable-file "mc-lists.el"))

             (defalias 'mc 'mc/edit-lines)
             (custom-set-key (kbd "H-<")     'mc/mark-next-like-this)
             (custom-set-key (kbd "H->")     'mc/mark-previous-like-this)
             (custom-set-key (kbd "C-c H-<") 'mc/mark-all-like-this)
             (custom-set-key (kbd "H-SPC")   'set-rectangular-region-anchor)))


;; ** Autopair
(use-package autopair
  :load-path "share/elisp/autopair"
  :commands  autopair-on
  :init      (defun ff/autopair-on () t)
  :config    (defun ff/autopair-on () (autopair-on)))


;; ** Trailing whitespace
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



;; * Interaction with external tools


;; ** Server
(use-package setup-server
  :init (ff/server-start))


;; ** Editable grep
(use-package wgrep
  :load-path "share/elisp/wgrep"
  :commands  wgrep-change-to-wgrep-mode)

(use-package grep
  :defer  t
  :config (define-key grep-mode-map
            (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))


;; ** Git

(use-package git-commit-mode
  :defer     t
  :load-path "share/elisp/git-modes")

(use-package git-rebase-mode
  :defer     t
  :load-path "share/elisp/git-modes")

(use-package magit
  :load-path "share/elisp/magit"
  :commands  magit-status
  :init      (custom-set-key (kbd "C-c v") 'magit-status)
  :config    (when (not (fboundp 'process-live-p))
               ;; This hack is needed for older emacs versions
               (defun process-live-p (process)
                 nil)))


;; ** Multi-term
(defun ff/multi-term (argp)
  "Open a new terminal or cycle among existing ones.

No prefix arg: cycle among terminals (open one if none exist)
C-u:           create new terminal
C-u C-u:       create new terminal and choose program"
  (interactive "P")
  (prin1 argp)
  (cond ((equal argp '(4))
         (let ((current-prefix-arg nil))
           (multi-term)))
        ((equal argp '(16))
         (let ((current-prefix-arg '(4)))
           (multi-term)))
        (t
         (call-interactively 'multi-term-next))))

(use-package multi-term
  :load-path "share/elisp/multi-term"
  :commands  (multi-term
              multi-term-next
              multi-term-dedicated-toggle)
  :init     (progn
              (custom-set-key (kbd "<f2>")   'ff/multi-term)
              (custom-set-key (kbd "C-<f2>") 'multi-term-dedicated-toggle))
  :config   (progn
              (setq multi-term-dedicated-select-after-open-p t)

              (defun ff/multi-term-bind (key fun)
                (setq term-bind-key-alist
                      (delq (assoc key term-bind-key-alist)
                            term-bind-key-alist))
                (add-to-list 'term-bind-key-alist (cons key fun)))

              (defun ff/term-char-mode (argp)
                (interactive "P")
                (when (and argp
                           (use-region-p))
                  (kill-ring-save (min (point) (mark))
                                  (max (point) (mark)))
                  (goto-char (point-max))
                  (yank))
                (call-interactively 'term-char-mode)
                (term-send-left))

              (defmacro ff/multi-term-raw (key)
                `(ff/multi-term-bind
                  ,key
                  (lambda ()
                    ,(format "Send raw %s" key)
                    (interactive)
                    (term-send-raw-string (kbd ,key)))))

              (ff/multi-term-bind "C-c C-j" 'term-line-mode)
              (ff/multi-term-bind "C-c C-k" 'ff/term-char-mode)
              (ff/multi-term-bind "C-c C-u" 'universal-argument)
              (ff/multi-term-bind "C-r"     'term-send-reverse-search-history)
              (ff/multi-term-raw  "C-z")
              (ff/multi-term-raw  "C-u")
              (ff/multi-term-raw  "C-k")
              (ff/multi-term-raw  "C-y")
              (ff/multi-term-raw  "C-x ~")

              (define-key term-mode-map (kbd "C-c C-k") 'ff/term-char-mode)))


;; ** Source environment
(defun ff/source (filename)
  "Update environment variables from a shell source file."
  (interactive "fSource file: ")

  (message "Sourcing environment from `%s'..." filename)
  (with-temp-buffer

    (shell-command (format "diff -u <(true; export) <(source %s; export)" filename) '(4))

    (let ((envvar-re "declare -x \\([^=]+\\)=\\(.*\\)$"))
      ;; Remove environment variables
      (while (search-forward-regexp (concat "^-" envvar-re) nil t)
        (let ((var (match-string 1)))
          (message "%s" (prin1-to-string `(setenv ,var nil)))
          (setenv var nil)))

      ;; Update environment variables
      (goto-char (point-min))
      (while (search-forward-regexp (concat "^+" envvar-re) nil t)
        (let ((var (match-string 1))
              (value (read (match-string 2))))
          (message "%s" (prin1-to-string `(setenv ,var ,value)))
          (setenv var value)))))
  (message "Sourcing environment from `%s'... done." filename))


;; ** Print with a2ps
(use-package a2ps-multibyte
  :load-path "share/elisp/a2ps-multibyte"
  :commands  (a2ps-buffer a2ps-region)
  :config  (progn
             (setq a2ps-command  (ff/emacsd "bin/a2ps")
                   a2ps-switches '("-l" "100"))

             (add-hook 'a2ps-filter-functions
                       (defun ff/a2ps-insert-page-breaks ()
                         (ff/insert-page-breaks 76 5)))

             (defun ff/insert-page-breaks (page-size page-offset-max)
               (let ((try-move (lambda (f)
                                 (let ((orig-pos  (point))
                                       (orig-line (line-number-at-pos)))
                                   (funcall f)
                                   (unless (or (and (looking-at "\^L")
                                                    (> page-size
                                                       (- orig-line (line-number-at-pos))))
                                               (> page-offset-max
                                                  (- orig-line (line-number-at-pos))))
                                     (goto-char orig-pos))))))
                 (goto-char (point-min))
                 (while (= 0 (forward-line page-size))
                   (funcall try-move 'ff/prev-page)
                   (funcall try-move 'backward-paragraph)
                   (unless (looking-at "\^L")
                     (insert "\^L"))
                   (unless (eolp)
                     (insert "\n")))))))



;; * Authoring

;; ** Text
(add-hook 'text-mode-hook 'turn-on-auto-fill)


;; ** LaTeX
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

(use-package latex
  :defer  t
  :config (progn
            (define-key LaTeX-mode-map (kbd "~")     'ff/insert-tilde)
            (define-key LaTeX-mode-map (kbd "C-c a") 'ff/align-latex-table)))

;; ** Markdown
(use-package markdown-mode
  :load-path "share/elisp/markdown-mode"
  :mode ("\\.md\\'" . markdown-mode))



;; * Programming


;; ** Compilation
(use-package compile
  :config (progn
            (make-repeatable-command 'next-error)
            (global-set-key (kbd "C-x `") 'next-error-repeat)
            (global-set-key (kbd "C-x è") 'next-error-repeat)))

(use-package multi-compile
  :load-path "share/elisp/multi-compile"
  :init (progn
          (multi-compile "compile5" :key (kbd "<f5>"))
          (multi-compile "compile6" :key (kbd "<f6>"))
          (multi-compile "compile7" :key (kbd "<f7>"))
          (multi-compile "compile8" :key (kbd "<f8>"))))


;; ** Common features for programming modes
(defun ff/setup-todo-keywords ()
  "Highlight keywords like FIXME or TODO"
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|FIX\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(defun ff/remap-newline-indent ()
  "Remap <RET> to `newline-and-indent'."
  (local-set-key (kbd "RET") 'newline-and-indent))

(ff/add-hooks '(c-mode-common-hook lisp-mode-hook emacs-lisp-mode-hook python-mode-hook
                sh-mode-hook octave-mode-hook LaTeX-mode-hook)
              '(ff/setup-todo-keywords ff/remap-newline-indent autopair-on
		auto-dtw-mode))

(ff/add-hooks '(c-mode-common-hook LaTeX-mode-hook)
              '(yas-minor-mode-on))


;; ** LISP

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

(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)

;; *** Auto-compile
(use-package packed
  :load-path "share/elisp/packed"
  :defer     t)
(use-package auto-compile
  :load-path "share/elisp/auto-compile"
  :commands  turn-on-auto-compile-mode
  :init      (add-hook 'emacs-lisp-mode-hook
                       'turn-on-auto-compile-mode)
  :config    t)


;; ** C/C++

(use-package cc-mode
  :defer  t
  :config (progn
            (c-add-style "my-cc-style"
                         '("gnu"
                           (c-offsets-alist . ((innamespace . [0])))))
            (setq c-default-style '((java-mode . "java")
                                    (awk-mode  . "awk")
                                    (c++-mode  . "my-cc-style")))))


;; ** Python

(use-package info
  :defer  t
  :config (use-package pydoc-info
            :load-path "share/elisp/pydoc-info"
            :init (add-to-list 'Info-default-directory-list
                               (ff/emacsd "share/info"))))


;; ** Fortran

(use-package fortran-index-args
  :load-path "share/elisp/fortran-index-args"
  :commands  fia/toggle)

(use-package fortran
  :defer  t
  :config (define-key fortran-mode-map (kbd "C-c i") 'fia/toggle))



;; * Postamble
(promote-minor-mode-map 'custom-bindings-mode)
(provide 'init-extra)
