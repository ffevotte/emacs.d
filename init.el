;;; init.el --- Initialization file
;;; Commentary:
;;; Code:

;; * Utilities

;; This section contains base utilities upon which the rest builds

;; ** Minimal base
(load (concat (file-name-directory (or load-file-name
                                       (buffer-file-name)))
              "base.el"))

;; ** Packages management

;; *** Dependencies

(add-to-list 'load-path (ff/emacsd "packages/cask"))
(require 'cask)
(cask-initialize)

;; *** Configuration

(add-to-list 'load-path (ff/emacsd "packages/use-package"))
(require 'use-package)
(setq use-package-verbose t)

(require 'cl-lib)
(eval-when-compile
  (require 'cl))
(defmacro defun-when-installed (package name arglist &rest body)
  "Define a function only if PACKAGE is installed.
This is more or less equivalent to:

  (defun NAME ARGLIST
    BODY)

except that PACKAGE is `require'd lazily when the function is
invoked.  If it can be correctly loaded, BODY is
executed.  Otherwise, a warning message is displayed."
  (declare (indent 3))
  `(progn
     (lexical-let ((installed? (cl-gensym "installed")))
       (set installed? 'unknown)
       (defun ,name ,arglist
         ;; Docstring
         ,@(if (stringp (car body))
               (prog1 (list (car body))
                 (setq body (cdr body))))
         ;; First time
         (when (eq (symbol-value installed?) 'unknown)
           (set installed?
                (require (quote ,package) nil 'noerror))
           (if (not (symbol-value installed?))
               (message ,(format "Could not load package `%s'" (symbol-name package)))))
         (if (symbol-value installed?)
             (progn ,@body))))))

(font-lock-add-keywords
 'emacs-lisp-mode
 `((,(concat "([ \t]*\\(\\_<defun-when-installed\\_>\\)[ \t]*"
             "\\(\\_<\\(\\sw\\|\\s_\\)+\\_>\\)?[ \t]*"
             "\\(\\_<\\(\\sw\\|\\s_\\)+\\_>\\)?")
    (1 font-lock-keyword-face)
    (2 font-lock-constant-face)
    (4 font-lock-function-name-face))))


;; ** Key bindings

;; *** Custom global key bindings

(define-minor-mode custom-bindings-mode
  "Install custom key bindings.

\\{custom-bindings-mode-map}"
  :global t
  :init-value t
  :keymap (make-sparse-keymap))

(defun custom-set-key (key command)
  "Bind KEY to COMMAND in `custom-bindings-mode'."
  (define-key custom-bindings-mode-map key command))

(defun promote-minor-mode-map (mode)
  "Make sure MODE appears first in `minor-mode-map-alist'.
This ensures no key bindings defined in MODE can be shadowed by
another minor-mode."
  (if (not (eq (car (car minor-mode-map-alist)) mode))
      (let ((mykeys (assq mode minor-mode-map-alist)))
        (assq-delete-all mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))


;; *** Repeatable commands

;; (http://stackoverflow.com/a/17310748/1225607)
(use-package repeat)
(defun make-repeatable-command (cmd)
  "Return a new command that is a repeatable version of CMD.
The new command is named CMD-repeat.  CMD should be a quoted
command.

This allows you to bind the command to a compound keystroke and
repeat it with just the final key.  For example:

  (global-set-key (kbd \"C-c a\") (make-repeatable-command 'foo))

will create a new command called foo-repeat.  Typing `C-c a' will
just invoke foo.  Typing `C-c a a a' will invoke foo three times,
and so on."
  (let ((repeatable-command (intern (concat (symbol-name cmd) "/repeat"))))
    (fset repeatable-command
          `(lambda ,(help-function-arglist cmd)
             ,(format "A repeatable version of `%s'." (symbol-name cmd))
             ,(interactive-form cmd)
             ;; see also repeat-message-function
             (setq last-repeatable-command ',cmd)
             (repeat nil)))
    repeatable-command))





;; *** Local rc files

(let* ((fullhostname (system-name))
       (hostname     (substring fullhostname 0
                                (progn
                                  (string-match "\\." (concat fullhostname ".domain"))
                                  (- (match-end 0) 1)))))
  (load (concat user-emacs-directory "host-" hostname) 'noerror))


;; * General settings

;; This section contains everything which is not directly related to text
;; editing.

;; don't suspend emacs on C-z (but C-x C-z still works)
(global-set-key (kbd "C-z") nil)

;; ** Look

;; *** Interface

;; Bare UI
(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)

;; Startup
(setq initial-scratch-message "")           ;; Empty scratch buffer
(setq initial-major-mode 'fundamental-mode) ;;   ... in fundamental-mode
(let-when scratch (get-buffer "*scratch*")
  (with-current-buffer scratch
    (funcall initial-major-mode)))
(setq inhibit-splash-screen t)              ;; No fancy splash screen

;; Show line and column numbers
(column-number-mode 1)
(line-number-mode 1)

;; Window title
(setq-default frame-title-format (list "%b - Emacs"))

;; Fringes
(setq-default indicate-buffer-boundaries 'left)

;; Margins
(let ((m-w 1))
  (setq-default left-margin-width  m-w
                right-margin-width m-w))

;; *** GUI Theme

(when (window-system)
  ;; Tango color theme
  (use-package naquadah-theme
    :config (load-theme 'naquadah t))

  (use-package term
    :defer    t
    :config (progn
	      (set-face-attribute 'term-color-red     nil :foreground "#ef2929")
	      (set-face-attribute 'term-color-green   nil :foreground "#73d216")
	      (set-face-attribute 'term-color-yellow  nil :foreground "#edd400")
	      (set-face-attribute 'term-color-blue    nil :foreground "#729fcf")
	      (set-face-attribute 'term-color-magenta nil :foreground "#ad7fa8")
	      (set-face-attribute 'term-color-cyan    nil :foreground "#c17d11")
	      (set-face-attribute 'term-color-white   nil :foreground "#babdb6")
              (set-face-attribute 'term-color-black   nil :background "#262b2c")))

  (custom-set-faces
   '(fringe ((t (:background "#1f2324")))))


  ;; Font
  (push '(font-backend . "xft")                default-frame-alist)
  (push '(font . "Bitstream Vera Sans Mono-9") default-frame-alist))


;; *** Mode line

(use-package smart-mode-line
  :config
  (progn
    (setq mode-line-position nil)
    (sml/setup)))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))
(rename-modeline "lisp-mode" emacs-lisp-mode "EL")


;; ** Windows management

;; *** Switch between windows

;; Use S-<arrows> to switch between windows
(use-package windmove
  :init   (windmove-default-keybindings)
  :config (setq windmove-wrap-around t))

;; Reclaim S-<arrows> keys in org-related mode
(use-package org
  :defer  t
  :config (progn
            ;; Left/Right are directly reclaimed
            (define-key org-mode-map (kbd "S-<left>")  nil)
            (define-key org-mode-map (kbd "S-<right>") nil)
            ;; Up/Down are kept for places where
            ;; they are useful (dates and such)
            (add-hook 'org-shiftup-final-hook   'windmove-up)
            (add-hook 'org-shiftdown-final-hook 'windmove-down)))

(use-package org-agenda
  :defer  t
  :config (progn
            (define-key org-agenda-mode-map (kbd "S-<left>")  nil)
            (define-key org-agenda-mode-map (kbd "S-<right>") nil)
            (define-key org-agenda-mode-map (kbd "S-<up>")    nil)
            (define-key org-agenda-mode-map (kbd "S-<down>")  nil)))

;; *** Manage window configurations

;; Navigate through window layouts with C-c <arrows>
(winner-mode 1)

;; *** Resize windows

(custom-set-key (kbd "C-x {") (make-repeatable-command 'shrink-window-horizontally))
(custom-set-key (kbd "C-x }") (make-repeatable-command 'enlarge-window-horizontally))
;; `make-repeatable-command' doesn't work with C functions
(custom-set-key (kbd "C-x ^") (make-repeatable-command
                               (defun ff/enlarge-window (size)
                                 "Lisp wrapper around `enlarge-window'"
                                 (interactive "p")
                                 (enlarge-window size))))

;; *** Swap windows

;; Adapted from Magnar Sveen:
;;   http://whattheemacsd.com/buffer-defuns.el-02.html
(defun ff/rotate-windows (count)
  "Perform COUNT circular permutations of the windows."
  (interactive "p")
  (let* ((non-dedicated-windows (-remove 'window-dedicated-p (window-list)))
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

(defun ff/rotate-windows-backwards (count)
  "Perform COUNT circular permutations of the windows.
Rotation is done in the opposite order as `ff/rotate-windows'."
  (interactive "p")
  (ff/rotate-windows (- count)))

(custom-set-key (kbd "H-<left>")  'ff/rotate-windows)
(custom-set-key (kbd "H-<right>") 'ff/rotate-windows-backwards)

;; *** Hydra to wrap all this

(custom-set-key
 (kbd "œ")
 (defhydra windows-hydra (:color amaranth)
   "
Resize windows ^^^^     Switch to  ^^    Window configuration
---------------^^^^     -----------^^    --------------------
^    _<kp-8>_     ^     _b_uffer         _0_: delete window
_<kp-4>_ ✜ _<kp-6>_     _f_ile           _1_: delete others
^    _<kp-5>_     ^     _r_ecent file    _2_: split above/below
^    ^      ^     ^     _B_ookmark       _3_: split left-right
_=_: balance^     ^     ^ ^              _u_ndo

"
   ;; Move
   ("S-<left>"  windmove-left  nil)
   ("S-<right>" windmove-right nil)
   ("S-<up>"    windmove-up    nil)
   ("S-<down>"  windmove-down  nil)
   ("SPC"       other-window   nil)
   ;; Rotate windows
   ("H-<left>"  ff/rotate-windows           nil)
   ("H-<right>" ff/rotate-windows-backwards nil)
   ;; Resize windows
   ("<kp-4>" shrink-window-horizontally  nil)
   ("<kp-6>" enlarge-window-horizontally nil)
   ("<kp-8>" shrink-window               nil)
   ("<kp-5>" enlarge-window              nil)
   ("="      balance-windows             nil)
   ;; Change configuration
   ("<kp-0>" delete-window        nil)
   ("0"      delete-window        nil)
   ("à"      delete-window        nil)
   ("<kp-1>" delete-other-windows nil)
   ("1"      delete-other-windows nil)
   ("&"      delete-other-windows nil)
   ("<kp-2>" split-window-below   nil)
   ("2"      split-window-below   nil)
   ("é"      split-window-below   nil)
   ("<kp-3>" split-window-right   nil)
   ("3"      split-window-right   nil)
   ("\""     split-window-right   nil)
   ("u"      winner-undo          nil)
   ;; Switch buffers / files
   ("b" ido-switch-buffer nil)
   ("f" ido-find-file     nil)
   ("r" helm-recentf      nil)
   ("B" bookmark-jump     nil)
   ;; Quit
   ("q" nil "quit" :color blue)))

;; ** Buffers management

;; *** Find-file and switch-to-buffer in other window

(custom-set-key (kbd "C-x C-f") 'ff/find-file)
(defun ff/find-file (&optional argp)
  "Use prefix argument to select where to find a file.

Without prefix argument ARGP, visit the file in the current window.
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

Without prefix argument ARGP, switch the buffer in the current window.
With a universal prefix, display the buffer in another window.
With two universal arguments, switch the buffer in another window."
  (interactive "p")
  (cond ((eq argp 1)
         (call-interactively 'switch-to-buffer))
        ((eq argp 4)
         (call-interactively 'display-buffer))
        (t
         (call-interactively 'switch-to-buffer-other-window))))

;; *** Uniquify buffer names

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward
		uniquify-separator         ":"))

;; *** Buffer switching

;; If a buffer is displayed in another frame, raise it
(setq-default display-buffer-reuse-frames t)

(use-package ibuffer
  :defer    t
  :init     (defalias 'list-buffers 'ibuffer)
  :config   (ff/load-configuration "ibuffer"))

;; ** User interaction

(defalias 'yes-or-no-p 'y-or-n-p)   ;; Don't bother typing 'yes'

;; *** recursive minibuffer

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; *** ido

(use-package ido
  :init
  (progn
    (ido-mode 1)
    (ido-everywhere 1))

  :config
  (progn
    (put 'ido-exit-minibuffer 'disabled nil)
    (setq ido-enable-flex-matching               t)
    (setq ido-auto-merge-work-directories-length -1)
    (setq ido-create-new-buffer                  'always)
    (setq ido-use-filename-at-point              'guess)
    (setq ido-default-buffer-method              'selected-window)

    ;; from http://oremacs.com/2015/01/09/ido-find-file-tilde/
    (defvar ff/ido-shortcuts nil)

    (defconst ff/ido--shortcuts
      (mapcar
       (lambda (x)
         (destructuring-bind (shortcut . dir) x
           (cons shortcut
                 `(lambda ()
                    (interactive)
                    (ido-set-current-directory ,dir)
                    (setq ido-exit 'refresh)
                    (exit-minibuffer)))))
       ff/ido-shortcuts))

    (defun ff/ido-setup-hook ()
      (mapc
       (lambda (x)
         (define-key ido-file-dir-completion-map (car x) (cdr x)))
       ff/ido--shortcuts))

    (add-hook 'ido-setup-hook #'ff/ido-setup-hook)))

(use-package ido-ubiquitous
  :config
  (ido-ubiquitous-mode 1))

;; *** helm

(use-package helm
  :defer t

  :init
  (progn
    (custom-set-key (kbd "C-x C-h") 'helm-mini)
    (custom-set-key (kbd "C-x C-r") 'helm-recentf)
    (custom-set-key (kbd "C-x M-x") 'helm-M-x)
    (custom-set-key (kbd "C-x C-i") 'helm-imenu)))

(use-package helm-files
  :defer t

  :config
  (progn
    (push `(candidates
            . ,(lambda ()
                 (--remove (string= it sync-recentf-marker)
                           recentf-list)))
          helm-source-recentf)))

;; *** smex

(use-package smex
  :config  (progn
             (custom-set-key (kbd "M-x") 'smex)
             (smex-initialize)))

;; *** Guide-key

(use-package guide-key
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/guide-key-sequence
          '("C-x r"   ;; rectangles, registers and bookmarks
            "C-x 8"   ;; special characters
            "C-x RET" ;; coding system
            "C-x C-k" ;; keyboard macros
            ))
    (guide-key-mode 1)))


;; *** "Toggle" and "Run" key maps

;; **** Toggle

;; http://endlessparentheses.com/the-toggle-map-and-wizardry.html
;; http://irreal.org/blog/?p=2830
(define-prefix-command        'ff/toggle-map)
(custom-set-key (kbd "C-c t") 'ff/toggle-map)
(add-to-list 'guide-key/guide-key-sequence "C-c t")

(define-key ff/toggle-map "e" #'emacs-lisp-mode)
(define-key ff/toggle-map "o" #'org-mode)

(define-key ff/toggle-map "l" #'linum-mode)
(define-key ff/toggle-map "v" #'visual-line-mode)
(define-key ff/toggle-map "d" #'toggle-debug-on-error)

;; **** Run

;; http://endlessparentheses.com/launcher-keymap-for-standalone-features.html
(define-prefix-command        'ff/run-map)
(custom-set-key (kbd "C-c r") 'ff/run-map)
(custom-set-key (kbd "M-r")   'ff/run-map)
(add-to-list 'guide-key/guide-key-sequence "C-c r")
(add-to-list 'guide-key/guide-key-sequence "M-r")

(define-key ff/run-map "f" #'find-dired)
(define-key ff/run-map "g" #'rgrep)
(define-key ff/run-map "G" #'lgrep)
(define-key ff/run-map "m" #'man)
(define-key ff/run-map "C" #'clone-indirect-buffer)


;; ** Persistency

;; *** Variable files

(setq url-configuration-directory (ff/variable-file "url/"))
(setq auto-save-list-file-prefix  (ff/variable-file "auto-save-list/"))
(setq tramp-persistency-file-name (ff/variable-file "tramp"))


;; *** Sessions

(use-package desktop+
  :defer t

  :config
  (progn
    (ido-ubiquitous-mode 1)
    (setq desktop-base-dir (ff/variable-file "desktops/"))
    (setq desktop-save t)

    (add-to-list 'desktop+/special-buffer-modes 'term-mode)
    (add-to-list 'desktop+/special-buffer-modes 'compilation-mode)))


;; *** Recent files

(use-package recentf
  :defer  t

  :idle-priority 10
  :idle (with-timer "Enabling recentf"
          (recentf-mode 1))

  :init
  (progn
    (defun ff/ido-recentf-open ()
      "Use `ido-completing-read' to \\[find-file] a recent file"
      (interactive)
      (recentf-mode 1)
      (if (find-file (ido-completing-read "Find recent file: "
                                          (--remove (string= it sync-recentf-marker) recentf-list)))
          (message "Opening file...")
        (message "Aborting"))))

  :config
  (progn
    (setq recentf-max-saved-items 1000)
    (setq recentf-auto-cleanup    60)
    (setq recentf-save-file (ff/variable-file "recentf"))
    (use-package sync-recentf)))

;; *** Bookmarks

(use-package bookmark+
  :defer t

  :init
  (progn
    (setq bookmark-default-file (ff/variable-file "bookmarks.el"))
    (setq bmkp-last-as-first-bookmark-file nil))

  :config
  (progn
    (require 'bookmark+-lit)
    (setq bmkp-auto-light-when-jump      'all-in-buffer)
    (setq bmkp-auto-light-when-set       'all-in-buffer)
    (setq bmkp-light-style-autonamed     'lfringe)
    (setq bmkp-light-style-non-autonamed 'rfringe)
    (custom-set-key (kbd "C-x <kp-add>")
                    'bmkp-next-bookmark-this-file/buffer-repeat)
    (custom-set-key (kbd "C-x <kp-subtract>")
                    'bmkp-previous-bookmark-this-file/buffer-repeat)))


;; * Text editing

;; This section contains everything related to text editing in general, i.e. not
;; tied to a particular major mode.

;; ** Global configuration

;; *** Enable commands

(put 'set-goal-column  'disabled nil) ;; (C-x C-n)
(put 'narrow-to-region 'disabled nil) ;; (C-x n n)
(put 'upcase-region    'disabled nil) ;; (C-x C-u)
(put 'downcase-region  'disabled nil) ;; (C-x C-l)
(put 'erase-buffer     'disabled nil)

;; *** General behaviour

(setq-default visible-bell t)
(show-paren-mode 1)                                   ;; Parenthesis matching
(setq-default show-paren-style 'mixed)                ;; Show the whole expression if it is too large
(setq-default shift-select-mode nil)                  ;; No shift selection
(setq-default diff-switches "-u")                     ;; Unified diffs
(define-coding-system-alias 'UTF-8 'utf-8)

;; Useful in combination with expand-region
(pending-delete-mode 1)


;; *** Key bindings

(custom-set-key (kbd "M-g")   'goto-line)     ;; better keybinding for goto-line
(custom-set-key (kbd "C-c q") 'join-line)     ;; join this line and the previous one
(custom-set-key (kbd "C-h a") 'apropos)       ;; search everything, not just commands
(custom-set-key (kbd "C-x g") 'revert-buffer)

(custom-set-key (kbd "C-c q") (make-repeatable-command 'join-line))

;; ** Point, mark & region handling

;; Easily cycle through the Mark Ring
(setq-default set-mark-command-repeat-pop t)

;; *** Move point

;; VI-like movement with H-{h,j,k,l}
(custom-set-key (kbd "H-j")   'next-line)
(custom-set-key (kbd "H-k")   'previous-line)
(custom-set-key (kbd "H-h")   'backward-char)
(custom-set-key (kbd "H-l")   'forward-char)
(custom-set-key (kbd "H-M-j") 'forward-paragraph)
(custom-set-key (kbd "H-M-k") 'backward-paragraph)
(custom-set-key (kbd "H-M-h") 'backward-word)
(custom-set-key (kbd "H-M-l") 'forward-word)

;; Move between pages or section headers with M-pgUp / M-pgDn
(defun ff/move-by-heading-or-page (move-heading move-page choose default-pos)
  "Move point by heading or page.

Headings are defined by `outline-minor-mode'. MOVE-HEADING should
be a function which moves point to the next/previous heading.

Pages are separated by . MOVE-PAGE should be a function which
moves point to the next/previous page.

Choose the closest location using the CHOOSE function, and move
to it (or to DEFAULT-POS if no heading nor page is found.)"
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
;; Move to BOL or indentation
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

(global-set-key [remap move-beginning-of-line]
                'ff/move-beginning-of-line)

;; *** Scroll

;; Set mark before scrolling
(defadvice scroll-up (before set-mark activate)
  "Set the mark before scrolling."
  (push-mark))

(defadvice scroll-down (before set-mark activate)
  "Set the mark before scrolling."
  (push-mark))

;; *** Multiple cursors

(use-package multiple-cursors
  :defer t

  :init
  (progn
    (setq mc/list-file (ff/variable-file "mc-lists.el"))

    (defalias 'mc 'mc/edit-lines)
    (custom-set-key (kbd "H-<")     'mc/mark-next-like-this)
    (custom-set-key (kbd "H->")     'mc/mark-previous-like-this)
    (custom-set-key (kbd "C-c H-<") 'mc/mark-all-like-this)
    (custom-set-key (kbd "H-SPC")   'set-rectangular-region-anchor)))

;; *** Expand region

(use-package expand-region
  :defer t

  :init
  (progn
    (custom-set-key (kbd "C-x SPC") 'er/expand-region)))

;; ** Standard text manipulation

;; This section is for standard text manipulation features: copy/paste, undo, ...

;; *** Copy-pasting

;; Yank at point (like in a tty)
(setq-default mouse-yank-at-point t)

;; Browse the kill ring using M-y
(use-package browse-kill-ring
  :config (browse-kill-ring-default-keybindings))

;; *** Undo

;; Undo in regions
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

;; *** Search

;; Call `occur' from isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(use-package helm-swoop
  :defer t

  :init
  (progn
    (custom-set-key (kbd "M-i") 'helm-swoop)
    (define-key isearch-mode-map (kbd "C-i") 'helm-swoop-from-isearch))

  :config
  (progn
    ;; Move up and down like isearch
    (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
    (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
    (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
    (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)))


;; *** Shuffle text

(custom-set-key
 (kbd "C-S-<up>")
 (defun ff/move-line-up ()
   "Move up the current line."
   (interactive)
   (transpose-lines 1)
   (forward-line -2)
   (indent-according-to-mode)))

(custom-set-key
 (kbd "C-S-<down>")
 (defun ff/move-line-down ()
   "Move down the current line."
   (interactive)
   (forward-line 1)
   (transpose-lines 1)
   (forward-line -1)
   (indent-according-to-mode)))

;; *** Handle capitalization

(custom-set-key
 (kbd "M-c")
 (defun ff/capitalize-word (arg)
   "Capitalize the last word (instead of the next one)"
   (interactive "p")
   (capitalize-word (- arg))))


;; ** Save typing

;; This section contains all sorts of features allowing to save some typing by
;; automatically inserting text.

;; *** Abbreviations

(use-package abbrev
  :defer t

  :diminish abbrev-mode

  :init
  (defun ff/enable-abbrev ()
    "Turn `abbrev-mode' on."
    (abbrev-mode 1))

  :config
  (quietly-read-abbrev-file))

;; *** Snippets

(use-package yasnippet
  :defer t

  :diminish (yas-minor-mode . " Y")

  :config
  (progn
    (setq yas-snippet-dirs `(,(ff/emacsd "snippets")))
    (yas-reload-all)))

(defun-when-installed yasnippet ff/enable-yasnippet ()
  (yas-minor-mode 1))

;; *** Auto completion

(use-package auto-complete-config
  :diminish (auto-complete-mode . " ⤳")

  :init
  (progn
    (setq ac-comphist-file (ff/variable-file "ac-comphist.dat"))
    (ac-config-default)))

;; *** Manipulate file names

(custom-set-key (kbd "C-c f") 'ff/insert-file-name)
(defun ff/insert-file-name (filename &optional argp)
  "Insert name of file FILENAME into buffer after point.

With a prefix argument ARGP, expand the file name to its fully
canonicalized path.  See `expand-file-name'.

The default with no prefix is to use the relative path to file
name from current directory, `default-directory'.  See
`file-relative-name'."
  (interactive "fInsert file name: \nP")
  (cond ((eq nil argp)
         (insert (file-relative-name filename)))
        (t
         (insert filename))))

(custom-set-key (kbd "C-c d") 'ff/copy-file-name)
(defun ff/copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; *** Automatically pair braces and quotes

(use-package autopair
  :defer t
  :diminish (autopair-mode . " ⒜")

  :idle-priority 2
  :idle (with-timer "Enabling autopair"
          (funcall
           (defun-when-installed autopair ff/enable-autopair ()
             (autopair-global-mode)))))


;; ** Whitespace handling

;; *** Indentation

(setq-default indent-tabs-mode nil)                 ;; Indent with spaces

;; *** Trailing whitespace

(define-minor-mode auto-dtw-mode
  "Automatically delete trailing whitespace."
  :lighter    " ˽"
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

;; ** Lines handling

;; *** Highlight current line

(defun ff/highlight-line ()
  "Turn on `hl-line-mode'."
  (hl-line-mode 1))

;; *** Truncate long lines

(defun ff/truncate-lines ()
  "Truncate long lines."
  (toggle-truncate-lines 1))

;; *** Fill & unfill paragraphs

(setq-default fill-column 80) ;; Larger fill column

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

;; *** Visual lines and adaptive wrapping

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(use-package adaptive-wrap
  :defer t

  :config
  (progn
    (defun ff/activate-adaptive-wrap-prefix-mode ()
      "Toggle `visual-line-mode' and `adaptive-wrap-prefix-mode' simultaneously."
      (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))
    (add-hook 'visual-line-mode-hook 'ff/activate-adaptive-wrap-prefix-mode)

    (diminish 'adaptive-wrap-prefix-mode)))

(defun ff/no-auto-fill ()
  "Disable `auto-fill-mode' when `visual-line-mode' is active."
  (if visual-line-mode
      (auto-fill-mode -1)))
(add-hook 'visual-line-mode-hook 'ff/no-auto-fill)

(diminish 'auto-fill-function " ¶")
(diminish 'visual-line-mode   " §")

;; ** Special characters

;; *** Fall-back font for unicode characters

(use-package unicode-fonts
  :idle-priority 10
  :idle
  (with-timer "Setup unicode-fonts"
    (unicode-fonts-setup)))


;; *** Easily insert unicode

(defun helm-insert-char ()
  (interactive)
  (helm :prompt "Character: "
        :sources '((name . "Characters")
                   (init . (lambda ()
                             (with-current-buffer (helm-candidate-buffer 'global)
                               (erase-buffer)
                               (mapc (lambda (char)
                                       (insert-char (cdr char))
                                       (insert " ")
                                       (insert (car char))
                                       (newline))
                                     (ucs-names)))))
                   (candidates-in-buffer)
                   (action . (("insert character"
                               . (lambda (candidate)
                                   (print candidate)
                                   (insert (substring candidate 0 1))))
                              ("insert name"
                               . (lambda (candidate)
                                   (insert (substring candidate 2))))
                              ("insert code"
                               . (lambda (candidate)
                                   (let* ((name (substring candidate 2))
                                          (code (cdr (assoc name ucs-names))))
                                     (insert (format "%d" code))))))))))
(custom-set-key (kbd "C-x 8 RET") 'helm-insert-char)

(eval-after-load 'iso-transl
  '(mapc (lambda (cell)
           (let* ((key       (car cell))
                  (char-name (cdr cell))
                  (char      (cdr (assoc char-name (ucs-names)))))
             (define-key iso-transl-ctl-x-8-map
               (kbd key)
               (vector char))))
         '(("<right>"   . "RIGHTWARDS ARROW")
           ("<left>"    . "LEFTWARDS ARROW")
           ("<up>"      . "UPWARDS ARROW")
           ("<down>"    . "DOWNWARDS ARROW")
           ("S-<right>" . "RIGHTWARDS DOUBLE ARROW"))))


;; * Interaction with external tools

;; This section contains everything related to running external processes from
;; within Emacs, or calling emacs from external processes.

;; ** Shell

;; *** Server

(use-package server
  :config
  (progn
    (defvar ff/main-server-name "server")

    (funcall
     (defun ff/server-start ()
       "Start an emacs server using an automatically generated name.
If an emacs server is already running, it is restarted."
       (if (and (not (string= server-name ff/main-server-name))
                (boundp 'server-process)
                server-process
                (memq (process-status server-process) '(connect listen open run)))
           ;; There is already an instance running; just restart it
           (server-start)

         ;; Start a new server
         (setq server-name (format "server%d" (emacs-pid)))
         (when (server-running-p server-name)
           (server-force-delete))
         (message "Starting server with name `%s'." server-name)
         (server-start))

       (setenv "EMACS_SERVER" server-name)))

    (defun ff/main-server ()
      (interactive)
      (when (and (boundp 'server-process)
                 server-process
                 (memq (process-status server-process) '(connect listen open run))
                 (not (string= server-name ff/main-server-name)))
        ;; There is already an instance running under a different name; kill it
        (server-force-delete)
        (delete-process server-process))

      (setq server-name ff/main-server-name)
      (message "Starting main emacs server with name `%s'." server-name)
      (server-start)
      (setenv "EMACS_SERVER" server-name))))

;; *** Bash commands

;; Helper for the E-source shell function
(defun ff/source (filename)
  "Update environment variables from a shell source file.

Source shell script FILENAME, recording any change made to the
environment.  These changes are then applied to Emacs' environment
in `process-environment'."
  (interactive "fSource file: ")

  (message "Sourcing environment from `%s'..." filename)
  (with-temp-buffer

    (shell-command (format "diff -u <(true; export) <(source %s; export)" filename)
                   (current-buffer))

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

;; Helper for the E-grep shell function
(defun ff/grep (&rest args)
  (let* ((quote-arg
          (lambda (arg)
            (format " \"%s\"" arg)))
         (grep-command
          (-reduce-from 'concat
                        "grep -nH"
                        (-map quote-arg
                              args)))
         (grep-buffer
          (save-window-excursion
            (grep grep-command))))
    (winner-undo)
    (switch-to-buffer grep-buffer)))

;; *** Terminal

(use-package term
  :defer  t

  :config
  (progn
    (setq term-buffer-maximum-size 100000)

    (declare-function ff/term-send-raw "init.el")
    (defmacro ff/term-send-raw (binding string)
      `(define-key term-raw-map (kbd ,binding)
         (lambda ()
           ,(format "Send \"%s\" as raw characters to the terminal process." string)
           (interactive)
           (term-send-raw-string ,string))))
    (ff/term-send-raw "C-<right>"     "\e[1;5C")
    (ff/term-send-raw "C-<left>"      "\e[1;5D")
    (ff/term-send-raw "C-<backspace>" "\e\d")

    ;; Automatically yank the active region and go to the end of buffer
    ;; on line->char mode switch
    (define-key term-mode-map (kbd "C-c C-k") 'ff/term-char-mode)
    (defun ff/term-char-mode (argp)
      (interactive "P")
      (when (and argp
                 (use-region-p))
        (kill-ring-save (min (point) (mark))
                        (max (point) (mark)))
        (goto-char (point-max))
        (yank))
      (term-char-mode)
      (term-send-right))
    ))

(use-package multi-term
  :defer t

  :init
  (progn
    (custom-set-key (kbd "<f2>")   'ff/multi-term)
    (custom-set-key (kbd "C-<f2>") 'multi-term-dedicated-toggle)

    (defun ff/multi-term (argp)
      "Open a new terminal or cycle among existing ones.

No prefix arg: cycle among terminals (open one if none exist)
C-u:           create new terminal
C-u C-u:       create new terminal and choose program"
      (interactive "P")
      (cond ((equal argp '(4))
             (let ((current-prefix-arg nil))
               (multi-term)))
            ((equal argp '(16))
             (let ((current-prefix-arg '(4)))
               (multi-term)))
            (t
             (call-interactively 'multi-term-next)))))

  :config
  (progn
    (setq multi-term-dedicated-select-after-open-p t)
    (setq term-bind-key-alist nil)

    (declare-function ff/multi-term-bind "init.el")
    (defun ff/multi-term-bind (key fun)
      (setq term-bind-key-alist
            (delq (assoc key term-bind-key-alist)
                  term-bind-key-alist))
      (when fun
        (add-to-list 'term-bind-key-alist (cons key fun))))

    (declare-function ff/multi-term-raw "init.el")
    (defmacro ff/multi-term-raw (key)
      `(ff/multi-term-bind
        ,key
        (lambda ()
          ,(format "Send raw %s" key)
          (interactive)
          (term-send-raw-string (kbd ,key)))))

    (ff/multi-term-bind "C-c C-j" 'term-line-mode)
    (ff/multi-term-bind "C-c C-u" 'universal-argument)
    (ff/multi-term-bind "C-c C-c" 'term-interrupt-subjob)
    (ff/multi-term-bind "C-c C-e" 'term-send-esc)
    (ff/multi-term-raw  "C-z")
    (ff/multi-term-raw  "C-u")
    (ff/multi-term-raw  "C-k")
    (ff/multi-term-raw  "C-y")
    (ff/multi-term-raw  "C-x ~")))

;; *** Isend

(use-package isend-mode
  :defer t
  :config
  (progn
    (add-hook 'isend-mode-hook 'isend-default-shell-setup)
    (add-hook 'isend-mode-hook 'isend-default-ipython-setup)))

;; ** Filesystem

(use-package dired
  :defer  t
  :bind   ("C-x C-j" . dired-jump)
  :config (progn
	    (add-hook 'dired-mode-hook 'ff/highlight-line)
	    (add-hook 'dired-mode-hook 'ff/truncate-lines)))

;; ** Version control tools

;; *** Auto-revert version-controlled files

(defadvice vc-find-file-hook (after ff/auto-revert-mode-for-vc activate)
  "Activate `auto-revert-mode' for vc-controlled files."
  (when vc-mode (auto-revert-mode 1)))

(use-package autorevert
  :defer t
  :diminish (auto-revert-mode . " ↻"))


;; *** Git

(use-package magit
  :defer t

  :init
  (progn
    (custom-set-key (kbd "C-c v") 'magit-status))

  :config
  (magit-auto-revert-mode -1))


(use-package git-gutter
  :diminish (git-gutter-mode . " ±")

  :init
  (progn
    (define-key ff/toggle-map "g" 'git-gutter-mode))

  :config
  (progn
    (use-package git-gutter-fringe)

    (custom-set-key
     (kbd "C-c g")
     (defhydra git-gutter-hydra
       (:pre
        (progn (git-gutter-mode 1)
               (display-buffer (get-buffer-create "*git-gutter:diff*")))
        :post
        (winner-undo))

       "
Git gutter:
  _j_: next hunk        _s_tage hunk     _q_uit
  _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _h_: first hunk
  _l_: last hunk        set start _R_evision
"
       ("j" git-gutter:next-hunk                  nil)
       ("k" git-gutter:previous-hunk              nil)
       ("<home>" (progn (goto-char (point-min))
                        (git-gutter:next-hunk 1)) nil)
       ("h" (progn (goto-char (point-min))
                   (git-gutter:next-hunk 1))      nil)
       ("<end>" (progn (goto-char (point-min))
               (git-gutter:previous-hunk 1))      nil)
       ("l" (progn (goto-char (point-min))
               (git-gutter:previous-hunk 1))      nil)
       ("s" git-gutter:stage-hunk                 nil)
       ("r" git-gutter:revert-hunk                nil)
       ("p" git-gutter:popup-hunk                 nil)
       ("R" git-gutter:set-start-revision         nil)
       ("q" nil nil :color blue)
       ("Q" (progn (git-gutter-mode -1)
                   (sit-for 0.1)
                   (git-gutter:clear))
        nil :color blue)))))

;; ** Various tools

;; *** grep

;; Make grep output editable
(use-package grep
  :defer t
  :config
  (use-package wgrep
    :config
    (define-key grep-mode-map
      (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)))

;; *** a2ps

;; Print (part of) a buffer using a2ps
(use-package a2ps-multibyte
  :init
  (progn
    (custom-set-key (kbd "<print>")    'a2ps-buffer)
    (custom-set-key (kbd "C-<print>")  'a2ps-region))

  :config
  (progn
    (setq a2ps-command  "a2ps-view")
    (setq a2ps-switches '("-l" "100"))
    (add-hook 'a2ps-filter-functions
              (defun ff/a2ps-insert-page-breaks ()
                (ff/insert-page-breaks 76 5)))

    (declare-function ff/insert-page-breaks "init.el")
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

;; *** SLURM

(use-package slurm)


;; * Authoring

;; ** Common features for text modes

;; *** Auto-filling
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; *** Spell check

(use-package flyspell
  :defer  t
  :diminish (flyspell-mode . " 🛪")

  :config
  (progn
    ;; Arrange for the entire buffer to be checked when `flyspell-mode' is activated
    (defun ff/flyspell-buffer-after-activation ()
      "Run `flyspell-buffer' after `flyspell-mode' is activated."
      (when flyspell-mode
        (flyspell-buffer)))
    (add-hook 'flyspell-mode-hook 'ff/flyspell-buffer-after-activation)

    ;; Use <F1> to correct the word at point
    (define-key flyspell-mode-map (kbd "<f1>") 'flyspell-correct-word-before-point)))

(defun ff/enable-flyspell ()
  "Turn `flyspell-mode' on."
  (flyspell-mode 1))

;; ** Org

(use-package org
  :defer t

  :config
  (progn
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (C          . t)
       (python     . t)
       (maxima     . t)
       (gnuplot    . t)
       (sh         . t)))))


;; ** GMail

(use-package gmail-message-mode
  :defer t

  :init
  (add-to-list 'auto-mode-alist
               '("[\\\\/]itsalltext[\\\\/]mail\\.google\\..*\\.md\\'" . gmail-message-mode)))

;; ** LaTeX

;; *** Global configuration

(use-package latex
  :defer t

  :config
  (progn
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
    (add-hook 'LaTeX-mode-hook 'ff/enable-yasnippet)

    (add-hook 'LaTeX-mode-hook
              (defun ff/enable-latex-math-mode () (LaTeX-math-mode 1)))

    (add-hook 'LaTeX-mode-hook
              (defun ff/enable-TeX-PDF-mode () (TeX-PDF-mode 1)))

    (add-hook 'LaTeX-mode-hook
              (defun ff/guide-key-LaTeX ()
                (guide-key/add-local-guide-key-sequence "`")))))

;; *** Abbreviations

(use-package latex
  :defer t
  :config
  (progn
    (add-hook
     'TeX-mode-hook
     (defun ff/TeX-turn-on-abbrev ()
       (abbrev-mode 1)
       (setq local-abbrev-table TeX-mode-abbrev-table)))))

;; *** Custom functions

(use-package latex
  :defer t
  :config
  (progn
    (define-key LaTeX-mode-map (kbd "~")
      (defun ff/insert-tilde ()
        "Insert a tilde (~) character at point.

Potentially remove surrounding spaces around point, so that the
newly inserted character replaces them."
        (interactive)
        (skip-syntax-forward " ")
        (let ((end (point)))
          (skip-syntax-backward " ")
          (delete-region (point) end)
          (insert "~"))))

    (define-key LaTeX-mode-map (kbd "C-c a")
      (defun ff/align-latex-table ()
        "Align columns in a latex tabular environment."
        (interactive)
        (save-excursion
          (search-backward "\\begin{tabular}")
          (forward-line 1)
          (let ((beg (point)))
            (search-forward "\\end{tabular}")
            (backward-char)
            (align-regexp beg (point) "\\(\\s-*\\)&" 1 1 t)))))

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
        (query-replace body macro)))))

;; *** Preview

(use-package preview
  :defer t
  :config
  (progn
    ;; Fix incompatibility between latex-preview and gs versions
    (setq preview-gs-options '("-q" "-dNOSAFER" "-dNOPAUSE" "-DNOPLATFONTS"
                               "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4"))))

;; *** Manage references

(use-package reftex
  :defer t
  :config (progn
            ;; Use \eqref for equation references
            (setq reftex-label-alist '(AMSTeX))))

;;; *** Compile

(use-package compile
  :defer t

  :config
  (progn
    ;; Parse LaTeX output to determine the source file
    (defun ff/compilation-error-latex-file ()
      "Analyse the LaTeX output to find the source file in which an error was reported."
      (condition-case nil
          (save-excursion
            (save-match-data
              (let ((found  nil)
                    (bound  (point))
                    beg
                    filename)
                (while (not found)
                  ;; Search backward for an opening paren
                  (search-backward "(" nil)

                  ;; Try to find a matching closing paren
                  (condition-case nil
                      (save-excursion
                        (goto-char (scan-sexps (point) 1))

                        (when (or (> (point) bound)         ;; Closing paren after the error message
                                  (not (looking-back ")"))) ;; Non-matching closing delimiter
                          (setq found t)))

                    ;; Unbalanced expression
                    ((error)
                     (setq found t))))

                ;; Extract filename
                (setq beg (1+ (point)))
                (re-search-forward "[[:space:]]" nil)
                (setq filename (buffer-substring beg (- (point) 1)))
                (list filename))))

        ;; Unexpected error
        ((error)
         nil)))

    ;; Unfill "LaTeX Warning" lines
    (defun ff/compilation-LaTeX-filter ()
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (setq buffer-read-only nil)
        (while (re-search-forward "^LaTeX Warning: " nil t)
          (while (not (re-search-forward "\\.$" (line-end-position) t))
            (end-of-line)
            (delete-char 1)
            (beginning-of-line)))
        (setq buffer-read-only t)))
    (add-hook 'compilation-filter-hook 'ff/compilation-LaTeX-filter)

    ;; Add LaTeX warnings detection to the list
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(latex-warning
                   "^LaTeX Warning: .* on input line \\([[:digit:]]+\\)\\.$" ;; Regular expression
                   ff/compilation-error-latex-file                           ;; Filename
                   1                                                         ;; Line number
                   nil                                                       ;; Column number
                   1))                                                       ;; Type (warning)
    (add-to-list 'compilation-error-regexp-alist 'latex-warning)))

;; *** Aggressive sub/super-scripts

(defun TeX-aggressive-sub-super-script (arg)
  "Insert typed character ARG times and possibly a sub/super-script.
Sub/super-script insertion is done only in a (La)TeX math mode region.
The inserted sub/super-script is copied from the last occurence of a
sub/superscript for the token at point."
  (interactive "p")
  (self-insert-command arg)
  (when (texmathp)
    (let ((sub-super-script
           (save-excursion
             (let ((current-token (let ((end (point)))
                                    (backward-sexp 1)
                                    (buffer-substring-no-properties (point) end))))
               (when (search-backward current-token nil t)
                 (search-forward current-token)
                 (let ((begin (point)))
                   (forward-sexp 1)
                   (buffer-substring-no-properties begin (point))))))))
      (when sub-super-script
        (set-mark (point))
        (insert sub-super-script)))))

(define-minor-mode TeX-aggressive-sub-super-script-mode
  "Aggressively complete sub and superscripts in (La)TeX math mode."
  :lighter " ^"
  :init-value nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "_") #'TeX-aggressive-sub-super-script)
            (define-key map (kbd "^") #'TeX-aggressive-sub-super-script)
            map))

;; ** Markdown

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

;; ** Gnuplot

(use-package gnuplot
  :defer t

  :config
  (progn
    ;; don't display the gnuplot window
    (setq gnuplot-display-process nil)))

;; * Programming

;; ** Common features for programming modes

;; *** Compilation

(use-package compile
  :defer t

  :config
  (progn
    (defhydra next-error-hydra
      (custom-bindings-mode-map "C-x")
      "
Compilation errors:
_j_: next error        _h_: first error    _q_uit
_k_: previous error    _l_: last error
"
      ("`" next-error     nil)
      ("è" next-error     nil)
      ("j" next-error     nil :bind nil)
      ("k" previous-error nil :bind nil)
      ("h" first-error    nil :bind nil)
      ("l" last-error     nil :bind nil)
      ("q" nil            nil :color blue))

    ;; scroll compilation buffer until first error
    (setq compilation-scroll-output 'first-error)

    ;; ANSI coloring in compilation buffers
    (require 'ansi-color)
    (defun ff/ansi-colorize-buffer ()
      (setq buffer-read-only nil)
      (ansi-color-apply-on-region (point-min) (point-max))
      (setq buffer-read-only t))
    (add-hook 'compilation-filter-hook 'ff/ansi-colorize-buffer)))

(use-package multi-compile
  :init
  (progn
    (multi-compile "compile5" :key (kbd "<f5>"))
    (multi-compile "compile6" :key (kbd "<f6>"))
    (multi-compile "compile7" :key (kbd "<f7>"))
    (multi-compile "compile8" :key (kbd "<f8>"))

    (defun list-compilation-buffers ()
      (interactive)
      (with-current-buffer (get-buffer-create "*compilation-buffers*")
        (erase-buffer)
        (setq truncate-lines t)
        (cl-labels ((s-truncate-left
                     (len s)
                     (let ((truncated (if (> (string-width s) len)
                                          (s-concat "..." (s-right (- len 3) s))
                                        s)))
                       (s-pad-right len " " truncated))))
          (mapc (lambda (buffer)
                  (when (with-current-buffer buffer (derived-mode-p 'compilation-mode))
                    (let ((name    (with-current-buffer buffer (buffer-name)))
                          (command (with-current-buffer buffer compilation-arguments))
                          (dir     (with-current-buffer buffer compilation-directory)))
                      (condition-case nil
                          (progn
                            (insert (s-truncate-left 20 name) "  ")
                            (insert (s-truncate-left 30 dir) "  ")
                            (insert (car command)))
                        (error t))
                      (newline))))
                (buffer-list))
          (sort-lines nil (point-min) (point-max))
          (goto-char (point-min))
          (insert (format "%-20s  " "NAME")
                  (format "%-30s  " "DIRECTORY")
                  "COMMAND")
          (newline)))
      (display-buffer "*compilation-buffers*"))

    (define-key ff/run-map "c" #'list-compilation-buffers)))


;; *** Flycheck

(use-package flycheck
  :defer    t
  :diminish flycheck-mode

  :config
  (progn
    (defun ff/flycheck-next-error ()
      (interactive)
      (condition-case nil
          (flycheck-next-error)
        (user-error (flycheck-next-error nil 'reset))))

    (define-key flycheck-mode-map [remap flycheck-next-error] 'ff/flycheck-next-error)

    (defun flycheck-mode-line-status-verbose (&optional status)
      "Get a text describing STATUS for use in the mode line.

STATUS defaults to `flycheck-last-status-change' if omitted or
nil."
      (let ((text (pcase (or status flycheck-last-status-change)
                    (`not-checked "not checked")
                    (`no-checker "no checker")
                    (`running "running")
                    (`errored "error")
                    (`finished
                     (if flycheck-current-errors
                         (let ((error-counts (flycheck-count-errors
                                              flycheck-current-errors)))
                           (format "%s errors / %s warnings"
                                   (or (cdr (assq 'error error-counts)) 0)
                                   (or (cdr (assq 'warning error-counts)) 0)))
                       ""))
                    (`interrupted "interrupted")
                    (`suspicious "suspicious"))))
        (concat "FlyCheck: " text)))

    (setq flycheck-mode-line
          '(:eval
            (propertize (flycheck-mode-line-status-text)
                        'mouse-face 'mode-line-highlight

                        'help-echo (concat (flycheck-mode-line-status-verbose) "\n\n"
                                           "mouse-1: go to next error\n"
                                           "mouse-2: select checker\n"
                                           "mouse-3: go to previous error\n")

                        'local-map
                        (let ((map (make-sparse-keymap)))
                          (define-key map [mode-line mouse-1] 'ff/flycheck-next-error)
                          (define-key map [mode-line mouse-2] 'flycheck-select-checker)
                          (define-key map [mode-line mouse-3] 'flycheck-previous-error)
                          map))))

    (add-to-list 'mode-line-misc-info
                 '(flycheck-mode ("" flycheck-mode-line " ")))))


;; *** Executable scripts

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; *** TODO keywords

(defun ff/setup-todo-keywords ()
  "Highlight keywords like FIXME or TODO."
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|FIX\\|HACK\\|REFACTOR\\|NOCOMMIT\\)\\>"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'ff/setup-todo-keywords)

;; *** Manage whitespace

(add-hook 'prog-mode-hook 'auto-dtw-mode)

;; *** Outline

(use-package org
  :defer t
  :diminish (orgstruct-mode . " 🖹")

  :init
  (add-hook 'prog-mode-hook #'turn-on-orgstruct)

  :config
  (progn
    ;; Highlight outline
    (defun ff/orgstruct-setup ()
      (setq orgstruct-heading-prefix-regexp
            (cond ((derived-mode-p 'emacs-lisp-mode)
                   ";; ")
                  (t
                   (concat comment-start "[[:space:]]*"))))
      (dotimes (i 8)
        (font-lock-add-keywords
         nil
         `((,(format "^%s\\(\\*\\{%d\\} .*\\)"
                     orgstruct-heading-prefix-regexp
                     (1+ i))
            1 ',(intern (format "outline-%d" (1+ i))) t)))))
    (add-hook 'orgstruct-mode-hook #'ff/orgstruct-setup)

    ;; Globally cycle visibility
    (defun ff/orgstruct-global-cycle ()
      "Cycle the global headings visibility, even when outside structure.
This function is similar to `orgstruct-hijacker-org-shifttab-1',
except it never falls back to default bindings."
      (interactive)
      ;; This part is copied from `orgstruct-make-binding'
      (org-run-like-in-org-mode
       (lambda ()
         (interactive)
         (let* ((org-heading-regexp
                 (concat "^"
                         orgstruct-heading-prefix-regexp
                         "\\(\\*+\\)\\(?: +\\(.*?\\)\\)?[		]*$"))
                (org-outline-regexp
                 (concat orgstruct-heading-prefix-regexp "\\*+ "))
                (org-outline-regexp-bol
                 (concat "^" org-outline-regexp))
                (outline-regexp org-outline-regexp)
                (outline-heading-end-regexp "\n")
                (outline-level 'org-outline-level)
                (outline-heading-alist))
           (call-interactively #'org-global-cycle)))))
    (define-key orgstruct-mode-map (kbd "S-<iso-lefttab>") #'ff/orgstruct-global-cycle)))

;; *** Hide-show mode

(use-package hideshow
  :defer t
  :diminish (hs-minor-mode . " ℏ")

  :init
  (progn
    (defun ff/enable-hideshow ()
      "Turn on Hide-Show mode"
      (hs-minor-mode 1))

    (add-hook 'prog-mode-hook 'ff/enable-hideshow))

  :config
  (progn
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

;; *** Aggressive indentation

(use-package aggressive-indent
  :defer t
  :diminish (aggressive-indent-mode . " ⭾")

  :init
  (progn
    (add-hook 'prog-mode-hook #'aggressive-indent-mode)))


;; ** LISP

;; *** Inline evaluation

(defun ff/eval-and-replace ()
  "Evaluate the sexp at point and replace it with its value."
  (interactive)
  (let ((value (eval-last-sexp nil)))
    (kill-sexp -1)
    (insert (format "%S" value))))

(custom-set-key
 (kbd "C-x C-e")
 (defun ff/eval-last-sexp (&optional argp)
   "Evaluate sexp before point.
With a prefix argument, replace the sexp by its evaluation."
   (interactive "P")
   (if argp
       (call-interactively 'ff/eval-and-replace)
     (call-interactively 'eval-last-sexp))))

;; *** Auto-compile

(use-package auto-compile
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook
            (defun-when-installed auto-compile ff/enable-auto-compile-mode ()
              (turn-on-auto-compile-mode))))


;; ** C/C++

;; *** Programming style

(use-package cc-mode
  :defer  t
  :config (progn
            (c-add-style "my-c++-style"
                         '("gnu"
                           (c-offsets-alist . ((innamespace . [0])))))
            (push '(c++-mode . "my-c++-style") c-default-style)))

;; *** Enable yasnippet

(add-hook 'c-mode-common-hook 'ff/enable-yasnippet)

;; *** Switch between header and implementation files

(use-package cc-mode
  :defer t
  :config
  (progn
    (define-key c-mode-map   (kbd "C-c o") 'ff-find-other-file)
    (define-key c++-mode-map (kbd "C-c o") 'ff-find-other-file)))

;; *** Index sources with GNU/global

(use-package ggtags
  :defer t

  :init
  (defun-when-installed ggtags ff/enable-gtags ()
    "Turn `ggtags-mode' on if a global tags file has been generated.

This function asynchronously runs 'global -u' to update global
tags. When the command successfully returns, `ggtags-mode' is
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
              (ggtags-mode 1))))))))

(use-package cc-mode
  :defer t
  :config
  (add-hook 'c-mode-common-hook 'ff/enable-gtags))

;; ** Python

(use-package info
  :defer  t
  :config (use-package pydoc-info
            :init (add-to-list 'Info-default-directory-list
                               (ff/emacsd "share/info"))))


;; ** Fortran

(use-package fortran
  :defer t

  :config
  (use-package fortran-index-args
    :config (define-key fortran-mode-map (kbd "C-c i") 'fia/toggle)))

;; ** Octave / Matlab

(use-package octave-mod
  :mode        ("\\.m\\'" . octave-mode)
  :interpreter ("octave"  . octave-mode))

;; ** Maxima

(use-package maxima
  :mode   ("\\.mac\\'" . maxima-mode)
  :config
  (progn
    (require 'imaxima)))

;; * Postamble

;; Ensure `custom-bindings-mode' has precendence
(promote-minor-mode-map 'custom-bindings-mode)

;; Don't display messages for idle use-package forms
(setq use-package-verbose nil)

(provide 'init)
;;; init.el ends here
