;; Global customization
(setq set-mark-command-repeat-pop t);; Easily cycle through the Mark Ring
(setq visible-bell t)               ;; Visible bell
(show-paren-mode 1)                 ;; Parenthesis matching
(column-number-mode 1)              ;; Show line and column numbers
(line-number-mode 1)
(winner-mode 1)                     ;; Navigate through window layouts with C-c <arrows>


;; Custom key bindings
(global-set-key (kbd "M-g")   'goto-line)        ;; better keybinding for goto-line
(global-set-key (kbd "C-c q") 'join-line)        ;; join this line and the previous one
(global-set-key (kbd "M-y")   'browse-kill-ring) ;; navigate in the kill ring
(global-set-key (kbd "<f5>")  'recompile)        ;; rerun last compilation command

;; Make buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;; Isearch
(define-key isearch-mode-map (kbd "C-o")  ;; occur-mode for Isearch
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))


;; Highlight-line mode
(defun ff/turn-on-highlight-line ()
  "Turn on and setup hl-line-mode"
  (hl-line-mode 1))


;; IBuffer
(load "emacs-ibuffer")
(add-hook 'ibuffer-mode-hook 'ff/turn-on-highlight-line)


;; Dired
(require 'dired-x)
(add-hook 'dired-mode-hook 'ff/turn-on-highlight-line)


;; Ido mode
(setq ido-enable-flex-matching t
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-default-buffer-method 'selected-window)
(ido-mode 1)
(ido-everywhere)
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
(defadvice vc-find-file-hook (after ff/auto-revert-mode-for-vc activate)
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


;; Compilation mode
(setq compilation-scroll-output 'first-error) ;; scroll compilation buffer until first error




;; Mode-specific customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
     (define-key c-mode-map   (kbd "C-c o") 'ff-find-other-file)
     (define-key c++-mode-map (kbd "C-c o") 'ff-find-other-file)))


;; LaTeX
(setq reftex-label-alist '(AMSTeX))          ;; Use \eqref for equation references
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)  ;; Turn on refTeX


;; Gnuplot-mode
(setq gnuplot-display-process nil) ;; dont display the gnuplot window


;; Octave-mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
