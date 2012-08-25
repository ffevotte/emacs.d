;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local emacs configuration file for azote ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/projets/git/expand-region.el")
(setq ff/use-org t)

;; Magit configuration
(setq magit-repo-dirs '("~/.etc" "~/.emacs.d" "~/projets/git/" "~/EDF/atelier"))

;; Multiple cursors
(add-to-list 'load-path "~/projets/git/multiple-cursors.el")
(require 'multiple-cursors)
(defalias 'mc 'mc/edit-lines)
(global-set-key (kbd "C-S-SPC") 'set-rectangular-region-anchor)
(define-key rectangular-region-mode-map (kbd "C-g")
  (lambda ()
    (interactive)
    (execute-kbd-macro [?  backspace ?\C-  ?\C- ])))

;; Local Variables:
;;   mode: emacs-lisp
;; End:
