;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local emacs configuration file for azote ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bitstream font
(setq default-frame-alist
      '((font-backend . "xft")
        (font . "Bitstream Vera Sans Mono-9")))


;; Org-mode
(setq ff/use-org t)


;; Magit configuration
(setq magit-repo-dirs '("~/.etc" "~/.emacs.d" "~/projets/git/" "~/EDF/atelier"))


;; Recent CEDET version
(load-file "/home/francois/.emacs.d/local/cedet-1.1/common/cedet.el")
(semantic-load-enable-code-helpers)
(require 'semantic-ia)


;; Local Variables:
;;   mode: emacs-lisp
;; End:
