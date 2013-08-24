;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local emacs configuration file for azote ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Local installation path
(load-file "~/.emacs.d/local/cedet-snapshot-8387/cedet-devel-load.el")


;; Bitstream font
(setq default-frame-alist
      '((font-backend . "xft")
        (font . "Bitstream Vera Sans Mono-9")))


;; Org-mode
(setq ff/use-org t)


;; Magit configuration
(setq magit-repo-dirs '("~/.etc" "~/.emacs.d" "~/projets/git/" "~/EDF/atelier"))


;; Python info documentation
(add-to-list 'Info-directory-list "/home/francois/.local/share/info")
(load-file "/home/francois/.emacs.d/local/pydoc-info.el")

;; Local Variables:
;;   mode: emacs-lisp
;; End:
