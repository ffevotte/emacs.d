;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local emacs configuration file for azote ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; * Font
(setq default-frame-alist
      '((font-backend . "xft")
        (font . "Bitstream Vera Sans Mono-9")))


;; * Magit configuration
(setq magit-repo-dirs '("~/.etc" "~/.emacs.d" "~/projets/git/" "~/EDF/atelier"))
