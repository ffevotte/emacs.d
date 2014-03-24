;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local emacs configuration file for clau5abb ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; * Font
(setq default-frame-alist
      '((font-backend . "xft")
        (font . "Inconsolata-11")))


;; * Magit configuration
(setq magit-repo-dirs '("~/.emacs.d" "~/.emacs.d/local" "~/.etc" "~/local/perso/projets/git/"))


;; * Frequently used ssh hosts
(setq ssh-term-hosts '("ivanoe1.noe.edf.fr"
                       "ivanoe2.noe.edf.fr"))
