;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local emacs configuration file for clau5abb ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; * Magit configuration
(setq magit-repo-dirs '("~/.emacs.d" "~/.emacs.d/local" "~/.etc" "~/local/perso/projets/git/"))


;; * Frequently used ssh hosts
(setq ssh-term-hosts '("ivanoe1.noe.edf.fr"
                       "ivanoe2.noe.edf.fr"))


;; * Frequently used paths
(setq ff/ido-shortcuts
      '(("#" . "/scratch/H55056/")
        ("@" . "/netdata/H55056/")))


;; * Local paths
(add-to-list 'load-path "/home/H55056/.localDebian/usr/share/emacs/site-lisp/maxima/")
