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


;; * Specific compilation regexps for Cocagne
(use-package compile
  :defer t
  :config
  (add-to-list 'compilation-error-regexp-alist 'cocagne-error)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(cocagne-error
                 "Test .*: \\([^ ]*\\) .****\\(Failed\\)"
                 (1 "Tests/%s.dir/") nil nil
                 2 1
                 (2 compilation-error-face)))

  (add-to-list 'compilation-error-regexp-alist 'cocagne-success)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(cocagne-success
                 "Test .*: \\([^ ]*\\) .*\\(Passed\\)"
                 (1 "Tests/%s.dir/") nil nil
                 0 1
                 (2 compilation-info-face))))
