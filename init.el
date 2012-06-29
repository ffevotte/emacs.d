;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs configuration file ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d" 'append)


;; Load local rc file
(let* ((fullhostname (system-name))
       (hostname     (substring fullhostname 0
                                (progn
                                  (string-match "\\." (concat fullhostname ".domain"))
                                  (- (match-end 0) 1)))))
  (load (concat "host-" hostname) 'noerror))


;; Base configuration  (only standard packages)
(load "init-std")


;; Extended configuration
(load "init-extra" 'noerror)
