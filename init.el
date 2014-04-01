(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)


(add-to-list 'load-path user-emacs-directory 'append)


(defmacro with-timer (title &rest forms)
  (declare (indent 1))
  (let ((nowvar (make-symbol "now"))
        (body   `(progn ,@forms)))
    `(let ((,nowvar (current-time)))
       (message "%s..." ,title)
       (prog1 ,body
         (let ((elapsed
                (float-time (time-subtract (current-time) ,nowvar))))
           (message "%s... done (%.3fs)" ,title elapsed))))))



(with-timer
 "Loading configuration files"

 ;; use-package
 (let ((use-package-dir (concat user-emacs-directory "share/elisp/use-package/")))
   (require 'bind-key    (concat use-package-dir "bind-key.el"))
   (require 'use-package (concat use-package-dir "use-package.el")))
 (setq use-package-verbose t)

 ;; Load local rc file
 (let* ((fullhostname (system-name))
        (hostname     (substring fullhostname 0
                                 (progn
                                   (string-match "\\." (concat fullhostname ".domain"))
                                   (- (match-end 0) 1)))))
   (load (concat "host-" hostname) 'noerror))

 ;; Standard configuration
 (use-package init-std)

 ;; Extra packages
 (use-package init-extra))
