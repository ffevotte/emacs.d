(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)



(setq user-emacs-directory "~/.emacs.d.new/")
(add-to-list 'load-path user-emacs-directory 'append)


(defmacro with-timer (title &rest forms)
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
 (require 'bind-key    "~/.emacs.d.new/packages/use-package/bind-key.el")
 (require 'use-package "~/.emacs.d.new/packages/use-package/use-package.el")
 (setq use-package-verbose t)

 (use-package init-std)
 (use-package init-extra))
