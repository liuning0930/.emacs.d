(require-package 'counsel-projectile)
(require 'counsel-projectile)
(add-to-list 'grep-find-ignored-files "bundle.js")
(counsel-projectile-mode)
(setq projectile-require-project-root nil)
(setq projectile-enable-caching t)
;; (setq-default
;;    projectile-mode-line
;;    '(:eval
;;      (if (file-remote-p default-directory)
;;          " Proj"
;;        (format " Proj[%s]" (projectile-project-name)))))

(require-package 'neotree)
(require 'neotree)
(use-package neotree
  :ensure t
  :config
  (slegetank/leader-define-key "dn" 'custom-neotree-toggle-project-dir "Neotree")

  (setq neo-smart-open t)
  (setq neo-window-fixed-size nil)
  (setq neo-force-change-root t)
  (setq projectile-switch-project-action 'neotree-projectile-action)

(setq neo-hidden-regexp-list '("^\\." "\\.cs\\.meta$" "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "*_flymake.py$"))

(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "<tab>") (neotree-make-executor :dir-fn 'neo-open-dir))
            (define-key evil-normal-state-local-map (kbd "u") 'neotree-select-up-node)
            (define-key evil-normal-state-local-map (kbd "v") 'neotree-quick-look)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "a") 'neotree-hidden-file-toggle)
            (define-key evil-normal-state-local-map (kbd "]") 'neotree-stretch-toggle)
            (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
            (define-key evil-normal-state-local-map (kbd "r") 'neotree-rename-node)
            (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node)
            (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node)
            (define-key evil-normal-state-local-map (kbd "o") (lambda () (interactive) (shell-command (format "open %s" (neo-buffer--get-filename-current-line)))))
            (define-key evil-normal-state-local-map (kbd "s-j") 'neotree-select-next-sibling-node)
            (define-key evil-normal-state-local-map (kbd "s-k") 'neotree-select-previous-sibling-node)

            (define-key evil-normal-state-local-map (kbd "<s-return>") 'neotree-enter-vertical-split)

            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter))))

(use-package find-file-in-project :ensure t)

(defun custom-neotree-toggle-project-dir ()
  "Open NeoTree using the git root. & show imenu"
  (interactive)
  ;; (imenu-list-smart-toggle)
  (if (neo-global--window-exists-p)
      (progn (neotree-hide)
             ;; (imenu-list-quit-window)
             )
    (let ((project-dir (or (ffip-project-root) (file-name-directory (or (buffer-file-name) ""))))
          (file-name (buffer-file-name)))
      ;; (imenu-list-show)
      (if project-dir
          (progn
            (neotree-dir project-dir)
            (neotree-find file-name))
        (message "Buffer is invalid.")))))

(setq neo-autorefresh t)
(defvar slegetank/last-neotree-refresh-buffer-file nil
  "Store the last refresh's neotree's buffer file name")

(defun slegetank/neo-auto-refresh (window)
  "NeoTree's autorefresh interval is too long. Only refresh when the buffer is a file buffer and not ends with ]"
  (when neo-global--autorefresh-timer
    (cancel-timer neo-global--autorefresh-timer))
  (when neo-autorefresh
    (setq neo-global--autorefresh-timer
          (run-with-idle-timer 0.5 t (lambda ()
                                       (interactive)
                                       (let ((buffer-filename (buffer-file-name)))
                                         (when (and buffer-filename
                                                    (neo-global--window-exists-p)
                                                    (not (s-ends-with? "]" buffer-filename))
                                                    (not (equal slegetank/last-neotree-refresh-buffer-file buffer-filename)))
                                           (setq slegetank/last-neotree-refresh-buffer-file buffer-filename)
                                           (neotree-refresh t))))))))

(add-hook 'neo-after-create-hook 'slegetank/neo-auto-refresh)

(defun slegetank/neotree-collapse-all ()
  (interactive)
  "Collapse all"
  (neo-global--select-window)
  (let ((filename (neo-buffer--get-filename-current-line)))
    (setq neo-buffer--expanded-node-list nil)
    (neotree-find filename)
    ;; (if (file-directory-p filename)
    ;;     (neo-buffer--set-expand filename (neo-buffer--expanded-node-p filename))
    ;;   (neotree-select-up-node))
    ))

(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "S-<tab>") 'slegetank/neotree-collapse-all)))

(defun slegetank/projectile-ag (&optional options)
  "Copy from counsel, and use .projectile"
  (interactive)
  (let* ((options
              (if current-prefix-arg
                  (read-string "options: ")
                options))
             (ignored
              (append
               (cl-union (projectile-ignored-files-rel) grep-find-ignored-files)
               (cl-union (projectile-ignored-directories-rel) grep-find-ignored-directories)))
             (options
              (concat options " "
                      (mapconcat (lambda (i)
                                   (concat "--ignore " (shell-quote-argument i)))
                                 ignored
                                 " "))))
        ;; -U ignore .gitignore but use .ignore
        (counsel-ag (eval counsel-projectile-ag-initial-input)
                    (or (projectile-project-root) default-directory)
                    (concat options " " "-U")
                    (projectile-prepend-project-name "ag"))))

(defun custom-note-init (filename)
  "Init a note."
  (interactive (list (read-string (format "Enter new note name: %s%s-" default-directory  (format-time-string "%Y%m%d" (current-time))))))
  (if (string= "" filename)
      (message "Need a filename")
    (or (string-match "\\.org$" filename) (setq filename (concat (file-name-sans-extension filename) ".org")))
    (progn (setq filepath (format "%s%s-%s" default-directory (format-time-string "%Y%m%d" (current-time)) filename))
           (find-file filepath)
           (let ((custom-org-buffer (get-file-buffer filepath))
                 (user-name (s-trim-right (shell-command-to-string "git config --global user.name") ))
                 (user-email (s-trim-right (shell-command-to-string "git config --global user.email") )))
             (with-current-buffer custom-org-buffer
               (insert (format "#+OPTIONS: \\n:\\n ^:nil\n\n#+TITLE: \n#+AUTHOR: %s\n#+EMAIL: %s\n#+DATE: %s\n" user-name user-email (format-time-string "<%Y-%m-%d %H:%M>" (current-time))))
               (goto-char 34))))))

(slegetank/leader-define-key
                             "p" nil "Project"
                             "pf" 'counsel-projectile-find-file "File in project"
                             "pb" 'counsel-projectile-switch-to-buffer "Buffer in project"
                             "pp" 'counsel-projectile-switch-project "Other project"
                             "pn" 'custom-note-init "Init project note file"
                             "pc" 'projectile-invalidate-cache "Reset projectile cache of this project")

(define-key evil-motion-state-map (kbd "s-F") 'slegetank/projectile-ag)
(define-key global-map (kbd "s-F") 'slegetank/projectile-ag)
