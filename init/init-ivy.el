(require-package 'ivy)
(ivy-mode 1)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-h a") 'counsel-apropos)

;; ignore candidate
(define-key ivy-minibuffer-map (kbd "s-<return>") 'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)

(setq-default ivy-use-virtual-buffers t
              ivy-display-style 'fancy
              ivy-count-format "(%d/%d) "
              ivy-virtual-abbreviate 'fullpath
              projectile-completion-system 'ivy
              ivy-initial-inputs-alist nil)

;; (require-package 'ivy-rich)
;; (require 'ivy-rich)
;; (ivy-rich-mode 1)

(require-package 'swiper)
(require'swiper)

(setq counsel-find-file-at-point t)
(setq enable-recursive-minibuffers t)

(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)

(define-key swiper-map (kbd "s-r") 'swiper-query-replace)

(setq slegetank/temp-swiper-cands nil)

(defun slegetank/swiper-evil-mc ()
  "Create a fake cursor for each `swiper' candidate."
  (interactive)
  (unless (require 'evil-mc nil t)
    (error "Evil-mc isn't installed"))
  (unless (window-minibuffer-p)
    (error "Call me only from `swiper'"))
  (setq slegetank/temp-swiper-cands (nreverse ivy--old-cands))
  (unless (string= ivy-text "")
    (ivy-exit-with-action
     (lambda (_)
       (evil-mc-mode 1)
       (let (cand)
         (while (setq cand (pop slegetank/temp-swiper-cands))
           (swiper--action cand)
           (when slegetank/temp-swiper-cands
             (evil-mc-make-cursor-here))))))))

(define-key ivy-minibuffer-map (kbd "s-m") 'slegetank/swiper-evil-mc)

(defun slegetank/ivy-occur-edit ()
  (interactive)
  "Easier way to edit occur buffer"
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and (get-buffer-window name 'visible)
                 (string-match "^\\*ivy-occur " name))
        (with-current-buffer buffer
          ;; (ivy-wgrep-change-to-wgrep-mode)

          (evil-define-key 'normal wgrep-mode-map (kbd "q") (lambda ()
                                                              (interactive)
                                                              (wgrep-exit)
                                                              (kill-buffer)
                                                              (delete-window (selected-window))))
          (define-key wgrep-mode-map (kbd "C-c C-c") (lambda ()
                                                       (interactive)
                                                       (wgrep-finish-edit)
                                                       (kill-buffer)
                                                       (delete-window (selected-window))))
          (define-key wgrep-mode-map (kbd "C-c C-k") (lambda ()
                                                       (interactive)
                                                       (wgrep-abort-changes)
                                                       (kill-buffer)
                                                       (delete-window (selected-window))))
          (setq-local
           header-line-format
           (substitute-command-keys
            " [m]-match, [x]-exclude, [e]-edit, [q]-quit"))
          (evil-normal-state))))))

(add-hook 'ivy-occur-grep-mode-hook (lambda () (interactive)
                                      (run-with-timer 0.1 nil 'slegetank/ivy-occur-edit)))

(define-key ivy-mode-map (kbd "s-o") 'ivy-occur)

;; (require-package 'flx)

;; (setq ivy-re-builders-alist
;;       '((t . ivy--regex-fuzzy)))

(require-package 'ivy-historian)
(add-hook 'after-init-hook (lambda () (ivy-historian-mode t)))

(recentf-mode 1)

(defun eh-ivy-return-recentf-index (dir)
  (when (and (boundp 'recentf-list)
             recentf-list)
    (let ((files-list
           (cl-subseq recentf-list
                      0 (min (- (length recentf-list) 1) 20)))
          (index 0))
      (while files-list
        (if (string-match-p dir (car files-list))
            (setq files-list nil)
          (setq index (+ index 1))
          (setq files-list (cdr files-list))))
      index)))

(defun eh-ivy-sort-file-function (x y)
  (let* ((x (concat ivy--directory x))
         (y (concat ivy--directory y))
         (x-mtime (nth 5 (file-attributes x)))
         (y-mtime (nth 5 (file-attributes y))))
    (if (file-directory-p x)
        (if (file-directory-p y)
            (let ((x-recentf-index (eh-ivy-return-recentf-index x))
                  (y-recentf-index (eh-ivy-return-recentf-index y)))
              (if (and x-recentf-index y-recentf-index)
                  ;; Directories is sorted by `recentf-list' index
                  (< x-recentf-index y-recentf-index)
                (string< x y)))
          t)
      (if (file-directory-p y)
          nil
        ;; Files is sorted by mtime
        (time-less-p y-mtime x-mtime)))))

(add-to-list 'ivy-sort-functions-alist
             '(read-file-name-internal . eh-ivy-sort-file-function))
