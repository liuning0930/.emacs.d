(defun slegetank/guess-input ()
  "Guess input for youdao dictionary."
  (let* ((thing-at-point (if (use-region-p)
                             (buffer-substring-no-properties (region-beginning) (region-end))
                           (slegetank/thing-at-point))))
    (if (and thing-at-point (> (length thing-at-point) 0))
        thing-at-point
      (if (fboundp 'simpleclip-get-contents)
          (simpleclip-get-contents)
        thing-at-point))))

;; make-term version
(defun slegetank/fast-terminal ()
  "fastway to access terminal. Only open one."
  (interactive)
  (unless (get-buffer-window "*terminal*" 'visible)
    (unless (get-buffer "*terminal*")
      (make-term "terminal" (getenv "SHELL")))
    (with-current-buffer (current-buffer)
      (let ((current-directory default-directory))
        (split-window-sensibly)
        (other-window 1)
        (set-buffer "*terminal*")
        (term-mode)
        (term-char-mode)
        (switch-to-buffer "*terminal*")
        (and current-directory (term-send-raw-string (format "cd %s\n" current-directory)))
        (goto-char (point-max))))))

(add-hook 'term-mode-hook '(lambda ()
                             (evil-define-key 'normal term-raw-map (kbd "q") '(lambda () (interactive) (other-window -1) (delete-window (get-buffer-window "*terminal*"))))
                             (evil-define-key 'normal term-raw-map (kbd "C-r") 'term-send-reverse-search-history)
                             ))

;; auto delete window when process exit
(add-hook 'term-exec-hook (lambda ()
                            (let* ((buff (current-buffer))
                                   (proc (get-buffer-process buff)))
                              (lexical-let ((buff buff))
                                (set-process-sentinel proc (lambda (process event)
                                                             (if (string= event "finished\n")
                                                                 (kill-buffer-and-window))))))))

(setq ediff-split-window-function 'split-window-horizontally)
(require-package 'evil-ediff)
(evil-ediff-init)

(slegetank/leader-define-key "t" nil "Tools")
