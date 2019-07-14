;;; -*- lexical-binding: t -*-

  (slegetank/leader-define-key "w" nil "Windows"
                               "wk" 'delete-other-windows "Kill other window"
                               "wK" 'delete-window "Kill window"
                               "wf" 'other-frame "Other frame")

(require-package 'switch-window)
(require 'switch-window)
(setq switch-window-shortcut-style 'qwerty)
(setq switch-window-qwerty-shortcuts
      '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o"))
(setq switch-window-increase 17)
;; (setq switch-window-multiple-frames t)
;; (setq switch-window-shortcut-appearance 'asciiart)

(slegetank/leader-define-key "wo" 'switch-window "Other window")

(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(slegetank/leader-define-key "wv"  (split-window-func-with-other-buffer 'split-window-vertically) "Split vertically"
                             "wh" (split-window-func-with-other-buffer 'split-window-horizontally) "Split horizontally")

(require-package 'swap-buffers)
(require 'swap-buffers)
(slegetank/leader-define-key "ws" 'swap-buffers "Swap buffers to another window")

(defun slegetank/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))

(slegetank/leader-define-key (kbd "wd") 'slegetank/toggle-current-window-dedication "Toggle window dedication")
