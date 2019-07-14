(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

(with-eval-after-load 'ivy
  (define-key ivy-minibuffer-map (kbd "<backspace>") 'ivy-backward-delete-char))
