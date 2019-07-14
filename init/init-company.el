(require-package 'company)

(add-hook 'after-init-hook 'global-company-mode)

(add-to-list 'completion-styles 'initials t)

(company-tng-configure-default)
(setq-default company-dabbrev-other-buffers 'all
              company-tooltip-align-annotations t)

;; https://github.com/company-mode/company-mode/issues/15
(when (fboundp 'evil-declare-change-repeat)
  (mapc #'evil-declare-change-repeat
        '(company-complete-common
          company-select-next
          company-select-previous
          company-complete-selection
          company-complete-number
          )))

(setq company-dabbrev-downcase nil
      ;; make previous/next selection in the popup cycles
      company-selection-wrap-around t
      ;; Some languages use camel case naming convention,
      ;; so company should be case sensitive.
      company-dabbrev-ignore-case nil
      ;; press s-number to choose candidate
      company-show-numbers t
      company-idle-delay 0.1
      company-clang-insert-arguments nil
      company-require-match nil
      company-etags-ignore-case t)

(setq company-minimum-prefix-length 2)

;; @see https://github.com/redguardtoo/emacs.d/commit/2ff305c1ddd7faff6dc9fa0869e39f1e9ed1182d
(defadvice company-in-string-or-comment (around company-in-string-or-comment-hack activate)
  ;; you can use (ad-get-arg 0) and (ad-set-arg 0) to tweak the arguments
  (if (memq major-mode '(php-mode html-mode rjsx-mode web-mode nxml-mode))
      (setq ad-return-value nil)
    ad-do-it))

;; press SPACE will accept the highlighted candidate and insert a space
;; `M-x describe-variable company-auto-complete-chars` for details
;; That's BAD idea.
(setq company-auto-complete nil)

(require-package 'company-statistics)
(with-eval-after-load 'company
  (require 'company-statistics)
  (company-statistics-mode))

(require-package 'company-quickhelp)
(company-quickhelp-mode 1)
(setq company-quickhelp-delay 1)

;; (require-package 'company-tabnine)
;; (require 'company-tabnine)
;; (add-to-list 'company-backends #'company-tabnine)

(define-key company-mode-map [remap indent-for-tab-command]
  'company-indent-for-tab-command)

(setq tab-always-indent 'complete)

(defvar completion-at-point-functions-saved nil)

(defun company-indent-for-tab-command (&optional arg)
  (interactive "P")
  (let ((completion-at-point-functions-saved completion-at-point-functions)
        (completion-at-point-functions '(company-complete-common-wrapper)))
    (indent-for-tab-command arg)))

(defun company-complete-common-wrapper ()
  (let ((completion-at-point-functions completion-at-point-functions-saved))
    (company-complete-common)))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)

  (define-key global-map (kbd "C-,") 'company-tabnine)

  ;; 我更喜欢command键
  (dotimes (i 10)
    (define-key company-active-map (read-kbd-macro (format "s-%d" i)) 'company-complete-number)))
