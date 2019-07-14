(setq package-selected-packages (append package-selected-packages '(smex
                                                                    smartparens
                                                                    popwin
                                                                    reveal-in-osx-finder
                                                                    rainbow-delimiters
                                                                    recentf-ext
                                                                    nyan-mode
                                                                    skewer-mode
                                                                    sr-speedbar
                                                                    flycheck
                                                                    yasnippet
                                                                    imenu-list
                                                                    neotree
                                                                    auto-highlight-symbol
                                                                    json-mode
                                                                    simpleclip
                                                                    ggtags
                                                                    xah-get-thing
                                                                    protobuf-mode
                                                                    pos-tip
                                                                    quickrun
                                                                    popup
                                                                    htmlize
                                                                    vdiff
                                                                    yaml-mode
                                                                    powershell
                                                                  )))

(require 'cl-lib)
(defun slegtank/check-installed-packages ()
  (cl-loop for pkg in package-selected-packages
           when (not (package-installed-p pkg)) do (cl-return nil)
           finally (cl-return t)))

(unless (slegtank/check-installed-packages)
  (message "%s" "Some packages missed, refreshing...")
  (package-refresh-contents)
  (dolist (pkg package-selected-packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(require 'smartparens-config)
(smartparens-global-mode t) ; smart paren

;; elisp单引号不自动配对
(sp-local-pair '(emacs-lisp-mode lisp-interaction-mode lisp-mode) "'" nil :actions nil)

;; 括号匹配
;; 不要了 会卡
;; (defadvice show-paren-function (around fix-show-paren-function activate)
;;   "Highlight enclosing parens."
;;   (cond ((looking-at-p "\\s(") ad-do-it)
;;         (t (save-excursion
;;              (ignore-errors (backward-up-list))
;;              ad-do-it))))

;; popwin
(require 'popwin)
(popwin-mode t)

(setq popwin:popup-window-height 35
      popwin:special-display-config
      '(("*Miniedit Help*" :noselect t)
        (help-mode :noselect nil)
        ("*quickrun*" :stick t)
        (completion-list-mode :noselect t)
        (compilation-mode :noselect nil)
        (grep-mode :noselect t)
        (occur-mode :noselect t)
        (anaconda-view-mode :noselect nil)
        ("*Pp Macroexpand Output*" :noselect t)
        ("*Shell Command Output*")
        ("*Async Shell Command*")
        ("*vc-diff*")
        ("*vc-change-log*")
        (" *undo-tree*" :width 60 :position right)
        ("^\\*anything.*\\*$" :regexp t)
        ("*slime-apropos*")
        ("*slime-macroexpansion*")
        ("*slime-description*")
        ("*slime-compilation*" :noselect t)
        ("*slime-xref*")
        ("*Flycheck errors*")
        ("*Warnings*")
        ("*Error*")
        ("*Process List*")
        ("*Smex: Unbound Commands*")
        ("*Paradox Report*" :noselect nil)
        ("*Package Commit List*" :noselect nil)
        ("*Diff*" :noselect nil)
        ;; ("*Messages*" :noselect nil)
        ("*Google Maps*" :noselect nil)
        ("*ag search*" :noselect nil)
        ("*PDF-Occur*" :noselect nil)
        ("*PDF-Metadata*" :noselect nil)
        ("^\\*Outline .*\\.pdf\\*$" :regexp t :noselect nil)
        ("*MULTI-TERM-DEDICATED*" :noselect nil :stick t)
        (sldb-mode :stick t)
        (slime-repl-mode)
        (slime-connection-list-mode)))

(defun slegetank/popwin-after-pop ()
  "Some pop window I need evil"
  (unless (equal major-mode 'help-mode)
    (turn-off-evil-mode)))

(add-hook 'popwin:after-popup-hook 'slegetank/popwin-after-pop)
(bind-keys :map popwin:window-map
           ((kbd "<escape>") . popwin:close-popup-window))

(slegetank/leader-define-key "fo" 'reveal-in-osx-finder "Reveal in Finder")

(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

(nyan-mode t)
(nyan-start-animation)

(require 'sr-speedbar)
(slegetank/leader-define-key "fs" 'custom-open-speedbar "Speedbar")

(defun custom-open-speedbar ()
  "Open & select & expand"
  (interactive)
  (unless (member 'speedbar-mode minor-mode-list)
    (let ((current-file (buffer-file-name)))
      (unless (sr-speedbar-exist-p)
        (sr-speedbar-open))
      (sr-speedbar-select-window)
      (speedbar-find-selected-file current-file)
      (speedbar-expand-line))))

(define-key speedbar-key-map (kbd "<tab>") 'speedbar-toggle-line-expansion)
(define-key speedbar-key-map (kbd "q") 'sr-speedbar-close)
(define-key speedbar-key-map (kbd "^") 'speedbar-up-directory)
(sr-speedbar-refresh-turn-on)

(require 'recentf-ext)

(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

(slegetank/leader-define-key "c" nil "Check"
  "cn" 'flycheck-next-error "Next error"
  "cp" 'flycheck-previous-error "Previous error"
  "cl" 'flycheck-list-errors "List errors")

(require 'yasnippet)
(yas-global-mode 1)

(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'web-mode-hook 'skewer-html-mode)

(require 'auto-highlight-symbol)
  (global-auto-highlight-symbol-mode)

  (use-package auto-highlight-symbol
                                          ; this only installs it for programming mode derivatives; you can also make it global...
    :init
    (add-hook 'org-mode-hook 'auto-highlight-symbol-mode)
    (add-hook 'rjsx-mode-hook 'auto-highlight-symbol-mode)
    (add-hook 'hexl-mode-hook 'disable-hi)
    (add-hook 'auto-highlight-symbol-mode-hook (lambda ()
                                                 (define-key evil-normal-state-map (kbd "C-p") 'ahs-backward)
                                                 (define-key evil-normal-state-map (kbd "C-n") 'ahs-forward)))
    :config
    (setq ahs-idle-interval 0.5) ;; if you want instant highlighting, set it to 0, but I find it annoying
    (setq ahs-default-range 'ahs-range-whole-buffer) ;; highlight every occurence in buffer
)

(defun slegetank/clean-symbol-after-avy (oldfunc &rest args)
  "After evil-avy-goto-char clean symbols"
  (apply oldfunc args)
  ;; (push-mark)
  (ahs-clear))

(with-eval-after-load 'avy
  (advice-add 'evil-avy-goto-char :around 'slegetank/clean-symbol-after-avy))

(require 'simpleclip)
(simpleclip-mode 1)

(setq simpleclip-unmark-on-copy t)

(defun slegetank/fix-evil-clip-not-function (oldfunc &rest args)
"Fix evil clip is nil."
  (apply oldfunc args)
  (car args))

(advice-add 'kill-new :around 'slegetank/fix-evil-clip-not-function)

(defun slegetank/after-paste-cursor-goto-begin (oldfunc &rest args)
  "After paste, go to begin instead of end."
  (apply oldfunc args)
  ;; not minibuffer
  (unless (window-minibuffer-p)
    ;; more than 1 line
    (let* ((p1 (car slegetank/simpleclip-last-paste-region))
           (p2 (+ p1 (cdr slegetank/simpleclip-last-paste-region))))
      (when (> (count-lines p1 p2) 1)
        (goto-char (car slegetank/simpleclip-last-paste-region)))
     (setq mark-active nil))))

(advice-add 'simpleclip-paste :around 'slegetank/after-paste-cursor-goto-begin)

(defun slegetank/after-paste-indent (oldfunc &rest args)
  "After paste indent the region."
  (apply oldfunc args)
  (unless (window-minibuffer-p)
    (indent-region  slegetank/simpleclip-last-paste-region)))

(advice-add 'simpleclip-paste :around 'slegetank/after-paste-indent)

(setq slegetank/simpleclip-last-paste-region nil)
(advice-add 'simpleclip-paste :before (lambda ()
                                        (let ((paste-length (length (simpleclip-get-contents))))
                                          (when (> paste-length 0)
                                            (setq slegetank/simpleclip-last-paste-region (cons (point) (length (simpleclip-get-contents))))))))

(defun slegetank/current-kill-advice-function (count &optional register yank-handler)
  "Before paste, store the last paste position info."
  (let ((paste-length (length (current-kill 0))))
    (when (> paste-length 0)
      (setq slegetank/simpleclip-last-paste-region (cons (point) (length (current-kill 0)))))
    ))

(when (fboundp 'evil-paste-after)
  (advice-add 'evil-paste-before :before 'slegetank/current-kill-advice-function)
  (advice-add 'evil-paste-after :before 'slegetank/current-kill-advice-function))

;; (defun slegetank/simpleclip-select-paste ()
;;   "Select the last paste string. Should call this ASAP after paste operation."
;;   (interactive)
;;   (when (and slegetank/simpleclip-last-paste-region
;;              (consp slegetank/simpleclip-last-paste-region))
;;     (let* ((p1 (car slegetank/simpleclip-last-paste-region))
;;            (p2 (+ p1 (cdr slegetank/simpleclip-last-paste-region))))
;;       (goto-char p1)
;;       (push-mark p2)
;;       (setq mark-active t))))

;; (slegetank/leader-define-key "gp" 'slegetank/simpleclip-select-paste "Select last paste word")
;; (evil-define-key 'normal global-map (kbd "gp") 'slegetank/simpleclip-select-paste)

(setq imenu-list-focus-after-activation t)
(setq imenu-list-auto-resize t)
(setq imenu-list-idle-update-delay-time 0.1)

(evil-define-key 'normal imenu-list-major-mode-map (kbd "v") 'imenu-list-display-entry)
(evil-define-key 'normal imenu-list-major-mode-map (kbd "<tab>") 'hs-toggle-hiding)
(defun slegetank/imenu-goto-and-quit ()
  (interactive)
  (with-current-buffer (current-buffer)
    (imenu-list-goto-entry)
    (imenu-list-quit-window)))

(global-set-key (kbd "s-i") 'imenu-list-smart-toggle)

(evil-define-key 'normal imenu-list-major-mode-map (kbd "<return>") 'slegetank/imenu-goto-and-quit)
(evil-define-key 'normal imenu-list-major-mode-map (kbd "q") 'imenu-list-quit-window)
(evil-define-key 'normal imenu-list-major-mode-map (kbd "g") 'imenu-list-refresh)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

(require 'xah-get-thing)

(require 'protobuf-mode)

(defconst my-protobuf-style
  '((c-basic-offset . 4)
    (indent-tabs-mode . nil)))

(add-hook 'protobuf-mode-hook
          (lambda () (c-add-style "my-style" my-protobuf-style t)))

;; (require 'quickrun)
;; (defun slegetank/quickrun ()
;;   "Custom quickrun command"
;;   (interactive) 
;;   (if (region-active-p)
;;       (call-interactively 'quickrun-region)
;;     (call-interactively 'quickrun)))

;; (global-set-key (kbd "s-r") 'slegetank/quickrun)
;; (evil-define-key 'normal quickrun--mode-map (kbd "q") 'quit-window)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
