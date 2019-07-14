(when (>= emacs-major-version 26)
  ;; (pixel-scroll-mode)
  (when (equal system-type 'darwin)
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . dark))))

(when (equal system-type 'darwin)
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control)))))

(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(setq custom-safe-themes t)
(require-package 'afternoon-theme)
(require-package 'monokai-theme)
(require-package 'ujelly-theme)
(require-package 'sublime-themes)
(require-package 'organic-green-theme)
(require-package 'zenburn-theme)
(require-package 'metalheart-theme)
(require-package 'color-theme-sanityinc-tomorrow)
(require-package 'doom-themes)
;; (load-theme 'afternoon t)

;; (slegetank/leader-define-key "tu" 'customize-themes "Custom theme")
(require-package 'material-theme)
(load-theme 'material t)

;; (require-package 'all-the-icons)
;; (require 'all-the-icons)
;; (require 'doom-themes)

;; ;; Global settings (defaults)
;; (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;       doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; ;; may have their own settings.
;; (load-theme 'doom-one t)

;; ;; Enable flashing mode-line on errors
;; (doom-themes-visual-bell-config)

;; ;; Enable custom neotree theme
;; (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

;; ;; Corrects (and improves) org-mode's native fontification.
;; (doom-themes-org-config)

;; ;; (require-package 'doom-modeline)
;; ;; (require 'doom-modeline)
;; ;; (doom-modeline-init)

;; (require-package 'smart-mode-line)
;; (require 'smart-mode-line)
;; (setq sml/no-confirm-load-theme t)
;; (sml/setup)
