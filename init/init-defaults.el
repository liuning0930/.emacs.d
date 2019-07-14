(when (equal system-type 'darwin)
  (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
    (when (file-exists-p default-directory)
      (normal-top-level-add-subdirs-to-load-path))))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; not good choice
;; (setq make-backup-files nil)

;; (desktop-save-mode 1)
;; (setq history-length 20)

;; (global-display-line-numbers-mode 1)
;; (setq display-line-numbers-grow-only t)
;; (setq display-line-numbers-type 'visual)

(global-linum-mode 0)
;; ;; 暂时显示行号
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unless (symbol-value global-linum-mode)
    (unwind-protect
        (progn
          (global-linum-mode 1)
          (goto-line (read-number "Goto line: ")))
      (global-linum-mode -1))))

;; title show full path
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; 高亮当前行
(global-hl-line-mode 1)

;; 选中删除
(delete-selection-mode t)

;; max size
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; 更改光标的样式
(setq-default cursor-type 'bar)

;; 关闭启动帮助画面
(setq inhibit-splash-screen 1)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
;; (setq initial-buffer-choice "~/")

;; 更改显示字体大小 16pt
;; http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
(set-face-attribute 'default nil :height 140)

(setq ring-bell-function 'ignore)

;; 更好的滚动
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; yes/no -> y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; narrow enable
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(setq isearch-forward t)

(setq blink-cursor-interval 0.4)

;; modeline显示列号
(setq column-number-mode t)

;; auto-revert
(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(add-hook 'after-init-hook 'global-prettify-symbols-mode)

;; 几个从陈斌那里拿过来的配置
(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq indicate-empty-lines t)

(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(setq goto-address-mail-face 'link)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'after-save-hook 'sanityinc/set-mode-for-new-scripts)

(defun sanityinc/set-mode-for-new-scripts ()
  "Invoke `normal-mode' if this file is a script and in `fundamental-mode'."
  (and
   (eq major-mode 'fundamental-mode)
   (>= (buffer-size) 2)
   (save-restriction
     (widen)
     (string= "#!" (buffer-substring (point-min) (+ 2 (point-min)))))
   (normal-mode)))

(setq scroll-preserve-screen-position t)

;; if use no simpleclip, then use isearch-yank-pop
(define-key isearch-mode-map (kbd "s-v") 'custom-isearch-yank-pop)

(defun custom-isearch-yank-pop ()
  "For paste in minibuffer isearch"
  (interactive)
  (deactivate-mark)
  (isearch-push-state)
  (isearch-yank-string (simpleclip-get-contents)))

  (defun custom-isearch-with-region ()
    "Use region as the isearch text."
    (when mark-active
      (let ((region (filter-buffer-substring (region-beginning) (1+ (region-end)))))
        (deactivate-mark)
        (isearch-push-state)
        (isearch-yank-string region))))

  (add-hook 'isearch-mode-hook 'custom-isearch-with-region)

(require 'server)
(unless (server-running-p) (server-start))

(require-package 'highlight-parentheses)
(global-highlight-parentheses-mode)

(defalias 'insert-string 'insert)

(require 'recentf)
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)

(setq-default
 recentf-max-saved-items 1000
 recentf-exclude '("/tmp/" "/ssh:"))

(defsubst file-was-visible-p (file)
  "Return non-nil if FILE's buffer exists and has been displayed."
  (let ((buf (find-buffer-visiting file)))
    (if buf
        (let ((display-count (buffer-local-value 'buffer-display-count buf)))
          (if (> display-count 0) display-count nil)))))

(let ((r-list recentf-list))
  (defsubst keep-default-old-and-visible-recentf-p (file)
    "Decide whether to keep file in recentf-list.
    Return non-nil if recentf would, by default, keep FILE, and
    either FILE name was loaded from recentf file on disk or FILE
    has been displayed in this session."
    (if (recentf-keep-default-predicate file)
        (or (member file r-list)
            (file-was-visible-p file)))))

(setf recentf-keep '(keep-default-old-and-visible-recentf-p))

(require-package 'better-defaults)
(require 'better-defaults)

(defun sanityinc/utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match "UTF-8" v)))

(defun sanityinc/locale-is-utf8-p ()
  "Return t iff the \"locale\" command or environment variables prefer UTF-8."
  (or (sanityinc/utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (sanityinc/utf8-locale-p (getenv "LC_ALL"))
      (sanityinc/utf8-locale-p (getenv "LC_CTYPE"))
      (sanityinc/utf8-locale-p (getenv "LANG"))))

(when (or window-system (sanityinc/locale-is-utf8-p))
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))
  (prefer-coding-system 'utf-8))

(defun slegetank/thing-at-point ()
  "Better thing at point"
  (if (fboundp 'xah-get-thing-at-point)
      (xah-get-thing-at-point 'word)
    (thing-at-point 'word)))

(require 'tramp)
(tramp-set-completion-function "sshx"
                               '((tramp-parse-sconfig "/etc/ssh_config")
                               (tramp-parse-sconfig "~/.ssh/config")))

(setq tramp-default-method "sshx")

(defun slegetank/fix-tramp-adb-get-ls-command (orig-fun &rest args)
  "Fix `tramp-adb-get-ls-command' for send ls command"
  (let ((command (apply orig-fun args)))
    (if (equal command "ls --color=never")
        "ls -1 --color=never"
      command)))

(advice-add 'tramp-adb-get-ls-command :around #'slegetank/fix-tramp-adb-get-ls-command)

(defun slegetank/fix-tramp-adb-send-command (orig-fun &rest args)
  "Fix `tramp-adb-send-command' for parse ls -l"
  (let* ((vec (car args))
         (command (cadr args))
         result)
    (when (s-contains? "-l" command)
      (setq command (s-replace "-1" "-e" command)))
    (setq result (apply orig-fun `(,vec ,command)))
    (with-current-buffer (tramp-get-connection-buffer vec)
      (save-excursion
        ;; remove \n in command if exist
        (goto-char (point-min))
        (let* ((first-line-end (search-forward "\n" nil :noerror))
               (first-line (buffer-substring-no-properties 1 (- first-line-end 1))))
          (when (and (> (length first-line) 2) (s-starts-with? first-line command))
            (replace-match "")))

        ;; remove command if exist
        (goto-char (point-min))
        (when (search-forward (format "%s\n" command) nil :noerror)
          (replace-match ""))

        ;; format ls result to Android
        (goto-char (point-min))
        (when (re-search-forward slegetank/tramp-adb-ls-toolbox-regexp nil t)
          (let* ((mod-string (match-string 1))
                 (is-dir (eq ?d (aref mod-string 0)))
                 (links (match-string 2))
                 (uid (match-string 3))
                 (gid (match-string 4))
                 (size (string-to-number (match-string 5)))
                 (date (format-time-string "%Y-%m-%d %H:%M" (date-to-time (match-string 6))))
                 (name (match-string 7)))
            (replace-match (format "%s %s %s %s %s %s %s" mod-string links uid gid size date name))))

        ;; advice -a -1 result
        (when (and (s-contains? "-a" command) (not (s-starts-with? "\n" (buffer-string))))
          (goto-char (point-min))
          (insert "\n"))
        result))))

(advice-add 'tramp-adb-send-command :around #'slegetank/fix-tramp-adb-send-command)

(defun slegetank/fix-tramp-do-parse-file-attributes-with-ls (orig-fun &rest args)
  "Fix `tramp-do-parse-file-attributes-with-ls' for parse Linux ls -a"
  (let ((vec (car args)))
    (with-current-buffer (tramp-get-buffer vec)
      (goto-char (point-min))
      (while (re-search-forward slegetank/tramp-adb-ls-toolbox-regexp nil t)
        (let* ((mod-string (match-string 1))
               (is-dir (eq ?d (aref mod-string 0)))
               (links (match-string 2))
               (uid (match-string 3))
               (gid (match-string 4))
               (size (string-to-number (match-string 5)))
               (date (format-time-string "%Y-%m-%d %H:%M" (date-to-time (match-string 6))))
               (name (match-string 7)))
          (replace-match (format "%s %s %s %s %s %s %s" mod-string links uid gid size date name))))))
  (apply orig-fun args))

(advice-add 'tramp-do-parse-file-attributes-with-ls :around #'slegetank/fix-tramp-do-parse-file-attributes-with-ls)

(defconst slegetank/tramp-adb-ls-toolbox-regexp
  (concat
   "^[[:space:]]*\\([-[:alpha:]]+\\)" 	; \1 permissions
   "[[:space:]]+\\([[:digit:]]+\\)"	        ; \2 links (Android 7/toybox)
   "[[:space:]]*\\([^[:space:]]+\\)"	; \3 username
   "[[:space:]]+\\([^[:space:]]+\\)"	; \4 group
   "[[:space:]]+\\([[:digit:]]+\\)"	        ; \5 size
   "[[:space:]]+\\([[:alpha:]]+[[:space:]][[:alpha:]]+[[:space:]]+[[:digit:]]+[[:space:]][:[:digit:]]+[[:space:]][[:digit:]]+\\)" ; \6 date
   "[[:space:]]\\(.*\\)$")		; \7 filename
  "Regexp for ls output.")

;;(add-to-list 'load-path (expand-file-name "bookmark-plus" user-emacs-directory))
;;(require 'bookmark+)

(defun slegetank/clear-message-buffer ()
  "Clear message buffer."
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and (get-buffer-window name 'visible)
                 (string-match "*Messages*" name))
        (with-current-buffer buffer
          (read-only-mode -1)
          (erase-buffer)
          (read-only-mode +1))))))

(defun custom-writeCurrentDirToCahceFile ()
  (with-temp-file  (concat user-emacs-directory  "currentDir") (insert (expand-file-name (directory-file-name default-directory)))))
  (add-hook 'focus-out-hook 'custom-writeCurrentDirToCahceFile)

(setq scheme-program-name   "/usr/local/bin/mit-scheme")

(require-package 'mode-line-bell)
(add-hook 'after-init-hook 'mode-line-bell-mode)

(require-package 'default-text-scale)
(require 'default-text-scale)

(add-hook 'after-init-hook 'default-text-scale-mode)

(define-key default-text-scale-mode-map (kbd "s-=") 'default-text-scale-increase)
(define-key default-text-scale-mode-map (kbd "s--") 'default-text-scale-decrease)
(define-key default-text-scale-mode-map (kbd "s-0") 'default-text-scale-reset)

(require-package 'exec-path-from-shell)
(when (equal system-type 'darwin)
  (exec-path-from-shell-initialize))

(require-package 's)
(require 's)

(global-set-key (kbd "s-k") nil)

;; help
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-f") 'find-function)

(defadvice find-function-do-it (around add-find-function-mark activate)
  "Find function/variable/key pop back"
  (xref-push-marker-stack)
  ad-do-it)

;; replace eval command from alt-x
(global-set-key (kbd "C-x C-m") 'counsel-M-x)

(defun clean-message-buffer ()
  "Fast way to clean message buffer's output"
  (interactive)
  (let ((messagebuffer (get-buffer "*Messages*")))
    (when messagebuffer
      (kill-buffer "*Messages*"))
    (view-echo-area-messages)))

(global-set-key (kbd "C-c m c") 'clean-message-buffer)

;; ace -> mark-ring -> xref
(defun slegetank/go-back ()
  (interactive)
  (if (and (boundp 'ace-jump-mode-mark-ring) (> (length ace-jump-mode-mark-ring) 0))
      (progn (ace-jump-mode-pop-mark)
             (if (= 1 (length ace-jump-mode-mark-ring))
                 (setq ace-jump-mode-mark-ring nil)
               (nbutlast ace-jump-mode-mark-ring 1)))
    (if (and (number-or-marker-p (car mark-ring)))
        (progn (set-mark-command (car mark-ring))
               (if (= 1 (length mark-ring))
                   (setq mark-ring nil)
                 (progn (delete-duplicates mark-ring :test 'equal)
                        (setq mark-ring (cdr mark-ring)))))
      (xref-pop-marker-stack))))

;; (global-set-key (kbd "s-,") 'slegetank/go-back)
