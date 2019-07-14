(require 'dired-x)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-dwim-target t)

(put 'dired-find-alternate-file 'disabled nil)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))))  ; was dired-up-directory)

(setq dired-omit-files-p t)
(setq dired-omit-verbose nil)
(setq dired-omit-files
      (concat dired-omit-files "\\.pyc$\\|.DS_Store"))

(defun recentd-track-opened-file ()
  "Insert the name of the directory just opened into the recent list."
  (and (derived-mode-p 'dired-mode)
       (not (bound-and-true-p tramp-mode))
       default-directory
       (recentf-add-file default-directory))
  ;; Must return nil because it is run from `write-file-functions'.
  nil)


(defun recentd-track-closed-file ()
  "Update the recent list when a dired buffer is killed.
  That is, remove a non kept dired from the recent list."
  (and (derived-mode-p 'dired-mode)
       default-directory
       (recentf-remove-if-non-kept default-directory)))

(add-hook 'dired-after-readin-hook 'recentd-track-opened-file)
(add-hook 'kill-buffer-hook 'recentd-track-closed-file)

;; (require-package 'dired+)
(add-to-list 'load-path (expand-file-name "dired+" user-emacs-directory))
(require 'dired+)

;; only use one buffer
(diredp-toggle-find-file-reuse-dir 1)

(require-package 'dired-filetype-face)
(require 'dired-filetype-face)

(require-package 'stripe-buffer)
(add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)

;; (require-package 'ivy-dired-history)
;; (require 'savehist)
;; (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable)
;; (savehist-mode 1)

;; (with-eval-after-load 'dired
;;   (require 'ivy-dired-history)
;;   ;; if you are using ido,you'd better disable ido for dired
;;   ;; (define-key (cdr ido-minor-mode-map-entry) [remap dired] nil) ;in ido-setup-hook
;;   (define-key dired-mode-map "," 'dired))

(require-package 'diredfl)
(with-eval-after-load 'dired (diredfl-global-mode))

(require-package 'diff-hl)
(with-eval-after-load 'dired (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(defun custom-dired-open-file-with-osx ()
  "In Dired, try to open files by osx"
  (interactive)
  (shell-command (format "open \"%s\"" (dired-get-file-for-visit))))

(defun custom-dired-temp-hide-files ()
  "Hide/show marked files temporary."
  (interactive)
  (if dired-omit-files-p
      (setq dired-omit-files-p nil)
    (progn
      (make-local-variable 'dired-omit-files)
      (dolist (item (dired-get-marked-files))
        (setq dired-omit-files
              (concat dired-omit-files (format "\\|%s" (file-name-nondirectory item)))))
      (setq dired-omit-files-p t)))
  (dired-unmark-all-marks)
  (revert-buffer))

;; (add-hook 'view-mode-hook '(lambda () (evil-define-key 'normal view-mode-map (kbd "q") 'View-quit)))

(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-verbosity nil)

(defun custom-dired-sort-dir-first ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))

(add-hook 'dired-after-readin-hook 'custom-dired-sort-dir-first)

(setq custom-dired-sort-seq-list `((,(concat dired-listing-switches "h") . "Name")
                                   (,(concat dired-listing-switches "cth") . "Create Time")
                                   (,(concat dired-listing-switches "uth") . "Modify Time")
                                   (,(concat dired-listing-switches "Xh") . "Extension")
                                   (,(concat dired-listing-switches "Sh") . "Size")))

(defun custom-dired-sort-next ()
  "In dired mode, sort"
  (interactive)
  (let ((nextpos (1+ (or (cl-position
                          (assoc dired-actual-switches custom-dired-sort-seq-list)
                          custom-dired-sort-seq-list :test 'equal) 0))))
    (when (= (length custom-dired-sort-seq-list) nextpos)
      (setq nextpos 0))
    (dired-sort-other (car (car (nthcdr nextpos custom-dired-sort-seq-list))))
    (message "SORTBY - \"%s\"" (cdr (car (nthcdr nextpos custom-dired-sort-seq-list))))))

(evil-define-key '(normal motion)
  dired-mode-map (kbd "e") 'wdired-change-to-wdired-mode
  (kbd "H") 'custom-dired-temp-hide-files
  (kbd "O") 'custom-dired-open-file-with-osx
  (kbd "s") 'custom-dired-sort-next)

(defun slegetank/recent-directories ()
  "Return recent access directories."
  (delete-dups
   (append
    (delq nil (mapcar (lambda (filename) ; recentf
                        (let ((directory (file-name-directory filename)))
                          (and (not (file-remote-p directory)) (file-exists-p directory) directory)))
                      recentf-list))
    (delq nil (mapcar (lambda (directory) ; finder recent
                        (and (file-exists-p directory) (concat directory "/")))
                      (split-string (shell-command-to-string (format "python %s/init/osx-recent-dir.py" user-emacs-directory)) "\n" t)))
    (when (file-exists-p "~/.z") ; append lines from z; append top 20 dirs
      (mapcar (lambda (directory)
                (and (file-exists-p directory) (concat directory "/")))
              (split-string (shell-command-to-string "cat ~/.z | sort -r -n -k 2 -t \"|\" | cut -f 1 -d \"|\" | head -n 20") "\n" t))))))

;; (defun slegetank/dired-do-rename (&optional arg)
;;   "Modify `dired-do-rename'"
;;   (interactive "P")
;;   (slegetank/dired-do-create-files 'move #'dired-rename-file
;;                          "Move" arg dired-keep-marker-rename "Rename"))

(defun slegetank/dired-do-copy (&optional arg)
  "Modify `dired-do-copy'"
  (interactive "P")
  (let ((dired-recursive-copies dired-recursive-copies))
    (slegetank/dired-do-create-files 'copy #'dired-copy-file
                           "Copy"
                           arg dired-keep-marker-copy
                           nil dired-copy-how-to-fn)))

(defun slegetank/dired-do-create-files (op-symbol file-creator operation arg
                                                  &optional marker-char op1
                                                  how-to)
  "Modify `dired-do-create-files'"
  (or op1 (setq op1 operation))
  (let* ((fn-list (dired-get-marked-files nil arg))
         (rfn-list (mapcar #'dired-make-relative fn-list))
         (dired-one-file	; fluid variable inside dired-create-files
          (and (consp fn-list) (null (cdr fn-list)) (car fn-list)))
         (target-dir (dired-dwim-target-directory))
         (default (and dired-one-file
                       (not dired-dwim-target) ; Bug#25609
                       (expand-file-name (file-name-nondirectory (car fn-list))
                                         target-dir)))
         (defaults (dired-dwim-target-defaults fn-list target-dir))
         ;; use ivy
         (cands (slegetank/recent-directories))
         (target (and cands (ivy-read (format (concat (if dired-one-file op1 operation) " %s to: ")
                                              (dired-mark-prompt arg rfn-list) op-symbol arg) cands
                                              :require-match nil)))
         (into-dir (cond ((null how-to)
                          (if (and (file-name-case-insensitive-p (car fn-list))
                                   (eq op-symbol 'move)
                                   dired-one-file
                                   (string= (downcase
                                             (expand-file-name (car fn-list)))
                                            (downcase
                                             (expand-file-name target)))
                                   (not (string=
                                         (file-name-nondirectory (car fn-list))
                                         (file-name-nondirectory target))))
                              nil
                            (file-directory-p target)))
                         ((eq how-to t) nil)
                         (t (funcall how-to target)))))
    (if (and (consp into-dir) (functionp (car into-dir)))
        (apply (car into-dir) operation rfn-list fn-list target (cdr into-dir))
      (if (not (or dired-one-file into-dir))
          (error "Marked %s: target must be a directory: %s" operation target))
      ;; rename-file bombs when moving directories unless we do this:
      (or into-dir (setq target (directory-file-name target)))
      (dired-create-files
       file-creator operation fn-list
       (if into-dir			; target is a directory
           ;; This function uses fluid variable target when called
           ;; inside dired-create-files:
           (lambda (from)
             (expand-file-name (file-name-nondirectory from) target))
         (lambda (_from) target))
       marker-char))))

;; (define-key dired-mode-map (kbd "C") 'slegetank/dired-do-copy)
;; (define-key dired-mode-map (kbd "R") 'slegetank/dired-do-rename)

(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

(require-package 'ivy)
(require 'ivy) ; swiper 7.0+ should be installed

(defun custom-goto-recent-directory ()
  "Open recent directory with dired; add z cmd & MacOS recent dir list to this"
  (interactive)
  (unless recentf-mode (recentf-mode 1))
  (let ((collection (slegetank/recent-directories)))
    (ivy-read "directories:" collection :action 'dired)))

(defun custom-goto-finder-directory ()
  "Open OSX Finder path with dired"
  (interactive)
  (dired (file-name-as-directory (s-trim (shell-command-to-string "osascript -e \'tell app \"Finder\" to POSIX path of (insertion location as alias)\'")))))

(defun slegetank/goto-xcode-project ()
  "Open current XCode project path with dired"
  (interactive)
  (dired (file-name-as-directory (file-name-directory (s-trim (shell-command-to-string "osascript -e 'tell application id \"com.apple.dt.Xcode\" to return path of document 1'"))))))


(defun slegetank/goto-android-project ()
  "Open current XCode project path with magit"
  (interactive)
  (let ((cmd-result (shell-command-to-string "osascript -e 'tell application \"System Events\" to get the {title} of window 1 of process \"Android Studio\"'")))
    (if cmd-result
        (dired (file-name-as-directory (file-name-directory (s-chop-suffix "]"
                                                                           (s-chop-prefix "["
                                                                                          (car
                                                                                           (s-match "\\[.*?\\]" (s-trim cmd-result))))))))
      (message "Found no android project."))))

(defun custom-open-xcode-file ()
  "Open current XCode editing file with emacs"
  (interactive)
  (find-file (s-trim (shell-command-to-string "osascript -e 'tell application id \"com.apple.dt.Xcode\" to return path of last item of source documents'"))))

(require 'bookmark)
(defun custom-ido-bookmark-jump ()
  "Jump to bookmark using ido"
  (interactive)
  (let ((dir (custom-ido-get-bookmark-dir)))
    (when dir
      (find-alternate-file dir))))

(defun custom-ido-get-bookmark-dir ()
  "Get the directory of a bookmark."
  (let* ((name (ido-completing-read "Use dir of bookmark: " (bookmark-all-names) nil t))
         (bmk (bookmark-get-bookmark name)))
    (when bmk
      (setq bookmark-alist (delete bmk bookmark-alist))
      (push bmk bookmark-alist)
      (let ((filename (bookmark-get-filename bmk)))
        (if (file-directory-p filename)
            filename
          (file-name-directory filename))))))

(slegetank/leader-define-key
 "d" nil "Dired"
  "dd" 'dired-jump "Current dir"
  "do" 'dired-jump-other-window "dd in other window"
  "dr" 'custom-goto-recent-directory "Recent dirs"
  "df" 'custom-goto-finder-directory "Dired Finder"
  "dx" 'slegetank/goto-xcode-project "Xcode proj dir"
  "da" 'slegetank/goto-android-project "Android proj dir"
  "fx" 'custom-open-xcode-file "Xcode file"
  "dm" 'custom-ido-bookmark-jump "Dired bookmark")
