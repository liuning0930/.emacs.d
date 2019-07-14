(require-package 'pyim)
(require-package 'posframe)
(require 'posframe)
(require 'pyim)
;; (require 'pyim-basedict)
;; (pyim-basedict-enable)

;; (require-package 'pyim-wbdict)
;; (require 'pyim-wbdict)
;; (pyim-wbdict-gb2312-enable)

(setq default-input-method "pyim")
;; (setq pyim-default-scheme 'wubi)
;; (setq pyim-default-scheme 'quanpin)

;; 使用 pupup-el 来绘制选词框
(if (>= emacs-major-version 26)
    (setq pyim-page-tooltip 'child-frame)
  (setq pyim-page-tooltip 'popup))

;; 选词框显示10个候选词
(setq pyim-page-length 5)

(setq-default pyim-english-input-switch-functions
              '(pyim-probe-org-structure-template))

(setq-default pyim-punctuation-half-width-functions
              '(pyim-probe-punctuation-line-beginning
                pyim-probe-punctuation-after-punctuation))

(global-set-key (kbd "C-s-;") 'pyim-convert-string-at-point)
(global-set-key (kbd "C-;") 'toggle-input-method)
(define-key pyim-mode-map (kbd "<tab>") 'pyim-page-next-page)
(define-key pyim-mode-map (kbd "C-j") 'pyim-page-next-page)
(define-key pyim-mode-map (kbd "C-k") 'pyim-page-previous-page)
(define-key pyim-mode-map (kbd "<escape>") 'pyim-quit-clear)

;; 特殊词上屏
(defun slegetank/special-words-converter (string)
  (if (equal string "物物")
      "IoT"
    string))
(setq pyim-magic-converter #'slegetank/special-words-converter)

(setq pyim-dicts
      `((:name "big" :file ,(expand-file-name "pyim/pyim-bigdict.pyim" user-emacs-directory))
        (:name "sougou-develop" :file ,(expand-file-name "pyim/sougou-develop.pyim" user-emacs-directory))
        (:name "sougou-jindaishi" :file ,(expand-file-name "pyim/sougou-jindaishi.pyim" user-emacs-directory))))

(when (file-exists-p (file-truename "~/ScriptHelper/liberime.so"))
  (setq load-path (cons (file-truename "~/ScriptHelper") load-path))
  (require 'liberime)
  (liberime-start "/Library/Input Methods/Squirrel.app/Contents/SharedSupport" (file-truename "~/.emacs.d/pyim/rime/"))
  (liberime-select-schema "luna_pinyin_simp")
  (setq pyim-default-scheme 'rime-quanpin))

(with-eval-after-load "evil"
  (require-package 'evil-find-char-pinyin)
  (require 'evil-find-char-pinyin)
  (evil-find-char-pinyin-mode +1))

(with-eval-after-load "swiper"
  (require-package 'pinyinlib)
  (defun re-builder-pinyin (str)
    (or (pinyin-to-utf8 str)
        (ivy--regex-plus str)
        (ivy--regex-ignore-order str)))

  (setq ivy-re-builders-alist
        '((t . re-builder-pinyin)))

  (defun my-pinyinlib-build-regexp-string (str)
    (cond ((equal str ".*")
           ".*")
          (t
           (pinyinlib-build-regexp-string str t))))

  (defun my-pinyin-regexp-helper (str)
    (cond ((equal str " ")
           ".*")
          ((equal str "")
           nil)
          (t
           str)))

  (defun pinyin-to-utf8 (str)
    (cond ((equal 0 (length str))
           nil)
          ((equal (substring str 0 1) "!")
           (mapconcat 'my-pinyinlib-build-regexp-string
                      (remove nil (mapcar 'my-pinyin-regexp-helper
                                          (split-string
                                           (replace-regexp-in-string "!" "" str ) "")))
                      ""))
          (t
           nil))))

(require-package 'ace-pinyin)
(require 'ace-pinyin)
(setq ace-pinyin-use-avy t)
(ace-pinyin-global-mode +1)

(require-package 'cnfonts)
(require 'cnfonts)
(cnfonts-enable)
(cnfonts-set-spacemacs-fallback-fonts)

(require-package  'youdao-dictionary)
(setq url-automatic-caching t)

(with-eval-after-load "popwin"
  (push "*Youdao Dictionary*" popwin:special-display-config))

(if (boundp 'slegetank/cloud-dir)
    (setq youdao-dictionary-search-history-file (expand-file-name ".youdao" slegetank/cloud-dir))
  (setq youdao-dictionary-search-history-file "~/.emacs.d/.youdao"))

(defun slegetank/youdao-search (word)
  (interactive (list (let* ((guessword (slegetank/guess-input))
                            (readword (read-string (format "Word(%s): " guessword) nil 'youdao-dictionary-history)))
                       (if (> (length readword) 0)
                           readword
                         guessword))))
  (if (> (length word) 0)
      (youdao-dictionary-search word)
    (message "Input empyt. About.")))

(slegetank/leader-define-key "hy" 'slegetank/youdao-search "Yudao")
