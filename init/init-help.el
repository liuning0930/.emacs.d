(setq custom-help-local-dir "/Users/hy/Work/github/MyBlogOrg/blog/")
(setq custom-help-remote-url "http://blog.slegetank.com/article/%s?blogtype=%s")


(setq custom-help-article-list '(("blog" "20170108-blogWithEmacs.org" "Emacs")
                                 ("dired" "20170106-dired.org" "Emacs")
                                 ("tramp" "20170318-tramp.org" "Emacs")
                                 ("magit" "20170206-magit.org" "Emacs")
                                 ("s" "https://github.com/magnars/s.el" "Emacs")
                                 ("RN-github" "https://github.com/jondot/awesome-react-native" "RN")
                                 ("flex" "https://css-tricks.com/snippets/css/a-guide-to-flexbox/" "react")
                                 ("emmet" "https://github.com/smihica/emmet-mode" "js")
                                 ("material-ui" "https://v0.material-ui.com/#/" "react")
                                 ("oc block" "http://blog.slegetank.com/article/20160721-block.org?blogtype=iOS")
                                 ))

(defun custom-help-open-article (remote)
  "Open help article for some topic"
  (interactive)
  (let* ((info (ivy-completing-read (if (equal remote "remote") "Open remote topic: " "Open local topic: ") custom-help-article-list nil t))
         (name (car (cdr (assoc info custom-help-article-list))))
         (type (car (nthcdr 2 (assoc info custom-help-article-list)))))
    (if (s-starts-with? "http" name)
        (browse-url name)
      (if (equal remote "remote")
          (let ((remoteurl (format custom-help-remote-url name type)))
            (browse-url remoteurl))
        (if (file-exists-p custom-help-local-dir)
            (progn (shell-command (format "cd %s && git fetch && git rebase" custom-help-local-dir))
                   (find-file (concat custom-help-local-dir name)))
          (message (format "Found no help article! %s" custom-help-article-list)))))))

(defun custom-help-open-local-article ()
  "Open local help article"
  (interactive)
  (custom-help-open-article "local"))

(defun custom-help-open-remote-article ()
  "Open remote help article"
  (interactive)
  (custom-help-open-article "remote"))

(require-package 'melpa-upstream-visit)

(defun slegetank/melpa-upstream-visit ()
  "Though muv has already supplied thing-at-point, but there's a problem for the listp code in org-mode: `thing-at-point in org returns diff from in el. So I have to do it myself for better exp."
  (interactive)
  (when (< (length package-archive-contents) 1000)
    (package-refresh-contents))
  (let* ((packages (mapcar (lambda (el)
                             (format "%-30s %s" (symbol-name (car el)) (package-desc-summary (cadr el))))
                           package-archive-contents))
         (p-at-point (slegetank/thing-at-point))

         (package-name (ivy-completing-read "Access home of package: " packages nil t p-at-point)))
    (muv (car (s-split " " package-name t)))))

(slegetank/leader-define-key "hp" 'slegetank/melpa-upstream-visit "Find package's homepage")

(require-package 'command-log-mode)
(use-package command-log-mode
  :commands global-command-log-mode
  :init
  (slegetank/leader-define-key "hc" 'global-command-log-mode "Show keystroke realtime")
  :config
  (setq clm/log-command-exceptions* (append clm/log-command-exceptions*
                                            '(evil-next-line
                                              evil-previous-line
                                              evil-forward-char
                                              evil-backward-char
                                              ivy-next-line
                                              ivy-previous-line))
        command-log-mode-auto-show t))

(require-package 'devdocs)
(slegetank/leader-define-key "hd" 'devdocs-search "dev docs")

(setq slegetank/last-search-engine 'google)
(setq slegetank/search-engine-history-list nil)

(defun slegetank/search-web (engine key)
  (setq slegetank/last-search-engine engine)
  (browse-url
   (cond ((equal engine 'google)
          (format "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s" (url-hexify-string key)))
         ((equal engine 'baidu)
          (format "https://www.baidu.com/s?wd=%s" (url-hexify-string key)))
         ((equal engine 'symbolhound)
          (format "http://symbolhound.com/?q=%s" (url-hexify-string key))))))



(defun slegetank/search-google (key)
  (interactive "sGoogle: ")
  (slegetank/search-web 'google key))

(defun slegetank/search-symbolhound (key)
  (interactive "sSymbolhound: ")
  (slegetank/search-web 'symbolhound key))

(defun slegetank/search-baidu (key)
  (interactive "s百度: ")
  (slegetank/search-web 'baidu key))

(defun slegetank/search-with-last (key)
  (interactive (list (let* ((guessword (slegetank/guess-input))
                            (readword (read-string (format "%s(%s): " slegetank/last-search-engine guessword) nil slegetank/search-engine-history-list)))
                       (if (> (length readword) 0)
                           readword
                         guessword))))
  (slegetank/search-web slegetank/last-search-engine key))

(slegetank/leader-define-key
                             "hs" nil "Search web"
                             "hsg" 'slegetank/search-google "Search google"
                             "hss" 'slegetank/search-symbolhound "Search symbolhound"
                             "hsb" 'slegetank/search-baidu "Search 百度"
                             (format "hs%s" (kbd "RET")) 'slegetank/search-with-last (format "Search %s" slegetank/last-search-engine))

(slegetank/leader-define-key "h" nil "Help"
                             "hl" 'custom-help-open-local-article "Local help"
                             "hr" 'custom-help-open-remote-article "Remote help")
