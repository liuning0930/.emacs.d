(defun slegetank/linkplay-muzo-project-path ()
  (let* ((currentDir (eshell/pwd))
         (index (s-index-of "ios_muzoplayer" currentDir)))
    (if index
        (substring currentDir 0 (+ index (length "ios_muzoplayer")))
      nil)))

(defun slegetank/linkplay-muzo (&rest params)
  (let ((tPath (slegetank/linkplay-muzo-project-path)))
    (if tPath
        (progn
          (goto-char (point-max))
          (insert (format "python3 %s/MuzoManager/MuzoManager.py %s" tPath (s-join " " (car params))))
          (run-with-timer 0.1 nil 'eshell-send-input))
      (eshell/echo "Not in muzo project."))))

(defconst pcmpl-muzo-commands
  '(-s -p -h -i -c)
  "List of `muzo' commands")

(defun pcomplete/muzo ()
  "Completion for `muzo'"
  (pcomplete-here* pcmpl-muzo-commands)

  (when (pcomplete-match (regexp-opt '("-s" "-p")) 1)
    (pcomplete-here (slegetank/linkplay-muzo-complete))))

(defun slegetank/linkplay-muzo-complete ()
  "Scan clients dir"
  (let ((tPath (format "%s/muzodev/Clients/" (slegetank/linkplay-muzo-project-path))))
    (let* ((files (cons nil (directory-files tPath)))
           (parent files)
           (current (cdr files))
           (exclude (list "." ".." ".DS_Store" "projects.plist"))
           (file nil))
      (while (and current exclude)
        (setq file (car current))
        (if (not (member file exclude))
            (setq parent current)
          (setcdr parent (cdr current))
          (setq exclude (delete file exclude)))
        (setq current (cdr current)))
      (cdr files))))

(let ((workel (format "%s/%s" slegetank/cloud-dir "init-work.el")))
  (when (file-exists-p workel)
    (load workel)))
