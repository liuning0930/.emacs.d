(setq slegetank/cloud-dir (expand-file-name "~/work/cloud/cloud"))
(unless (file-exists-p slegetank/cloud-dir)
  (setq slegetank/cloud-dir (expand-file-name "~")))
