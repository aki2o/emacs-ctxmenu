(require 'ctxmenu)
(require 'el-expectations)

(expectations
  (desc "get-common-words non-index")
  (expect '("region" "delete" "copy" "do" "dired")
    (ctxmenu::get-common-words '(dired-do-delete
                                 dired-do-copy
                                 dired-mark-directories
                                 copy-region-as-kill
                                 delete-region)))
  (desc "get-common-words index 1")
  (expect '("dired")
    (ctxmenu::get-common-words '(dired-do-delete
                                 dired-do-copy
                                 dired-mark-directories
                                 copy-region-as-kill
                                 delete-region)
                               1))
  (desc "get-common-words index 2")
  (expect '("region" "do")
    (ctxmenu::get-common-words '(dired-do-delete
                                 dired-do-copy
                                 dired-mark-directories
                                 copy-region-as-kill
                                 delete-region)
                               2))
  )

