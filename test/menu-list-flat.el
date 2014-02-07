(require 'ctxmenu)
(require 'el-expectations)

(expectations
  (desc "menu-list-flat")
  (expect '(#("Copy Rectangle To Register" 0 1 (value copy-rectangle-to-register document ctxmenu::get-menu-help)) #("Copy Region As Kill" 0 1 (value copy-region-as-kill document ctxmenu::get-menu-help)) #("Dired At Point" 0 1 (value dired-at-point document ctxmenu::get-menu-help)) #("Dired Do Copy" 0 1 (value dired-do-copy document ctxmenu::get-menu-help)) #("Dired Do Delete" 0 1 (value dired-do-delete document ctxmenu::get-menu-help)) #("Dired Mark Directories" 0 1 (value dired-mark-directories document ctxmenu::get-menu-help)))
    (ctxmenu:menu-list-flat '(dired-do-delete
                              dired-do-copy
                              dired-mark-directories
                              dired-at-point
                              copy-region-as-kill
                              copy-rectangle-to-register)
                            'ctxmenu:sort-menu-default))
  )

