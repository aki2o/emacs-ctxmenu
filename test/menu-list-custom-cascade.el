(require 'ctxmenu)
(require 'el-expectations)

(expectations
  (desc "menu-list-custom-cascade")
  (expect '(("Node" ("File" #("Dired Find File" 0 15 (document ctxmenu::get-menu-help value dired-find-file))) ("Dir" #("MarkDired" 0 9 (document ctxmenu::get-menu-help value dired-mark-directories)))) ("Copy" #("Copy Rectangle To Register" 0 26 (summary "C-x r r" document ctxmenu::get-menu-help value copy-rectangle-to-register)) #("Dired Do Copy" 0 13 (document ctxmenu::get-menu-help value dired-do-copy))) ("Dired" #("Dired At Point" 0 14 (document ctxmenu::get-menu-help value dired-at-point)) #("Dired Do Copy" 0 13 (document ctxmenu::get-menu-help value dired-do-copy)) #("Dired Do Delete" 0 15 (document ctxmenu::get-menu-help value dired-do-delete)) #("Dired Mark Directories" 0 22 (document ctxmenu::get-menu-help value dired-mark-directories)) #("Finding" 0 7 (document ctxmenu::get-menu-help value dired-find-file))))
    (let ((ctxmenu::menu-arg '((Node :sub ((File :equal "dired-find-file")
                                           (Dir :equal ("dired-mark-directories" . "MarkDired"))))
                               (Copy :regexp "copy")
                               (Dired :regexp (("do")
                                               ("mark"))
                                      :equal (("dired-at-point")
                                              ("dired-find-file" . "Finding"))))))
      (ctxmenu:menu-list-custom-cascade '(dired-do-delete
                                          dired-do-copy
                                          dired-mark-directories
                                          dired-at-point
                                          dired-find-file
                                          copy-rectangle-to-register)
                                        'ctxmenu:sort-menu-default)))
  )

