(require 'ctxmenu)
(require 'el-expectations)

(expectations
  (desc "count-menu-list simple")
  (expect 3
    (ctxmenu::count-menu-list '(a b c)))
  (desc "count-menu-list nest")
  (expect 15
    (ctxmenu::count-menu-list '(a (b c (d e f (g))) (h (i (j k l m n o))))))
  )

