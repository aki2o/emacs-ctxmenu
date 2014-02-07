(require 'ctxmenu)
(require 'el-expectations)

(expectations
  (desc "add-source to global")
  (expect '(((prefix . "test+")
             (menu-name . "Hoge")
             (delimiter . "-")
             (is-regexp . t)
             (keystroke . "C-x RET")
             (excludes . (hoge fuga))
             (exclude-regexp . "\\`bar")
             (include-all . t)
             (include-menu . t)
             (include-regexp . "baz\\'")
             (menu-list . ctxmenu:menu-list-flat)
             (sort . ctxmenu:sort-menu-default)
             (remain-prefix . t)))
    (let ((ctxmenu:global-sources))
      (ctxmenu:add-source :prefix "test+"
                          :menu-name "Hoge"
                          :delimiter "-"
                          :is-regexp t
                          :keystroke "C-x RET"
                          :excludes '(hoge fuga)
                          :exclude-regexp "\\`bar"
                          :include-all t
                          :include-menu t
                          :include-regexp "baz\\'"
                          :menu-list 'ctxmenu:menu-list-flat
                          :sort 'ctxmenu:sort-menu-default
                          :remain-prefix t)
      ctxmenu:global-sources))
  (desc "add-source to local")
  (expect '(((prefix . "test+")
             (menu-name . "Hoge")
             (delimiter . "-")
             (is-regexp . t)
             (keystroke . "C-x RET")
             (excludes . (hoge fuga))
             (exclude-regexp . "\\`bar")
             (include-all . t)
             (include-menu . t)
             (include-regexp . "baz\\'")
             (menu-list . ctxmenu:menu-list-flat)
             (sort . ctxmenu:sort-menu-default)
             (remain-prefix . t)))
    (let ((ctxmenu:sources))
      (ctxmenu:add-source :prefix "test+"
                          :menu-name "Hoge"
                          :delimiter "-"
                          :is-regexp t
                          :keystroke "C-x RET"
                          :excludes '(hoge fuga)
                          :exclude-regexp "\\`bar"
                          :include-all t
                          :include-menu t
                          :include-regexp "baz\\'"
                          :menu-list 'ctxmenu:menu-list-flat
                          :sort 'ctxmenu:sort-menu-default
                          :remain-prefix t
                          :local t)
      ctxmenu:sources))
  (desc "add-source to hook")
  (expect '(ctxmenu::setup-localtest+)
    (let ((ctxmenu:sources)
          (hoge-hook))
      (ctxmenu:add-source :prefix "localtest+"
                          :hook 'hoge-hook)
      (when (not ctxmenu:sources)
        hoge-hook)))
  ;; (desc "add-source to local by hook")
  ;; (expect '(((prefix . "localtest+")
  ;;            (menu-name . "Hoge")
  ;;            (delimiter . "-")
  ;;            (is-regexp . t)
  ;;            (keystroke . "C-x RET")
  ;;            (excludes . (hoge fuga))
  ;;            (exclude-regexp . "\\`bar")
  ;;            (include-all . t)
  ;;            (include-menu . t)
  ;;            (include-regexp . "baz\\'")
  ;;            (menu-list . ctxmenu:menu-list-flat)
  ;;            (sort . ctxmenu:sort-menu-default)
  ;;            (remain-prefix . t)))
  ;;   (let ((ctxmenu:sources)
  ;;         (hoge-hook))
  ;;     (ctxmenu:add-source :prefix "localtest+"
  ;;                         :menu-name "Hoge"
  ;;                         :delimiter "-"
  ;;                         :is-regexp t
  ;;                         :keystroke "C-x RET"
  ;;                         :excludes '(hoge fuga)
  ;;                         :exclude-regexp "\\`bar"
  ;;                         :include-all t
  ;;                         :include-menu t
  ;;                         :include-regexp "baz\\'"
  ;;                         :menu-list 'ctxmenu:menu-list-flat
  ;;                         :sort 'ctxmenu:sort-menu-default
  ;;                         :remain-prefix t
  ;;                         :hook 'dired-mode-hook)
  ;;     (ctxmenu::setup-localtest+)
  ;;     ctxmenu:sources))
  (desc "add-source to hooks 1")
  (expect '(ctxmenu::setup-localtest+)
    (let ((hoge-hook)
          (fuga-hook))
      (ctxmenu:add-source :prefix "localtest+"
                          :hook '(hoge-hook fuga-hook))
      hoge-hook))
  (desc "add-source to hooks 2")
  (expect '(ctxmenu::setup-localtest+)
    (let ((hoge-hook)
          (fuga-hook))
      (ctxmenu:add-source :prefix "localtest+"
                          :hook '(hoge-hook fuga-hook))
      fuga-hook))
  (desc "add-source to hook with non symbolable char")
  (expect '(ctxmenu::setup-azaz09!$%&=~|@:*_/\?\.\,<>{}-)
    (let ((hoge-hook))
      (ctxmenu:add-source :prefix "azAZ09!$%&#=~'|@:*_`/?.,<あ>{}-"
                          :hook 'hoge-hook)
      hoge-hook))
  )

