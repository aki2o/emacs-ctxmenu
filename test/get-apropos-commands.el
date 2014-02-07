(require 'ctxmenu)
(require 'el-expectations)

(expectations
  (desc "get-apropos-commands")
  (expect t
    (let ((expectlist '(forward-char backward-char)))
      (loop with cnt = 0
            for e in (ctxmenu::get-apropos-commands "\\`\\(for\\|back\\)ward-char\\'")
            if (not (memq e expectlist))
            return nil
            do (incf cnt)
            finally return (eq cnt (length expectlist)))))
  )

