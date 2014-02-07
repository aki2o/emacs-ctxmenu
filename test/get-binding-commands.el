(require 'ctxmenu)
(require 'el-expectations)

(expectations
  (desc "get-binding-commands")
  (expect t
    (let ((expectlist '(set-selection-coding-system
                        set-terminal-coding-system
                        revert-buffer-with-coding-system
                        set-buffer-process-coding-system
                        set-language-environment
                        set-keyboard-coding-system
                        set-buffer-file-coding-system
                        universal-coding-system-argument
                        set-next-selection-coding-system
                        set-file-name-coding-system
                        set-input-method)))
      (loop with cnt = 0
            for e in (ctxmenu::get-binding-commands "C-x RET")
            if (not (memq e expectlist))
            return nil
            do (incf cnt)
            finally return (eq cnt (length expectlist)))))
  )

