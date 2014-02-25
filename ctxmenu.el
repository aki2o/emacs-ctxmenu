;;; ctxmenu.el --- Provide a context menu like right-click.

;; Copyright (C) 2014  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: popup
;; URL: https://github.com/aki2o/emacs-ctxmenu
;; Version: 0.3.0
;; Package-Requires: ((popup "20140205.103") (log4e "0.2.0") (yaxception "0.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; This extension provides a context menu like right-click.
;; 
;; For more infomation, see <https://github.com/aki2o/emacs-ctxmenu/blob/master/README.md>

;;; Dependencies:
;; 
;; - popup.el ( bundled auto-complete.el. see <https://github.com/auto-complete/auto-complete> )
;; - yaxception.el ( see <https://github.com/aki2o/yaxception> )
;; - log4e.el ( see <https://github.com/aki2o/log4e> )

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your .emacs or site-start.el file.
;; 
;; (require 'ctxmenu)

;;; Configuration:
;; 
;; ;; Key Binding
;; (define-key global-map (kbd "M-@") 'ctxmenu:show)
;; 
;; ;; Also, you need to define the contents of context menu into `ctxmenu:global-sources'/`ctxmenu:sources'.
;; ;; `ctxmenu:add-source' is a helper function for it.
;; ;; Moreover I have a basic configuration, see <https://github.com/aki2o/emacs-ctxmenu/blob/master/README.md>

;;; Customization:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'user-variable :prefix "ctxmenu:[^:]" :docstring t)
;; `ctxmenu:default-menu-list-function'
;; Function for building the menu of source.
;; `ctxmenu:default-sort-menu-function'
;; Function for the menu sort of source.
;; `ctxmenu:show-at-pointed'
;; Whether show a context menu at the point of cursol.
;; `ctxmenu:use-isearch'
;; Whether use isearch on selecting menu.
;; `ctxmenu:use-quick-help'
;; Whether use quick help.
;; `ctxmenu:quick-help-delay'
;; Delay to show quick help.
;; `ctxmenu:warning-menu-number-threshold'
;; Number as the threshold whether show the warning about the slowness of Emacs.
;; 
;;  *** END auto-documentation

;;; API:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'macro :prefix "ctxmenu:[^:]" :docstring t)
;; `ctxmenu:add-source'
;; Add a source into `ctxmenu:global-sources'/`ctxmenu:sources'.
;; 
;;  *** END auto-documentation
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "ctxmenu:[^:]" :docstring t)
;; `ctxmenu:show'
;; Show a context menu.
;; `ctxmenu:clear-cache'
;; Clear the cache of menu-list.
;; 
;;  *** END auto-documentation

;;; Other Function:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'function :prefix "ctxmenu:[^:]" :docstring t)
;; `ctxmenu:sort-menu-default'
;; Sort the candidate of menu.
;; `ctxmenu:menu-list-simple-cascade'
;; Build a cascade menu with prefixmatch.
;; `ctxmenu:menu-list-custom-cascade'
;; Build a cascade menu from MENU-ARG.
;; `ctxmenu:menu-list-flat'
;; Build a flat menu.
;; `ctxmenu:remove-source'
;; Remove a source from `ctxmenu:global-sources'/`ctxmenu:sources'.
;; 
;;  *** END auto-documentation
;; [Note] Functions and variables other than listed above, Those specifications may be changed without notice.

;;; Tested On:
;; 
;; - Emacs ... GNU Emacs 24.2.1 (i386-mingw-nt5.1.2600) of 2012-12-08 on GNUPACK
;; - popup.el ... 20140205.103
;; - yaxception.el ... Version 0.2.0
;; - log4e.el ... Version 0.1


;; Enjoy!!!


(eval-when-compile (require 'cl))
(require 'popup)
(require 'auto-complete nil t)
(require 'log4e)
(require 'yaxception)


(defgroup ctxmenu nil
  "Context menu like right-click."
  :group 'popup
  :prefix "ctxmenu:")

(defcustom ctxmenu:default-menu-list-function 'ctxmenu:menu-list-flat
  "Function for building the menu of source."
  :type 'function
  :group 'ctxmenu)

(defcustom ctxmenu:default-sort-menu-function 'ctxmenu:sort-menu-default
  "Function for the menu sort of source."
  :type 'function
  :group 'ctxmenu)

(defcustom ctxmenu:show-at-pointed nil
  "Whether show a context menu at the point of cursol."
  :type 'boolean
  :group 'ctxmenu)

(defcustom ctxmenu:use-isearch t
  "Whether use isearch on selecting menu."
  :type 'boolean
  :group 'ctxmenu)

(defcustom ctxmenu:use-quick-help t
  "Whether use quick help."
  :type 'boolean
  :group 'ctxmenu)

(defcustom ctxmenu:quick-help-delay 1.5
  "Delay to show quick help."
  :type 'float
  :group 'ctxmenu)

(defcustom ctxmenu:warning-menu-number-threshold nil
  "Number as the threshold whether show the warning about the slowness of Emacs."
  :type 'integer
  :group 'ctxmenu)


(defvar ctxmenu:sources nil
  "Buffer local sources for building a context menu.

About the format of source, see `ctxmenu:global-sources'.")
(make-variable-buffer-local 'ctxmenu:sources)

(defvar ctxmenu:global-sources nil
  "Global sources for building a context menu.

The format of source is alist that has the follwing item.
- prefix
- menu-name
- delimiter
- is-regexp
- keystroke
- excludes
- exclude-regexp
- include-all
- include-menu
- include-regexp
- menu-list
- menu-arg
- sort
- remain-prefix

About the item, see `ctxmenu:add-source'.")

(defvar ctxmenu:empty-candidate-value "(Default)"
  "Value as the display of a empty candidate in a cascade menu.")


(log4e:deflogger "ctxmenu" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                     (error . "error")
                                                     (warn  . "warn")
                                                     (info  . "info")
                                                     (debug . "debug")
                                                     (trace . "trace")))
(ctxmenu--log-set-level 'trace)


(defvar ctxmenu::hash-menu-list (make-hash-table :test 'equal))
(defvar ctxmenu::regexp-prefix nil)
(defvar ctxmenu::remain-prefix nil)
(defvar ctxmenu::menu-arg nil)


(defun* ctxmenu::show-message (msg &rest args)
  (apply 'message (concat "[CTXMENU] " msg) args)
  nil)

(defsubst ctxmenu::remove-prefix (sym)
  (let ((symnm (if (stringp sym) sym (symbol-name sym))))
    (if (or ctxmenu::remain-prefix
            (not ctxmenu::regexp-prefix))
        symnm
      (replace-regexp-in-string ctxmenu::regexp-prefix "\\1" symnm))))

(defsubst ctxmenu::get-composed-words (sym)
  (mapcar (lambda (s) (downcase s))
          (split-string (ctxmenu::remove-prefix sym) "-")))

(defsubst ctxmenu::get-menu-candidate-value (sym &optional depth)
  (loop with ret = ""
        with depth = (or depth 0)
        with words = (ctxmenu::get-composed-words sym)
        for idx from depth to (- (length words) 1)
        for w = (nth idx words)
        if w
        do (progn (when (not (string= ret ""))
                    (setq ret (concat ret " ")))
                  (setq ret (concat ret (capitalize w))))
        finally return ret))

(defsubst ctxmenu::get-binding-keys (cmd &optional include-menu)
  (loop for b in (where-is-internal cmd overriding-local-map)
        for bindkey = (or (ignore-errors (key-description b))
                          "")
        if (and (not (string= bindkey ""))
                (if (string-match "\\`<menu-bar>" bindkey) include-menu t)
                (not (string-match "\\`<[^>]*mouse[^>]*>" bindkey)))
        collect bindkey))

(defsubst ctxmenu::get-menu-candidate-summary (sym)
  (loop with ret = nil
        for k in (ctxmenu::get-binding-keys sym)
        if (or (not ret)
               (and (not (string-match "<[^>]+>" k))
                    (< (length k) (length ret))))
        do (setq ret k)
        finally return ret))

(defun ctxmenu::get-menu-help (cand)
  (ctxmenu--trace "start get menu help : %s" cand)
  (when (commandp cand)
    (let* ((doc (documentation cand))
           (bindkeys (mapcar (lambda (x) (format "'%s'" x)) (ctxmenu::get-binding-keys cand))))
      (when (or (not doc)
                (string= doc ""))
        (setq doc "Not documented."))
      (concat "This item calls `" (symbol-name cand) "'.\n"
              (or (when bindkeys
                    (concat "The command is bound to " (mapconcat 'identity bindkeys ", ") ".\n\n"))
                  "The command is not bound to any key.\n\n")
              "[Documentation]\n\n"
              doc))))

(defsubst ctxmenu::mk-menu-candidate (menunm sym)
  (popup-make-item menunm
                   :value sym
                   :summary (ctxmenu::get-menu-candidate-summary sym)
                   :document 'ctxmenu::get-menu-help))

(defun ctxmenu::get-menu-candidates (syms &optional depth)
  (ctxmenu--trace "start get menu candidates of depth[%s] from %s" depth syms)
  (let ((ret (loop for s in syms
                   for v = (ctxmenu::get-menu-candidate-value s depth)
                   do (when (string= v "")
                        (setq v (format ctxmenu:empty-candidate-value)))
                   collect (ctxmenu::mk-menu-candidate v s))))
    (ctxmenu--trace "got menu candidates : %s" (mapconcat 'identity ret ", "))
    ret))

(defun ctxmenu::get-max-composed-count (syms)
  (ctxmenu--trace "start get max composed count : %s" syms)
  (loop with ret = 0
        for s in syms
        for curr = (length (ctxmenu::get-composed-words s))
        if (> curr ret)
        do (setq ret curr)
        finally return ret))

(defun ctxmenu::get-common-words (syms &optional idx)
  (ctxmenu--trace "start get common words in idx[%s] from %s" idx syms)
  (let (words ret)
    (dolist (s syms)
      (dolist (w (cond ((not idx) (ctxmenu::get-composed-words s))
                       (t         (list (nth (- idx 1) (ctxmenu::get-composed-words s))))))
        (when w
          (if (member w words)
              (pushnew w ret :test 'equal)
            (push w words)))))
    (ctxmenu--trace "got common words : %s" ret)
    ret))

(defsubst ctxmenu::is-composed-symbol (sym word &optional idx)
  (let* ((testwords (ctxmenu::get-composed-words sym))
         (testword (when idx
                     (nth (- idx 1) testwords))))
    (cond (testword (string= word testword))
          (t        (member word testwords)))))

(defun ctxmenu::grep-composed-symbol (syms word &optional idx)
  (ctxmenu--trace "start grep composed symbol\nsymbols: %s\nword: %s\nindex: %s" syms word idx)
  (let ((ret (loop for s in syms
                   if (ctxmenu::is-composed-symbol s word idx)
                   collect s)))
    (ctxmenu--trace "greped composed symbol : %s" ret)
    ret))

(defsubst ctxmenu::expand-source (src)
  (cond ((symbolp src) (symbol-value src))
        ((listp src)   src)))

(defsubst ctxmenu::get-menunm (src)
  (let* ((prefix (assoc-default 'prefix src))
         (menunm (assoc-default 'menu-name src))
         (is-regexp (assoc-default 'is-regexp src)))
    (or menunm
        (when (and (stringp prefix)
                   (not is-regexp))
          (capitalize prefix))
        "")))

(defun ctxmenu::get-condition-matched-commands (cmds src)
  (ctxmenu--trace "get condition matched commands\ncmds: %s\nsrc: %s" cmds src)
  (let* ((excludes (assoc-default 'excludes src))
         (exclude-re (assoc-default 'exclude-regexp src))
         (include-re (assoc-default 'include-regexp src))
         (include-menu (assoc-default 'include-menu src))
         (include-all (assoc-default 'include-all src)))
    (loop for cmd in cmds
          for cmdnm = (symbol-name cmd)
          if (and (not (string-match "\\`ad-Orig-" cmdnm))
                  (not (memq cmd excludes))
                  (or (not exclude-re)
                      (not (string-match exclude-re cmdnm)))
                  (or include-all
                      (cond (include-re   (string-match include-re cmdnm))
                            (t            (ctxmenu::get-binding-keys cmd include-menu)))))
          collect cmd)))

(defun ctxmenu::get-binding-commands (keystroke)
  (ctxmenu--trace "start get binding commands : %s" keystroke)
  (with-temp-buffer
    (loop with indent-tabs-mode = t
          with cmds = nil
          for keystroke in (or (when (listp keystroke) keystroke)
                               (when (stringp keystroke) (list keystroke)))
          for keystroke = (read-kbd-macro keystroke)
          do (loop for kmapinfo in (append (minor-mode-key-binding keystroke)
                                           `((,major-mode . ,(local-key-binding keystroke)))
                                           `((global . ,(global-key-binding keystroke))))
                   for modenm = (car kmapinfo)
                   for modenm = (cond ((stringp modenm) modenm)
                                      ((symbolp modenm) (symbol-name modenm)))
                   for kmap = (cdr kmapinfo)
                   do (when (keymapp kmap)
                        (loop initially (progn (erase-buffer)
                                               (insert (substitute-command-keys "\\<kmap>\\{kmap}"))
                                               (goto-char (point-min))
                                               (forward-line 3)
                                               (delete-region (point-min) (point)))
                              while (not (eobp))
                              for e = (split-string (buffer-substring-no-properties
                                                     (point-at-bol)
                                                     (point-at-eol))
                                                    "\t+")
                              for cmdnm = (nth 1 e)
                              for cmd = (when cmdnm (intern-soft cmdnm))
                              do (when (commandp cmd) (pushnew cmd cmds))
                              do (forward-line))))
          finally return cmds)))

(defun ctxmenu::get-apropos-commands (prefix-re)
  (ctxmenu--trace "start get apropos commands : %s" prefix-re)
  (when prefix-re
    (loop for e in (apropos-internal prefix-re)
          if (commandp e) collect e)))

(defun ctxmenu::build-menu-list (&optional sources)
  (yaxception:$
    (yaxception:try
      (ctxmenu--trace "start build menu list. sources : %s" sources)
      (loop for src in (or sources
                           (append ctxmenu:sources ctxmenu:global-sources))
            for src = (ctxmenu::expand-source src)
            for menunm = (ctxmenu::get-menunm src)
            for prefix = (assoc-default 'prefix src)
            for delim = (assoc-default 'delimiter src)
            for is-regexp = (assoc-default 'is-regexp src)
            for delim = (cond ((and delim is-regexp) delim)
                              (delim                 (regexp-quote delim))
                              (t                     "[^a-zA-Z0-9]+"))
            for prefix-re = (cond ((or (not prefix)
                                       (string= prefix ""))
                                   (ctxmenu--error "not prefix in source : %s" src))
                                  (is-regexp
                                   (concat prefix delim "\\([a-zA-Z0-9]\\|\\'\\)"))
                                  (t
                                   (concat "\\`" (regexp-quote prefix) delim "\\([a-zA-Z0-9]\\|\\'\\)")))
            for keystroke = (assoc-default 'keystroke src)
            for key = (concat (symbol-name major-mode) " " menunm)
            for ret = (or (gethash key ctxmenu::hash-menu-list)
                          (puthash key
                                   (let* ((ctxmenu::regexp-prefix prefix-re)
                                          (ctxmenu::remain-prefix (assoc-default 'remain-prefix src))
                                          (menulist-func (or (assoc-default 'menu-list src)
                                                             ctxmenu:default-menu-list-function))
                                          (sort-func (or (assoc-default 'sort src)
                                                         ctxmenu:default-sort-menu-function))
                                          (ctxmenu::menu-arg (assoc-default 'menu-arg src))
                                          (cmds (cond (keystroke (ctxmenu::get-binding-commands keystroke))
                                                      (t         (ctxmenu::get-apropos-commands prefix-re))))
                                          (cmds (ctxmenu::get-condition-matched-commands cmds src))
                                          (cmdcount (length cmds)))
                                     (ctxmenu--trace "get menu list\nmenulist-func: %s\nmenu-arg: %s\nsort-func: %s\ncmds[%s]: %s"
                                                     menulist-func ctxmenu::menu-arg sort-func cmdcount cmds)
                                     (when (> cmdcount 50)
                                       (ctxmenu--info "the menu size of [%s] is [%s]" menunm cmdcount))
                                     (funcall menulist-func cmds sort-func))
                                   ctxmenu::hash-menu-list))
            if (and (not (string= menunm "")) ret)
            collect `(,menunm ,@ret)))
    (yaxception:catch 'error e
      (ctxmenu--error "failed build menu list : %s\n%s"
                      (yaxception:get-text e)
                      (yaxception:get-stack-trace-string e))
      (ctxmenu::show-message "Failed show : %s" (yaxception:get-text e)))))

(defsubst ctxmenu::get-setup-symbol (menunm)
  (intern (concat "ctxmenu::setup-"
                  (replace-regexp-in-string "[^a-zA-Z0-9!$%&=~|@:*_/?.,<>{}+-]" "" (downcase menunm)))))

(defun ctxmenu::count-menu-list (menu-list)
  (loop with ret = 0
        for e in menu-list
        do (incf ret)
        if (consp e)
        do (incf ret (ctxmenu::count-menu-list (cdr e)))
        finally return ret))


;;;;;;;;;;;;;;;
;; Sort Menu

(defun ctxmenu:sort-menu-default (m1 m2)
  "Sort the candidate of menu."
  (yaxception:$
    (yaxception:try
      (cond ((and (stringp m1) (string= m1 ctxmenu:empty-candidate-value))
             t)
            ((and (stringp m2) (string= m2 ctxmenu:empty-candidate-value))
             nil)
            ((and (listp m1) (listp m2))
             (string< (car m1) (car m2)))
            ((listp m1)
             t)
            ((listp m2)
             nil)
            (t
             (string< m1 m2))))
    (yaxception:catch 'error e
      (ctxmenu--error "failed sort menu default about m1[%s] m2[%s] : %s\n%s"
                      m1 m2 (yaxception:get-text e) (yaxception:get-stack-trace-string e)))))


;;;;;;;;;;;;;;;;
;; Build Menu

(defun ctxmenu:menu-list-simple-cascade (commands sort-func &optional depth)
  "Build a cascade menu with prefixmatch."
  (let* ((depth (or depth 1))
         (remains commands)
         (cwords (ctxmenu::get-common-words commands depth))
         (menu-list (loop for cword in cwords
                          for greped = (ctxmenu::grep-composed-symbol remains cword depth)
                          do (loop for e in greped
                                   if (memq e remains)
                                   do (setq remains (delq e remains)))
                          collect `(,(capitalize cword)
                                    ,@(ctxmenu:menu-list-simple-cascade greped sort-func (+ depth 1))))))
    (when (and menu-list remains)
      (setq menu-list (append (ctxmenu::get-menu-candidates remains (- depth 1))
                              menu-list)))
    (sort (or (when (or (> (length cwords) 1)
                        remains)
                menu-list)
              (ctxmenu::get-menu-candidates commands (- depth 1)))
          sort-func)))

(defun ctxmenu:menu-list-custom-cascade (commands sort-func &optional menu-arg)
  "Build a cascade menu from MENU-ARG.

The format of MENU-ARG is the list of plist that each of build one menu.
The plist format is (MENUNM :PROP VALUE ...).
MENUNM is a symbol means the name of child menu.
PROP/VALUE are the following items.

- REGEXP ... The format is a string or a cons of string or list of cons.
             This value or the first of this cons is a regular expression for listing menu.
             If the second of this cons exists, replace the matched part with it.
             If this format is list of cons, command is listed if one of them matches that.
- EQUAL  ... The format is a string or a cons of string or list of cons.
             This value or the first of this cons is a command name for listing menu.
             If the second of this cons exists, use that for the name of menu.
- NOSORT ... If non-nil, the menu item is not sorted.
- SUB    ... The format is a symbol of function or same format of MENU-ARG.
             If this value exists, create child menu by that."
  (loop with parse-prop = (lambda (x)
                            (cond ((stringp x)                       (list x))
                                  ((and (consp x) (stringp (car x))) (list x))
                                  (t                                 x)))
        with parse-elem = (lambda (x)
                            (list (if (stringp x) x (car x))
                                  (when (consp x) (cdr x))))
        for p in (or menu-arg ctxmenu::menu-arg)
        for menunm = (pop p)
        for menunm = (if (symbolp menunm) (symbol-name menunm) menunm)
        for re-prop = (plist-get p :regexp)
        for eq-prop = (plist-get p :equal)
        for nst-prop = (plist-get p :nosort)
        for sub-prop = (plist-get p :sub)
        for cands = (append
                     (loop for recons in (funcall parse-prop re-prop)
                           append (multiple-value-bind (re rep) (funcall parse-elem recons)
                                    (loop with ctxmenu::remain-prefix = (when rep t)
                                          for cmd in commands
                                          for cmdnm = (symbol-name cmd)
                                          for cmdnm = (when (string-match re cmdnm)
                                                        (if rep
                                                            (replace-regexp-in-string re rep cmdnm)
                                                          cmdnm))
                                          if cmdnm
                                          collect (if sub-prop
                                                      cmd
                                                    (ctxmenu::mk-menu-candidate (ctxmenu::get-menu-candidate-value cmdnm)
                                                                                cmd)))))
                     (loop for eqcons in (funcall parse-prop eq-prop)
                           collect (multiple-value-bind (eqval nmval) (funcall parse-elem eqcons)
                                     (loop for cmd in commands
                                           for cmdnm = (symbol-name cmd)
                                           if (string= cmdnm eqval)
                                           return (if sub-prop
                                                      cmd
                                                    (ctxmenu::mk-menu-candidate (or nmval
                                                                                    (ctxmenu::get-menu-candidate-value cmd))
                                                                                cmd))))))
        for cands = (loop for c in cands if c collect c)
        for cands = (cond (sub-prop
                           (when (and (not re-prop) (not eq-prop))
                             (setq cands commands))
                           (if (functionp sub-prop)
                               (funcall sub-prop cands sort-func)
                             (ctxmenu:menu-list-custom-cascade cands sort-func sub-prop)))
                          ((not nst-prop)
                           (sort cands sort-func))
                          (t
                           cands))
        if cands collect `(,menunm ,@cands)))

(defun ctxmenu:menu-list-flat (commands sort-func)
  "Build a flat menu."
  (sort (ctxmenu::get-menu-candidates commands) sort-func))


;;;;;;;;;;;;;;;;;;;;;;;
;; For Configuration

;;;###autoload
(defmacro* ctxmenu:add-source (&key local
                                    hook
                                    prefix
                                    menu-name
                                    delimiter
                                    is-regexp
                                    keystroke
                                    excludes
                                    exclude-regexp
                                    include-all
                                    include-menu
                                    include-regexp
                                    menu-list
                                    menu-arg
                                    sort
                                    remain-prefix)
  "Add a source into `ctxmenu:global-sources'/`ctxmenu:sources'.

About the search of the listed command. It's in two ways, PREFIX/DELIMITER or KEYSTROKE.
- PREFIX is string as the prefix of the target commands.
- DELIMITER is string as the delimiter between prefix and name in the command symbol.
- KEYSTROKE is string as the stroke of the keymap which has the target commands.
- IS-REGEXP is boolean. If non-nil means PREFIX/DELIMITER is a regular expression. (In the case, MENU-NAME is required.)

About the name of the menu which is built from the result of the command search.
- MENU-NAME is string as the menu name. If nil, use PREFIX.
- REMAIN-PREFIX is boolean. If non-nil, remain the part of PREFIX/DELIMITER in command. In default, remove it.

About the filter of listing the commands into menu-list.
In default, list only the command that is bound to some key from within the result of the command search.
- EXCLUDES is the list of the excluded command symbol.
- EXCLUDE-REGEXP is string as the regular expression of the excluded command.
- INCLUDE-ALL is boolean. If non-nil, list all commands.
- INCLUDE-MENU is boolean. If non-nil, moreover list the commands that is bound to menu-bar.
- INCLUDE-REGEXP is string as the regular expression of the included command.

About the target of the added source.
- LOCAL is boolean. If nil, the target is `ctxmenu:global-sources'. Else, it's `ctxmenu:sources'.
- HOOK is the symbol of hook. If the target is not a current `ctxmenu:sources', set the target mode hook.

About the method of the menu format.
- MENU-LIST is the symbol of the function that format menu-list. If nil, use `ctxmenu:default-menu-list-function'.
- MENU-ARG is the argument to MENU-LIST. It depends on MENU-LIST whether this value is necessary or not.
- SORT is the symbol of the function that sort menu-list. If nil, use `ctxmenu:default-sort-menu-function'."
  (let* ((src `((prefix         . ,(eval prefix))
                (menu-name      . ,menu-name)
                (delimiter      . ,(eval delimiter))
                (is-regexp      . ,is-regexp)
                (keystroke      . ,keystroke)
                (excludes       . ,(eval excludes))
                (exclude-regexp . ,(eval exclude-regexp))
                (include-all    . ,include-all)
                (include-menu   . ,include-menu)
                (include-regexp . ,(eval include-regexp))
                (menu-list      . ,(eval menu-list))
                (menu-arg       . ,(eval menu-arg))
                (sort           . ,(eval sort))
                (remain-prefix  . ,remain-prefix)))
         (currnm (ctxmenu::get-menunm src))
         (sources-sym (if local 'ctxmenu:sources 'ctxmenu:global-sources)))
    `(cond
      ((string= ,currnm "")
       (ctxmenu::show-message "Failed add source : prefix/menu-name is not given."))
      (t
       (ctxmenu:remove-source :local ,local
                              :hook ,hook
                              :prefix ,(eval prefix)
                              :menu-name ,menu-name
                              :is-regexp ,is-regexp)
       (cond (,hook
              (defun ,(ctxmenu::get-setup-symbol currnm) ()
                (ctxmenu:remove-source :local t
                                       :prefix ,(eval prefix)
                                       :menu-name ,menu-name
                                       :is-regexp ,is-regexp)
                (add-to-list 'ctxmenu:sources ',src)
                (ctxmenu--info "pushed local source : %s" ',src))
              (if (listp ,hook)
                  (dolist (h ,hook)
                    (add-hook h ',(ctxmenu::get-setup-symbol currnm) t)
                    (ctxmenu--info "define setup function for %s on %s" ,currnm h))
                (add-hook ,hook ',(ctxmenu::get-setup-symbol currnm) t)
                (ctxmenu--info "define setup function for %s on %s" ,currnm ,hook)))
             (t
              (add-to-list ',sources-sym ',src)
              (ctxmenu--info "pushed %s source : %s" (if ,local "local" "global") ',src)))))))

;;;###autoload
(defun* ctxmenu:remove-source (&key local
                                    hook
                                    prefix
                                    menu-name
                                    is-regexp)
  "Remove a source from `ctxmenu:global-sources'/`ctxmenu:sources'.

The argument is the same value of `ctxmenu:add-source'."
  (yaxception:$
    (yaxception:try
      (let* ((src `((prefix         . ,prefix)
                    (menu-name      . ,menu-name)
                    (is-regexp      . ,is-regexp)))
             (currnm (ctxmenu::get-menunm src))
             (sources-sym (if local 'ctxmenu:sources 'ctxmenu:global-sources)))
        (cond
         ((string= currnm "")
          (ctxmenu::show-message "Failed remove source : prefix/menu-name is not given."))
         (hook
          (if (listp hook)
              (dolist (h hook)
                (remove-hook h (ctxmenu::get-setup-symbol currnm)))
            (remove-hook hook (ctxmenu::get-setup-symbol currnm))))
         (t
          (dolist (src (symbol-value sources-sym))
            (let* ((rsrc (ctxmenu::expand-source src))
                   (menunm (ctxmenu::get-menunm rsrc)))
              (when (string= (downcase menunm) (downcase currnm))
                (set sources-sym (delete src (symbol-value sources-sym)))
                (ctxmenu--info "removed from %s source : %s" (if local "local" "global") src))))))))
    (yaxception:catch 'error e
      (ctxmenu--error "failed remove source : %s\n%s"
                      (yaxception:get-text e)
                      (yaxception:get-stack-trace-string e))
      (ctxmenu::show-message "Failed remove source : %s" (yaxception:get-text e)))))


;;;;;;;;;;;;;;;;;;
;; User Command

;;;###autoload
(defun ctxmenu:show (&optional sources)
  "Show a context menu.

SOURCES is the list of source. If nil, use `ctxmenu:global-sources' and `ctxmenu:sources'.
About the format of source, see `ctxmenu:global-sources'."
  (interactive)
  (yaxception:$
    (yaxception:try
      (ctxmenu--trace "start show : %s" sources)
      (let* ((menu-list (ctxmenu::build-menu-list sources))
             (menucount (ctxmenu::count-menu-list menu-list))
             (popup-menu-show-quick-help-function (if (and (functionp 'ac-quick-help-use-pos-tip-p)
                                                           (functionp 'ac-pos-tip-show-quick-help)
                                                           (ac-quick-help-use-pos-tip-p))
                                                      'ac-pos-tip-show-quick-help
                                                    popup-menu-show-quick-help-function))
             (selected (if (not menu-list)
                           (ctxmenu::show-message "Available menu is nothing")
                         (when (and ctxmenu:warning-menu-number-threshold
                                    (> menucount ctxmenu:warning-menu-number-threshold))
                           (ctxmenu--warn "the all number of the menu is %s" menucount)
                           (ctxmenu::show-message "Freeze might happen because the number of the menu exceeds %s."
                                                  ctxmenu:warning-menu-number-threshold)
                           (sleep-for 1))
                         (ctxmenu--debug "start popup cascade menu : %s" menucount)
                         (ctxmenu--trace "menu-list:\n%s" menu-list)
                         (popup-cascade-menu menu-list
                                             :height (length menu-list)
                                             :scroll-bar t
                                             :help-delay (when ctxmenu:use-quick-help ctxmenu:quick-help-delay)
                                             :point (when (not ctxmenu:show-at-pointed) (window-start))
                                             :isearch ctxmenu:use-isearch))))
        (if (or (not (symbolp selected))
                (not (commandp selected)))
            (ctxmenu::show-message "Selected value is not command : %s" selected)
          (ctxmenu--trace "execute selected command : %s" selected)
          (call-interactively selected))))
    (yaxception:catch 'error e
      (ctxmenu--error "failed show : %s\n%s"
                      (yaxception:get-text e)
                      (yaxception:get-stack-trace-string e))
      (ctxmenu::show-message "Failed show : %s" (yaxception:get-text e)))))

;;;###autoload
(defun ctxmenu:clear-cache ()
  "Clear the cache of menu-list."
  (interactive)
  (setq ctxmenu::hash-menu-list (make-hash-table :test 'equal))
  (ctxmenu::show-message "Cleared cache"))


(provide 'ctxmenu)
;;; ctxmenu.el ends here
