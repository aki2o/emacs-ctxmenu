;;; ctxmenu-config.el --- provide configuration for ctxmenu.el.

;; Copyright (C) 2014  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: popup
;; URL: https://github.com/aki2o/emacs-ctxmenu
;; Version: 0.0.1
;; Package-Requires: ((ctxmenu "0.0.1"))

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
;; This extension provides configuration for ctxmenu.el.

;;; Dependency:
;; 
;; - ctxmenu.el ( see <https://github.com/aki2o/emacs-ctxmenu> )

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your .emacs or site-start.el file.
;; 
;; (require 'ctxmenu-config)

;;; Configuration:
;; 
;; see <https://github.com/aki2o/emacs-ctxmenu/blob/master/README.md>.

;;; Tested On:
;; 
;; - Emacs ... GNU Emacs 24.2.1 (i386-mingw-nt5.1.2600) of 2012-12-08 on GNUPACK
;; - ctxmenu.el ... Version 0.0.1


;; Enjoy!!!


(require 'ctxmenu)

(defvar ctxmenu-config:default-features '(emacshelp
                                          move window region rectangle register coding-system kmacro outline flymake package el-get
                                          dired help info buff ibuffer shell org gnus w3m magit dsvn twittering-mode bbdb bbdb- cperl
                                          moccur anything helm yas tabbar sdic text-translator direx e2wm tail pophint
                                          plsense rsense vbasense tss log4e ajc-java-complete-config scala-mode-auto ensime)
  "List of the symbol that the menu is built of in default.")

(defvar ctxmenu-config:extra-features '(frame kmacro-all tag narrow vc)
  "List of the symbol that the menu is not built of in default.")

(defvar ctxmenu-config:exclude-features nil
  "List of the symbol that the menu is not built of in `ctxmenu-config:default-features'.")

(defun* ctxmenu-config:setup (&rest features)
  "Define sources for `ctxmenu:show'.

- FEATURES is the list of the symbol that the menu is built of. If nil, use `ctxmenu-config:default-features'."
  (dolist (f (or features ctxmenu-config:default-features))
    (when (not (memq f ctxmenu-config:exclude-features))
      (case f
        (emacshelp                (ctxmenu:add-source :menu-name "EmacsHelp"
                                                      :keystroke "<f1>"
                                                      :menu-list 'ctxmenu:menu-list-simple-cascade))
        (move                     (ctxmenu:add-source :prefix (rx-to-string `(and bos (or "forward"
                                                                                          "backward"
                                                                                          "up"
                                                                                          "down"
                                                                                          "beginning-of"
                                                                                          "end-of"
                                                                                          "next"
                                                                                          "previous"
                                                                                          "goto") eow))
                                                      :menu-name "Move"
                                                      :delimiter "-"
                                                      :is-regexp t
                                                      :include-all t
                                                      :menu-list 'ctxmenu:menu-list-simple-cascade
                                                      :remain-prefix t))
        (window                   (ctxmenu:add-source :menu-name "Window"
                                                      :keystroke "C-x 4"
                                                      :menu-list 'ctxmenu:menu-list-flat))
        (frame                    (ctxmenu:add-source :menu-name "Frame"
                                                      :keystroke "C-x 5"
                                                      :menu-list 'ctxmenu:menu-list-flat))
        (region                   (ctxmenu:add-source :prefix "-region\\'"
                                                      :menu-name "Region"
                                                      :delimiter ""
                                                      :is-regexp t
                                                      :remain-prefix t
                                                      :menu-list 'ctxmenu:menu-list-flat))
        (rectangle                (ctxmenu:add-source :menu-name "Rectangle"
                                                      :keystroke "C-x r"
                                                      :include-regexp "\\<rectangle\\>"
                                                      :menu-list 'ctxmenu:menu-list-flat))
        (register                 (ctxmenu:add-source :menu-name "Register"
                                                      :keystroke "C-x r"
                                                      :include-regexp "\\<register\\>"
                                                      :menu-list 'ctxmenu:menu-list-flat))
        (coding-system            (ctxmenu:add-source :menu-name "CodingSystem"
                                                      :keystroke "C-x RET"
                                                      :include-regexp "\\<coding\\>"
                                                      :menu-list 'ctxmenu:menu-list-flat))
        (kmacro                   (ctxmenu:add-source :menu-name "KbdMacro"
                                                      :keystroke "C-x C-k"
                                                      :menu-list 'ctxmenu:menu-list-flat))
        (kmacro-all               (ctxmenu:add-source :prefix "-?k\\(?:bd-\\)?macro-?"
                                                      :menu-name "KbdMacro"
                                                      :delimiter ""
                                                      :is-regexp t
                                                      :include-all t
                                                      :remain-prefix t
                                                      :menu-list 'ctxmenu:menu-list-flat))
        (tag                      (ctxmenu:add-source :prefix "\\<tags?\\>"
                                                      :menu-name "Tag"
                                                      :delimiter ""
                                                      :is-regexp t
                                                      :include-all t
                                                      :menu-list 'ctxmenu:menu-list-flat
                                                      :remain-prefix t))
        (outline                  (ctxmenu:add-source :menu-name "Outline"
                                                      :keystroke "C-o"
                                                      :menu-list 'ctxmenu:menu-list-flat))
        (narrow                   (ctxmenu:add-source :menu-name "Narrow"
                                                      :keystroke "C-x n"
                                                      :menu-list 'ctxmenu:menu-list-flat))
        (vc                       (ctxmenu:add-source :prefix "vc"
                                                      :menu-name "VersionControl"
                                                      :delimiter "-"
                                                      :include-menu t
                                                      :menu-list 'ctxmenu:menu-list-flat))
        (flymake                  (ctxmenu:add-source :prefix "flymake"
                                                      :delimiter "-"
                                                      :include-menu t
                                                      :menu-list 'ctxmenu:menu-list-flat))
        (package                  (ctxmenu:add-source :prefix "package"
                                                      :delimiter "-"
                                                      :hook 'package-menu-mode-hook))
        (el-get                   (ctxmenu:add-source :prefix "el-get"
                                                      :menu-name "El-Get"
                                                      :delimiter "-"
                                                      :hook 'el-get-package-menu-mode-hook))
        (dired                    (ctxmenu:add-source :prefix "dired"
                                                      :delimiter "-"
                                                      :menu-list 'ctxmenu:menu-list-simple-cascade
                                                      :hook 'dired-mode-hook))
        (help                     (ctxmenu:add-source :prefix "help"
                                                      :delimiter "-"
                                                      :hook 'help-mode-hook))
        (info                     (ctxmenu:add-source :prefix "\\`[iI]nfo"
                                                      :menu-name "Info"
                                                      :delimiter "-"
                                                      :is-regexp t
                                                      :menu-list 'ctxmenu:menu-list-simple-cascade
                                                      :hook 'Info-mode-hook))
        (buff                     (ctxmenu:add-source :prefix "Buffer-menu"
                                                      :menu-name "Buffer"
                                                      :delimiter "-"
                                                      :menu-list 'ctxmenu:menu-list-flat
                                                      :hook 'Buffer-menu-mode-hook))
        (ibuffer                  (ctxmenu:add-source :prefix "ibuffer"
                                                      :menu-name "iBuffer"
                                                      :delimiter "-"
                                                      :include-regexp (rx-to-string `(and bos "ibuffer-" (or "add"
                                                                                                             "backward"
                                                                                                             "do"
                                                                                                             "filter-by"
                                                                                                             "forward"
                                                                                                             "jump"
                                                                                                             "mark"
                                                                                                             "switch"
                                                                                                             "toggle"
                                                                                                             "unmark") eow))
                                                      :menu-list 'ctxmenu:menu-list-simple-cascade
                                                      :hook 'ibuffer-mode-hook))
        (shell                    (ctxmenu:add-source :prefix "shell"
                                                      :delimiter "-"
                                                      :hook 'shell-mode-hook))
        (org                      (ctxmenu:add-source :prefix "org"
                                                      :delimiter "-"
                                                      :menu-list 'ctxmenu:menu-list-simple-cascade
                                                      :hook 'org-mode-hook))
        (gnus                     (ctxmenu:add-source :prefix "gnus"
                                                      :delimiter "-"
                                                      :menu-list 'ctxmenu:menu-list-simple-cascade
                                                      :hook '(gnus-started-hook
                                                              gnus-select-group-hook
                                                              gnus-select-article-hook)))
        (w3m                      (ctxmenu:add-source :prefix "w3m"
                                                      :menu-name "W3M"
                                                      :delimiter "-"
                                                      :hook 'w3m-mode-hook))
        (magit                    (ctxmenu:add-source :prefix "magit"
                                                      :menu-name "Git"
                                                      :delimiter "-"
                                                      :hook 'magit-mode-hook))
        (dsvn                     (ctxmenu:add-source :prefix "dsvn"
                                                      :menu-name "SVN"
                                                      :delimiter "-"
                                                      :hook 'dsvn-mode-hook))
        (twittering-mode          (ctxmenu:add-source :prefix "twittering"
                                                      :menu-name "Twitter"
                                                      :delimiter "-"
                                                      :hook 'twittering-mode-hook))
        (bbdb                     (ctxmenu:add-source :prefix "bbdb"
                                                      :menu-name "BBDB"
                                                      :delimiter "-"
                                                      :hook 'bbdb-mode-hook))
        (bbdb-                    (ctxmenu:add-source :prefix "bbdb-"
                                                      :menu-name "BBDB-"
                                                      :delimiter ":"
                                                      :hook 'bbdb-:mode-hook))
        (cperl                    (ctxmenu:add-source :prefix "cperl"
                                                      :menu-name "Perl"
                                                      :delimiter "-"
                                                      :include-all t
                                                      :hook 'cperl-mode-hook))
        (moccur                   (ctxmenu:add-source :prefix "moccur"
                                                      :delimiter "-"
                                                      :include-menu t))
        (anything                 (ctxmenu:add-source :prefix "anything"
                                                      :delimiter "-"))
        (helm                     (ctxmenu:add-source :prefix "helm"
                                                      :delimiter "-"))
        (yas                      (ctxmenu:add-source :prefix "yas"
                                                      :menu-name "YASnippet"
                                                      :delimiter "/"
                                                      :menu-list 'ctxmenu:menu-list-flat))
        (tabbar                   (ctxmenu:add-source :prefix "tabbar"
                                                      :menu-name "TabBar"
                                                      :delimiter "-"
                                                      :menu-list 'ctxmenu:menu-list-flat))
        (sdic                     (ctxmenu:add-source :prefix "sdic"
                                                      :menu-name "SDic"
                                                      :delimiter "-"
                                                      :include-all t
                                                      :menu-list 'ctxmenu:menu-list-flat))
        (text-translator          (ctxmenu:add-source :prefix "text-translator"
                                                      :menu-name "TextTranslator"
                                                      :delimiter "-"
                                                      :include-all t
                                                      :menu-list 'ctxmenu:menu-list-flat))
        (direx                    (ctxmenu:add-source :prefix "direx"
                                                      :menu-name "DireX"
                                                      :delimiter ":"
                                                      :include-menu t))
        (e2wm                     (ctxmenu:add-source :prefix "e2wm"
                                                      :menu-name "E2WM"
                                                      :delimiter ":"
                                                      :include-menu t
                                                      :menu-list 'ctxmenu:menu-list-simple-cascade))
        (tail                     (ctxmenu:add-source :prefix "tail"
                                                      :delimiter "-"
                                                      :include-all t
                                                      :menu-list 'ctxmenu:menu-list-flat))
        (pophint                  (ctxmenu:add-source :prefix "pophint"
                                                      :menu-name "PopHint"
                                                      :delimiter ":"
                                                      :include-all t))
        (plsense                  (ctxmenu:add-source :prefix "plsense"
                                                      :menu-name "PlSense"
                                                      :delimiter "-"
                                                      :include-all t
                                                      :menu-list 'ctxmenu:menu-list-flat
                                                      :hook 'cperl-mode-hook))
        (rsense                   (ctxmenu:add-source :prefix "rsense"
                                                      :menu-name "RSense"
                                                      :delimiter "-"
                                                      :include-all t
                                                      :menu-list 'ctxmenu:menu-list-flat
                                                      :hook 'ruby-mode-hook))
        (vbasense                 (ctxmenu:add-source :prefix "vbasense"
                                                      :menu-name "VBASense"
                                                      :delimiter "-"
                                                      :include-all t
                                                      :menu-list 'ctxmenu:menu-list-flat
                                                      :hook 'visual-basic-mode-hook))
        (tss                      (ctxmenu:add-source :prefix "tss"
                                                      :menu-name "TSS"
                                                      :delimiter "-"
                                                      :include-all t
                                                      :menu-list 'ctxmenu:menu-list-flat
                                                      :hook 'typescript-mode-hook))
        (log4e                    (ctxmenu:add-source :prefix "log4e"
                                                      :menu-name "Log4E"
                                                      :delimiter ":"
                                                      :menu-list 'ctxmenu:menu-list-flat
                                                      :hook '(emacs-lisp-mode-hook
                                                              log4e-mode-hook)))
        (ajc-java-complete-config (ctxmenu:add-source :prefix "ajc"
                                                      :delimiter "-"
                                                      :hook '(java-mode-hook
                                                              malabar-mode-hook)))
        (scala-mode-auto          (ctxmenu:add-source :prefix "scala"
                                                      :delimiter "-"
                                                      :hook 'scala-mode-hook))
        (ensime                   (ctxmenu:add-source :prefix "ensime"
                                                      :delimiter "-"
                                                      :include-menu t
                                                      :menu-list 'ctxmenu:menu-list-simple-cascade
                                                      :hook 'scala-mode-hook))
        ))))


(provide 'ctxmenu-config)
;;; ctxmenu-config.el ends here
