;;; init --- The emacs initialization file
;;
;; Copyright (c) 2016 Remy Goldschmidt
;;
;;; Commentary:
;;
;; The initialization file for my Emacs config.
;;
;; Mostly just loads `settings.org' and sets custom variables, though there
;; some miscellany is also done here.
;;
;;
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

;; Global variables

;; Default size for GC pool in bytes.
(defvar gc-default-max-size)
(setq gc-default-max-size 16777216) ;; 16 MiB

;; Helper functions
(defun set-gc-max-size (size)
  "Set the GC threshold to a given SIZE in bytes.

The given size will be rounded to the nearest 16 bytes because we are setting
the `gc-cons-threshold', which is measured in cons cells that are two words
wide each.  We also add 2048 bytes to the given SIZE, just in case you specify
a value that is too small.  If a negative SIZE is given, nothing happens."
  (when (> size 0)
    (setq gc-cons-threshold (+ (/ size 16) 128))))

(defun reset-gc-max-size ()
  "Reset the GC max size to the value specified in `gc-default-max-size'."
  (set-gc-max-size gc-default-max-size))

;; Opening the minibuffer should temporarily disable garbage collection.
(defun gc-minibuffer-setup-hook () "FIXME." (set-gc-max-size 8589934592)) ;; 8 GiB
(defun gc-minibuffer-exit-hook  () "FIXME." (reset-gc-max-size))
(add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook  #'gc-minibuffer-exit-hook)

;; Disable bell
(setq ring-bell-function 'ignore)
(setq visible-bell nil)

;; Temporarily increase the GC threshold to 128 MiB while initializing
(set-gc-max-size 134217728)

(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org" user-emacs-directory))

;; Set it back to the default value
(reset-gc-max-size)

;; -----------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-indent-level 0)
 '(LaTeX-item-indent 0)
 '(TeX-auto-untabify t)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(asm-comment-char 35)
 '(c-offsets-alist (quote ((innamespace . [0]))))
 '(create-lockfiles nil)
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("07dda9a3249f9ac909e7e0dc3c8876fd45898aa21646e093148dbd6ebb294f66" "0aa12caf6127772c1a38f7966de8258e7a0651fb6f7220d0bbb3a0232fba967f" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "870a63a25a2756074e53e5ee28f3f890332ddc21f9e87d583c5387285e882099" "79a3f477ac0cb4a106f78b6109614e991564a5c2467c36e6e854d4bc1102e178" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(delete-trailing-lines t)
 '(el-get-allow-insecure t)
 '(el-get-fossil-dir nil)
 '(el-get-status-file "/home/remy/.emacs.d/el-get/status.el")
 '(elfeed-db-directory "~/.cache/emacs/elfeed")
 '(elm-indent-after-keywords
   (quote
    (("of" 2)
     ("in" 2 0)
     ("{" 2)
     "if" "then" "else" "let")))
 '(elmo-localdir-folder-path "~/.cache/emacs/mail")
 '(elmo-msgdb-directory "~/.cache/emacs/elmo")
 '(epa-file-cache-passphrase-for-symmetric-encryption t)
 '(fci-rule-color "#383838")
 '(flycheck-checkers
   (quote
    (rtags idris ada-gnat asciidoc c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint d-dmd emacs-lisp emacs-lisp-checkdoc erlang eruby-erubis fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck groovy haml handlebars haskell-ghc haskell-hlint html-tidy jade javascript-eslint javascript-jshint javascript-gjslint javascript-jscs javascript-standard json-jsonlint json-python-json less luacheck lua perl perl-perlcritic php php-phpmd php-phpcs processing puppet-parser puppet-lint python-flake8 python-pylint python-pycompile r-lintr racket rpm-rpmlint rst-sphinx rst ruby-rubocop ruby-rubylint ruby ruby-jruby rust-cargo rust sass scala scala-scalastyle scss-lint scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim sql-sqlint tex-chktex tex-lacheck texinfo verilog-verilator xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby)))
 '(flycheck-clang-blocks t)
 '(flycheck-clang-language-standard "c++14")
 '(flycheck-cppcheck-checks (quote ("style")))
 '(flycheck-elm-executable "elm make")
 '(flycheck-pmd-rulesets
   (quote
    ("java-basic" "java-design" "java-imports" "java-braces" "java-unusedcode" "java-naming" "java-optimizations" "java-unnecessary" "java-sunsecure" "java-clone" "java-codesize" "java-comments" "java-coupling" "java-typeresolution" "java-strictexception" "java-strings" "java-empty" "java-junit")))
 '(flycheck-sh-shellcheck-executable nil)
 '(font-latex-fontify-script nil)
 '(gac-automatically-push-p t)
 '(geiser-guile-extra-keywords
   (quote
    ("lambda\\*" "λ\\*" "case-lambda" "define~" "define-inlinable" "syntax-case")))
 '(geiser-guile-load-init-file-p t)
 '(geiser-mode-smart-tab-p t)
 '(haskell-complete-module-preferred
   (quote
    ("Data.ByteString" "Data.ByteString.Lazy" "Data.Conduit" "Data.Function" "Data.List" "Data.Map" "Data.Maybe" "Data.Monoid" "Data.Ord")))
 '(haskell-completing-read-function (quote helm--completing-read-default))
 '(haskell-decl-scan-add-to-menubar nil)
 '(haskell-doc-chop-off-context nil)
 '(haskell-doc-show-global-types nil)
 '(haskell-interactive-mode-eval-mode (quote ignore))
 '(haskell-interactive-mode-eval-pretty nil)
 '(haskell-interactive-mode-include-file-name nil)
 '(haskell-interactive-mode-scroll-to-bottom t)
 '(haskell-mode-hook
   (quote
    (interactive-haskell-mode turn-on-haskell-indentation turn-on-haskell-doc-mode)))
 '(haskell-notify-p t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-generate-tags nil)
 '(haskell-process-reload-with-fbytecode nil)
 '(haskell-process-suggest-haskell-docs-imports nil)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-suggest-overloaded-strings t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-process-use-presentation-mode t)
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save nil)
 '(helm-autoresize-max-height 30)
 '(helm-swoop-speed-or-color t)
 '(hi2-show-indentations nil)
 '(hl-sexp-background-color "#efebe9")
 '(hs-lint-executable "hlint --ignore='Use camelCase'")
 '(idris-enable-elab-prover t)
 '(idris-interpreter-flags nil)
 '(latex-indent-within-escaped-parens t)
 '(lisp-backquote-indentation nil)
 '(lisp-lambda-list-keyword-alignment t)
 '(lisp-lambda-list-keyword-parameter-alignment t)
 '(lisp-lambda-list-keyword-parameter-indentation 0)
 '(markdown-enable-math t)
 '(markdown-indent-on-enter nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(offlineimap-mode-line-text "imap: ")
 '(org-agenda-span (quote fortnight))
 '(org-deadline-warning-days 4)
 '(org-src-fontify-natively t)
 '(org-support-shift-select (quote always))
 '(package-selected-packages
   (quote
    (company-flow flycheck-flow gitattributes-mode gitconfig-mode gitignore-mode maude-mode boogie-friends rtags z3-mode thrift rustfmt nlinum nixos-options material-theme jq-mode javaimp help-fns+ helm-fuzzier helm-flx flycheck-ghcmod flycheck-clangcheck flycheck-ats2 delight cmake-font-lock adjust-parens)))
 '(purescript-mode-hook
   (quote
    (turn-on-eldoc-mode turn-on-purescript-indent turn-on-purescript-indentation turn-on-purescript-unicode-input-method)))
 '(python-shell-interpreter "python3")
 '(safe-local-variable-values
   (quote
    ((eval progn
           (git-auto-commit-mode 1)
           (fci-mode 1))
     (eval delete-trailing-whitespace))))
 '(sh-alias-alist
   (quote
    ((csh . tcsh)
     (ksh . pdksh)
     (ksh . ksh88)
     (bash2 . bash)
     (sh5 . sh)
     (nix-shell . zsh))))
 '(sml/mode-width
   (if
       (eq powerline-default-separator
           (quote arrow))
       (quote right)
     (quote full)))
 '(sml/pos-id-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s" powerline-default-separator
                            (car powerline-default-separator-dir)))
                   (quote powerline-active1)
                   (quote powerline-active2))))
     (:propertize " " face powerline-active2))))
 '(sml/pos-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s" powerline-default-separator
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active1)
                   nil)))
     (:propertize " " face sml/global))))
 '(sml/pre-id-separator
   (quote
    (""
     (:propertize " " face sml/global)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s" powerline-default-separator
                            (car powerline-default-separator-dir)))
                   nil
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active2)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s" powerline-default-separator
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active2)
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes)))
 '(sp-use-subword t)
 '(tags-revert-without-query t)
 '(tex-fontify-script nil)
 '(tls-checktrust t)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(verilog-indent-level 2)
 '(verilog-indent-level-behavioral 2)
 '(verilog-indent-level-declaration 2)
 '(verilog-indent-level-directive 2)
 '(verilog-indent-level-module 2)
 '(warning-suppress-types (quote ((\(undo\ discard-info\)))))
 '(wl-address-file "~/.emacs.d/wl/addresses")
 '(wl-alias-file "~/.emacs.d/wl/aliases")
 '(wl-default-spec ".")
 '(wl-folders-file "~/.emacs.d/wl/folders")
 '(wl-from "Remy Goldschmidt <taktoa@gmail.com>")
 '(wl-init-file "~/.emacs.d/wl/settings")
 '(wl-queue-folder ".queue")
 '(wl-score-files-directory "~/.cache/emacs/elmo")
 '(wl-smtp-authenticate-type "plain")
 '(wl-smtp-connection-type (quote starttls))
 '(wl-smtp-posting-port 587)
 '(wl-temporary-file-directory "~/.cache/emacs/wl/tmp")
 '(x-gtk-use-system-tooltips nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(persp-selected-face ((t (:inherit sml/filename :foreground "blue")))))
(put 'downcase-region 'disabled nil)
