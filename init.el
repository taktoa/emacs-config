(require 'org)
(org-babel-load-file
(expand-file-name "settings.org" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-auto-untabify t)
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(create-lockfiles nil)
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "870a63a25a2756074e53e5ee28f3f890332ddc21f9e87d583c5387285e882099" "79a3f477ac0cb4a106f78b6109614e991564a5c2467c36e6e854d4bc1102e178" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
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
 '(haskell-doc-chop-off-context nil)
 '(haskell-doc-show-global-types t)
 '(haskell-doc-use-inf-haskell t)
 '(haskell-interactive-mode-eval-mode (quote ignore))
 '(haskell-interactive-mode-eval-pretty nil)
 '(haskell-interactive-mode-include-file-name nil)
 '(haskell-interactive-mode-scroll-to-bottom t)
 '(haskell-mode-hook
   (quote
    (interactive-haskell-mode turn-on-haskell-indentation turn-on-haskell-doc-mode)) t)
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
 '(haskell-tags-on-save t)
 '(helm-autoresize-max-height 30)
 '(hi2-show-indentations nil)
 '(hs-lint-executable "hlint --ignore='Use camelCase'")
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
 '(org-src-fontify-natively t)
 '(org-support-shift-select (quote always))
 '(purescript-mode-hook
   (quote
    (turn-on-eldoc-mode turn-on-purescript-indent turn-on-purescript-indentation turn-on-purescript-unicode-input-method)))
 '(python-shell-interpreter "python3")
 '(safe-local-variable-values (quote ((eval delete-trailing-whitespace))))
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
