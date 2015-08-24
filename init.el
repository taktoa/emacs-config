;;; init.el --- The initial emacs load script
;;; Commentary:
;; Copyright 2014-2015 Remy Goldschmidt / Sebastian Conybeare
;; Provided under the GNU General Public License v3.0
;;; Code:

(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org" user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(create-lockfiles nil)
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e"
     "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
     "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
     default)))
 '(elmo-localdir-folder-path "~/.emacs.d/mail")
 '(elmo-msgdb-directory "~/.emacs.d/elmo")
 '(epa-file-cache-passphrase-for-symmetric-encryption t)
 '(flycheck-pmd-rulesets
   (quote
    ("java-basic"
     "java-design"
     "java-imports"
     "java-braces"
     "java-unusedcode"
     "java-naming"
     "java-optimizations"
     "java-unnecessary"
     "java-sunsecure"
     "java-clone"
     "java-codesize"
     "java-comments"
     "java-coupling"
     "java-typeresolution"
     "java-strictexception"
     "java-strings"
     "java-empty"
     "java-junit")))
 '(gac-automatically-push-p t)
 '(geiser-guile-extra-keywords (quote ("lambda\\*")))
 '(geiser-guile-load-init-file-p t)
 '(haskell-complete-module-preferred
   (quote
    ("Data.ByteString"
     "Data.ByteString.Lazy"
     "Data.Conduit"
     "Data.Function"
     "Data.List"
     "Data.Map"
     "Data.Maybe"
     "Data.Monoid"
     "Data.Ord")))
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
 '(hi2-show-indentations nil)
 '(hs-lint-executable "hlint --ignore='Use camelCase'")
 '(lisp-backquote-indentation nil)
 '(lisp-lambda-list-keyword-alignment t)
 '(lisp-lambda-list-keyword-parameter-alignment t)
 '(lisp-lambda-list-keyword-parameter-indentation 0)
 '(offlineimap-mode-line-text "imap: ")
 '(org-support-shift-select (quote always))
 '(sh-alias-alist
   (quote
    ((csh . tcsh)
     (ksh . pdksh)
     (ksh . ksh88)
     (bash2 . bash)
     (sh5 . sh)
     (nix-shell . zsh))))
 '(tags-revert-without-query t)
 '(warning-suppress-types (quote ((\(undo\ discard-info\)))))
 '(wl-address-file "~/.emacs.d/wl/addresses")
 '(wl-alias-file "~/.emacs.d/wl/aliases")
 '(wl-default-spec ".")
 '(wl-folders-file "~/.emacs.d/wl/folders")
 '(wl-from "Remy Goldschmidt <taktoa@gmail.com>")
 '(wl-init-file "~/.emacs.d/wl/settings")
 '(wl-queue-folder ".queue")
 '(wl-score-files-directory "~/.emacs.d/elmo/")
 '(wl-smtp-authenticate-type "plain")
 '(wl-smtp-connection-type (quote starttls))
 '(wl-smtp-posting-port 587)
 '(wl-temporary-file-directory "~/.emacs.d/wl/tmp/")
 '(x-gtk-use-system-tooltips nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(persp-selected-face ((t (:inherit sml/filename :foreground "blue")))))
(put 'downcase-region 'disabled nil)
(provide 'init)
;;; init.el ends here
