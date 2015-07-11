;;; init.el --- The initial emacs load script
;;; Commentary:
;; Copyright 2014 Remy Goldschmidt
;; Provided under the GNU General Public License v3.0
;;; Code:
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://github.com/dimitri/el-get/raw/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(require 'el-get)

;; now set our own packages
(defvar my-packages
  '(el-get
    cedet
    org-mode
    color-theme-zenburn
    escreen
    helm
    rainbow-delimiters
    smartparens
    yasnippet
    yaml-mode
    markdown-mode
    let-alist
    ace-jump-mode
    flycheck
    flycheck-haskell
    flycheck-pmd
    ;; haskell-flycheck
    flyspell
    magit
    diminish
    tramp
    powerline
    company-mode
    company-ghc
    hi2
    haskell-mode
    ghc-mod
    iedit
    smex
    auctex
    nix-mode
    k3-mode
    rudel
    ledger-mode
    multi-term
    fill-column-indicator
    scala-mode
    purescript-mode
    tuareg
    org-trello
    sr-speedbar
    perspective
    projectile
    persp-projectile
    flx
    flx-ido
    zlc))

;; install new packages and init already installed packages
(el-get 'sync my-packages)

;; remove all packages not listed
(el-get-cleanup my-packages)

;;(require 'cedet-remove-builtin)

;;(require 'k3-mode)

;;(require 'haskell-flycheck)

;;(require 'cedet-devel-load)
;;(require 'ede/generic)
;;(require 'ede/java-root)
;;(require 'ede/jvm-base)
;;(require 'ede/maven2)
;;(require 'semantic/ia)
;;(require 'semantic/db-javap)
;;(semantic-mode 1)
;;(global-ede-mode t)

;;(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
;;(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
;;(global-set-key (kbd "S-C-<down>") 'shrink-window)
;;(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; on to the visual settings
(setq inhibit-splash-screen t)          ; no splash screen, thanks
(line-number-mode 1)                    ; have line numbers and
(column-number-mode 1)                  ; column numbers in the mode line

(tool-bar-mode -1)                      ; no tool bar with icons
(scroll-bar-mode -1)                    ; no scroll bars
(menu-bar-mode -1)

(setq default-frame-alist '((font . "Inconsolata-10")))
;;(setq default-frame-alist '((font . "Inconsolata-dz for Powerline-12")))

(global-subword-mode)

(global-hl-line-mode)                   ; highlight current line
(global-linum-mode 1)                   ; add line numbers on the left
(defvar linum-format)
(setq linum-format "%4d \u2502")

(defvar linum-disable)
(setq linum-disable (lambda () (linum-mode -1)))

(defun hook-select-flycheck-checker (checker)
  "Select a flycheck checker (CHECKER) in a hook."
  `(lambda () (interactive) (flycheck-select-checker ',checker)))

(defun hook-create-dtw-hook ()
  "Deletes trailing whitespace on save in a hook."
  '(lambda ()
     (add-hook 'write-file-hooks '(lambda ()
                                    (interactive)
                                    (delete-trailing-whitespace)))))

(add-hook 'term-mode-hook                linum-disable)
(add-hook 'multi-term-mode-hook          linum-disable)
(add-hook 'haskell-interactive-mode-hook linum-disable)
(add-hook 'java-mode-hook                (hook-select-flycheck-checker 'java-pmd))
(add-hook 'java-mode-hook                (hook-create-dtw-hook))

(cua-mode)

(sr-speedbar-open)
;;(zlc-mode)

(load-theme 'zenburn t)

(windmove-default-keybindings 'meta)
(defvar windmove-wrap-around)
(setq windmove-wrap-around t)

(setq x-select-enable-clipboard t)

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )

(require 'term)
(define-key term-raw-map  (kbd "C-'") 'term-line-mode)
(define-key term-mode-map (kbd "C-'") 'term-char-mode)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'smartparens-config)
(setq-default indent-tabs-mode nil)

(setq-default cursor-type 'bar)

(setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups"))))

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

(require 'powerline)
(powerline-default-theme)

(global-unset-key (kbd "C-<next>"))
(global-set-key (kbd "C-<next>") 'scroll-down-command)
(global-set-key (kbd "C-<prior>") 'scroll-up-command)
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-u") 'yank)
(global-set-key (kbd "C-x C-k") 'kill-buffer)
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key [f5] 'compile)
(global-set-key [f6] 'recompile)

(persp-mode)
(persp-turn-on-modestring)
(global-unset-key (kbd "C-a"))
(global-set-key (kbd "C-a s") 'persp-switch)
(global-set-key (kbd "C-a b") 'persp-add-buffer)
(global-set-key (kbd "C-a a") 'persp-rename)
(global-set-key (kbd "C-a k") 'persp-kill)
(global-set-key (kbd "C-a C-s") 'persp-switch)
(global-set-key (kbd "C-a C-b") 'persp-add-buffer)
(global-set-key (kbd "C-a C-a") 'persp-rename)
(global-set-key (kbd "C-a C-k") 'persp-kill)

(electric-indent-mode +1)

(setq-default flycheck-emacs-lisp-load-path 'inherit)

(turn-on-haskell-simple-indent)

(global-company-mode)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "<menu>") 'smex)
(global-set-key (kbd "<menu>") 'smex-major-mode-commands)

(require 'smartparens-config)
(smartparens-global-mode)

(defun conditionally-enable-paredit-mode ()
  "Enable `paredit-mode' in the minibuffer, during `eval-expression'."
  (when (eq this-command 'eval-expression) (smartparens-mode)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

;;(defadvice haskell-mode-stylish-buffer (around skip-if-flycheck-errors activate)
;;  "Add haskell-stylish to haskell-mode."
;;  (unless (flycheck-has-current-errors-p 'error)
;;    ad-do-it))

(add-hook 'haskell-mode-hook 'turn-on-hi2)

(global-flycheck-mode)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(create-lockfiles nil)
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(ede-project-directories (quote ("/home/remy/Documents/ResearchWork/KHaskell/k")))
 '(flycheck-pmd-rulesets
   (quote
    ("java-basic" "java-design" "java-imports" "java-braces" "java-unusedcode" "java-naming" "java-optimizations" "java-unnecessary" "java-sunsecure" "java-clone" "java-codesize" "java-comments" "java-coupling" "java-typeresolution" "java-strictexception" "java-strings" "java-empty" "java-junit")))
 '(haskell-complete-module-preferred
   (quote
    ("Data.ByteString" "Data.ByteString.Lazy" "Data.Conduit" "Data.Function" "Data.List" "Data.Map" "Data.Maybe" "Data.Monoid" "Data.Ord")))
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
 '(hi2-show-indentations nil)
 '(hs-lint-executable "hlint --ignore='Use camelCase'")
 '(sh-alias-alist
   (quote
    ((csh . tcsh)
     (ksh . pdksh)
     (ksh . ksh88)
     (bash2 . bash)
     (sh5 . sh)
     (nix-shell . zsh)))))

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(require 'haskell-interactive-mode)
(define-key haskell-interactive-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)

(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "M-[") 'align)

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(provide 'init)
;;; init.el ends here
(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
