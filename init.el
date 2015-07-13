;;; init.el --- The initial emacs load script
;;; Commentary:
;; Copyright 2014-2015 Remy Goldschmidt / Sebastian Conybeare
;; Provided under the GNU General Public License v3.0
;;; Code:


;; --------------------------------------------------------------------------------
;; ----------------------------------- packages -----------------------------------
;; --------------------------------------------------------------------------------


(defvar my-packages
  '(el-get
    cedet
    org-mode
    color-theme-solarized
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
    flyspell
    magit
    diminish
    tramp
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
    delight
    powerline
    smart-mode-line
    smart-mode-line-powerline-theme
    scala-mode
    undo-tree
    zlc))


;; --------------------------------------------------------------------------------
;; --------------------------------- el-get setup ---------------------------------
;; --------------------------------------------------------------------------------


(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://github.com/dimitri/el-get/raw/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(defvar el-get-recipe-path)
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(require 'el-get)

(el-get 'sync my-packages)
(el-get-cleanup my-packages)


;; --------------------------------------------------------------------------------
;; ----------------------------- require misc modules -----------------------------
;; --------------------------------------------------------------------------------


(require 'flycheck)
(require 'term)
(require 'rainbow-delimiters)
(require 'smartparens-config)
(require 'powerline)
(require 'smart-mode-line)
(require 'smex)
(require 'company)
(require 'haskell-mode)
(require 'haskell-interactive-mode)
(require 'haskell-simple-indent)
(require 'hi2)
(require 'ace-jump-mode)
(require 'helm)
(require 'sr-speedbar)
(require 'perspective)
(require 'lilypond-mode) ;; (CONTROVERSIAL)


;; --------------------------------------------------------------------------------
;; ----------------------------------- themeing -----------------------------------
;; --------------------------------------------------------------------------------


;; Disable various annoyances that come with Emacs
(setq inhibit-splash-screen t)
(column-number-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Set font to Inconsolata (CONTROVERSIAL)
(setq default-frame-alist '((font . "Inconsolata-10")))

;; Line numbers
(line-number-mode 1)
(global-hl-line-mode)
(global-linum-mode 1)
(defvar linum-format)
(setq linum-format "%4d \u2502")
(defvar linum-disable (lambda () (linum-mode -1)))

;; Disable line numbers for various modes
(add-hook 'term-mode-hook                linum-disable)
(add-hook 'multi-term-mode-hook          linum-disable)
(add-hook 'haskell-interactive-mode-hook linum-disable)
(add-hook 'sr-speedbar-mode-hook         linum-disable)
(add-hook 'speedbar-mode-hook            linum-disable)

;; Enable zenburn theme
(load-theme 'zenburn t)

;; Thin cursor (CONTROVERSIAL)
(setq-default cursor-type 'bar)

;; Enable Powerline modeline
(powerline-default-theme)

;; Smart mode line
(defvar sml/no-confirm-load-theme)
(setq sml/no-confirm-load-theme t)
(sml/setup)


;; --------------------------------------------------------------------------------
;; ------------------------------------ editing -----------------------------------
;; --------------------------------------------------------------------------------


;; Move by subword in CamelCase
(global-subword-mode)

;; Auto-revert buffers every so often
(global-auto-revert-mode)
(defvar auto-revert-check-vc-info t)

;; Smarter editing with matching delimiters
(smartparens-global-mode)

;; On-the-fly syntax checking
(global-flycheck-mode)
(setq-default flycheck-emacs-lisp-load-path 'inherit)

;; Add multiple "perspectives" for buffers (i.e.: workspaces)
(persp-mode)
(persp-turn-on-modestring)

;; Indent automagically
(electric-indent-mode +1)

;; Better indenting for Haskell
(turn-on-haskell-simple-indent)

;; Better autocompletion
(global-company-mode)

;; Better minibuffer autocompletion
(smex-initialize)

;; In-buffer project browser
(sr-speedbar-open)

;; Allow X11 copy-and-paste into buffers
(setq x-select-enable-clipboard t)

;; Disable indenting with tabs by default
(setq-default indent-tabs-mode nil)

;; Enable undo-tree (CONTROVERSIAL)
;;(require 'undo-tree)
;;(global-undo-tree-mode)

;; Scroll compilation output
(setq-default compilation-scroll-output t)

;; Set C tab width to 4
(defvar c-default-style "linux")
(setq-default c-basic-offset 4
              tab-width 4)


;; --------------------------------------------------------------------------------
;; ---------------------------------- keybindings ---------------------------------
;; --------------------------------------------------------------------------------


;; Fix Ctrl-PgUp and Ctrl-PgDown weirdness
(global-unset-key (kbd "C-<next>"))
(global-set-key (kbd "C-<next>") 'scroll-down-command)
(global-set-key (kbd "C-<prior>") 'scroll-up-command)

;; Fix C-x C-k and C-x f not being the same as C-x k and C-x C-f respectively
(global-set-key (kbd "C-x C-k") 'kill-buffer)
(global-set-key (kbd "C-x f") 'find-file)

;; Nano-style line killing/yanking (CONTROVERSIAL)
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-u") 'yank)

;; Useful shortcuts for compile
(global-set-key [f5] 'compile)
(global-set-key [f6] 'recompile)

;; Resize windows with super + arrow keys
(global-set-key (kbd "s-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-<down>")  'shrink-window)
(global-set-key (kbd "s-<up>")    'enlarge-window)

;; Switch windows with meta + arrow keys
(windmove-default-keybindings 'meta)
(defvar windmove-wrap-around t)

;; Free up C-a for keybindings (CONTROVERSIAL)
(global-unset-key (kbd "C-a"))

;; GNU screen-style keybindings for persp-mode
(global-set-key (kbd "C-a s") 'persp-switch)
(global-set-key (kbd "C-a b") 'persp-add-buffer)
(global-set-key (kbd "C-a a") 'persp-rename)
(global-set-key (kbd "C-a k") 'persp-kill)
(global-set-key (kbd "C-a C-s") 'persp-switch)
(global-set-key (kbd "C-a C-b") 'persp-add-buffer)
(global-set-key (kbd "C-a C-a") 'persp-rename)
(global-set-key (kbd "C-a C-k") 'persp-kill)

;; Enable smex on M-x, M-X, and <menu>
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "<menu>") 'smex)

;; Enable CUA keybindings (CONTROVERSIAL)
(cua-mode)

;; Switch between line and char mode in term with C-' (CONTROVERSIAL)
(define-key term-raw-map  (kbd "C-'") 'term-line-mode)
(define-key term-mode-map (kbd "C-'") 'term-char-mode)

;; Misc keybindings
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "M-[") 'align)
(define-key haskell-interactive-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)


;; --------------------------------------------------------------------------------
;; ---------------------------- hook utility functions ----------------------------
;; --------------------------------------------------------------------------------


(defun hook-select-flycheck-checker (checker)
  "Select a flycheck checker (CHECKER) in a hook."
  `(lambda () (interactive) (flycheck-select-checker ',checker)))

(defun create-dtw-hook ()
  "Deletes trailing whitespace on save in a hook."
  '(lambda () (add-hook 'write-contents-functions
                        (lambda () (save-excursion (delete-trailing-whitespace))))))

(defun create-untabify-hook ()
  "Untabifies on save in a hook."
  '(lambda () (add-hook 'write-contents-functions
                        (lambda () (save-excursion (untabify))))))

(defun minibuffer-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
  '(lambda () (when (eq this-command 'eval-expression) (smartparens-mode))))


;; --------------------------------------------------------------------------------
;; ------------------------------------- hooks ------------------------------------
;; --------------------------------------------------------------------------------


(add-hook 'java-mode-hook                (hook-select-flycheck-checker 'java-pmd))
(add-hook 'java-mode-hook                (create-dtw-hook))
(add-hook 'haskell-mode-hook             (create-dtw-hook))
(add-hook 'lisp-mode-hook                (create-dtw-hook))
(add-hook 'minibuffer-setup-hook         (minibuffer-smartparens-mode))
(add-hook 'prog-mode-hook                'rainbow-delimiters-mode)
(add-hook 'haskell-mode-hook             'turn-on-hi2)
(add-hook 'haskell-mode-hook             'interactive-haskell-mode)
(add-hook 'flycheck-mode-hook            'flycheck-haskell-setup)


;; --------------------------------------------------------------------------------
;; --------------------------- general utility functions --------------------------
;; --------------------------------------------------------------------------------


(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )


;; --------------------------------------------------------------------------------
;; ------------------------------- autosave/tempfiles -----------------------------
;; --------------------------------------------------------------------------------


;; Autosave into ~/.emacs.d/backups
(setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups"))))

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)


;; --------------------------------------------------------------------------------
;; --------------------------------- miscellaneous --------------------------------
;; --------------------------------------------------------------------------------


(put 'upcase-region 'disabled nil)

;; (setq exec-path (append exec-path '("~/.emacs.d/bin"))) ;; (CONTROVERSIAL)


;; --------------------------------------------------------------------------------
;; -------------------------------- custom variables ------------------------------
;; --------------------------------------------------------------------------------


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(create-lockfiles nil)
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
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
     (nix-shell . zsh))))
 '(tags-revert-without-query t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(persp-selected-face ((t (:inherit sml/filename :foreground "blue")))))

(provide 'init)
;;; init.el ends here
