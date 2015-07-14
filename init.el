;;; init.el --- The initial emacs load script
;;; Commentary:
;; Copyright 2014-2015 Remy Goldschmidt / Sebastian Conybeare
;; Provided under the GNU General Public License v3.0
;;; Code:


;; --------------------------------------------------------------------------------
;; ---------------------------------- constants -----------------------------------
;; --------------------------------------------------------------------------------


(defvar init-profiling-enabled nil) ;; Enable profiling
(defvar init-debug-enabled nil)     ;; Enable debug messages
(defvar init-errors-enabled t)      ;; Enable errors

;; Test url for checking internet access
(defvar init-test-url (url-encode-url "http://ipecho.net/plain"))

;;(defvar init-extra-exec-path '("~/.emacs.d/bin")) ;; CONTROVERSIAL
(defvar init-extra-exec-path nil)


;; --------------------------------------------------------------------------------
;; ------------------------------- global variables -------------------------------
;; --------------------------------------------------------------------------------


(defvar my-capabilities nil)
(defvar my-options nil)
(defvar my-packages nil)


;; --------------------------------------------------------------------------------
;; --------------------------- global accessors/mutators --------------------------
;; --------------------------------------------------------------------------------


(defun add-cap (cap &optional disabled)
  "Add CAP to the set of existing capabilities.
If DISABLED is true, do nothing."
  (unless disabled
    (unless (capabilityp cap)
      (push cap my-capabilities))))

(defun capabilityp (cap)
  "Is the given CAP an available capability?"
  (member cap my-capabilities))

(defun capabilitiesp (caps)
  "Are the given CAPS available capabilities?"
  (catch 'return
    (dolist (element caps)
      (unless (capabilityp element)
        (throw 'return nil)))
    (throw 'return t)))


(defun add-opt (opt &optional disabled)
  "Add OPT to the set of existing options.
If DISABLED is true, do nothing."
  (unless disabled
    (unless (optionp opt)
      (push opt my-options))))

(defun optionp (opt)
  "Is the given OPT an available option?"
  (member opt my-options))

(defun optionsp (opts)
  "Are the given OPTS available options?"
  (catch 'return
    (dolist (element opts)
      (unless (optionp element)
        (throw 'return nil)))
    (throw 'return t)))


(defun add-package (package)
  "Add the given PACKAGE to `my-packages'."
  (setq my-packages (append my-packages (list package))))

(defun add-to-packages (packages)
  "Add the given PACKAGES to `my-packages'."
  (dolist (element packages) (add-package element)))


;; --------------------------------------------------------------------------------
;; ------------------------------ utility functions -------------------------------
;; --------------------------------------------------------------------------------


(defun decrement (symbol)
  "Decrement the value held in the variable named SYMBOL."
  (let ((value (symbol-value symbol)))
    (if (numberp value)
        (set symbol (- value 1))
      (error "Wrong argument: decrement takes a numeric variable symbol"))))

(defun increment (symbol)
  "Increment the value held in the variable named SYMBOL."
  (let ((value (symbol-value symbol)))
    (if (numberp value)
        (set symbol (+ value 1))
      (error "Wrong argument: increment takes a numeric variable symbol"))))

(defun profile-function (function &optional args)
  "Profile a given FUNCTION symbol with optional ARGS."
  (unless (symbolp function)
    (error "Wrong argument: in profile-function, FUNCTION is a symbol"))
  (unless (listp args)
    (error "Wrong argument: in profile-function, ARGS is a list"))
  (let ((elp-function-list (list function)))
    (elp-instrument-list)
    (apply function args)
    (elp-results)
    (elp-reset-list)))

(defun profile-functions (functions)
  "Profile the given FUNCTIONS (called with no arguments)."
  (unless (listp functions)
    (error "Wrong argument: in profile-functions, FUNCTIONS is a list"))
  (let ((elp-function-list functions))
    (elp-instrument-list)
    (dolist (element functions)
      (apply element nil))
    (elp-results)
    (elp-reset-list)))

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )

(defun wait-on-variable (symbol resolution timeout)
  "Wait for the value of a variable to become non-nil.
Specifically, check if the value held in SYMBOL is non-nil
every RESOLUTION milliseconds until the TIMEOUT, also measured
in milliseconds.
If the timeout is reached, return nil.
Otherwise, return the number of milliseconds that passed."
  (catch 'return
    (let ((counter 0))
      (while (not (symbol-value symbol))
        (when (> (* counter resolution) timeout) (throw 'return nil))
        (sleep-for 0 resolution)
        (increment 'counter))
      (throw 'return (* counter resolution)))))

(defun get-url (url callback)
  "Retrieve a URL and run the CALLBACK on the parsed results.
Returns nil if the request did not lead to an HTTP response
CALLBACK is called with (apply CALLBACK STATUS HEADERS BODY).
STATUS is the HTTP status code, e.g.: `200' for HTTP OK
HEADERS is a list of cons cells containing a map from a header name
to its value (e.g.: '((\"Content-Type\" . \"text/html\"))).
BODY is the text of the body (e.g.: the HTML of the page)."
  (catch 'return
    (let* ((download-done nil)
           (download-timeout 1000)
           (encoded-url (url-encode-url url))
           (url-callback (lambda (status) (setq download-done t)))
           (response-buffer
            (ignore-errors (url-retrieve encoded-url url-callback nil t t)))
           (response (progn (unless response-buffer (throw 'return nil))
                            (with-current-buffer response-buffer
                              (wait-on-variable 'download-done 50
                                                download-timeout)
                                (buffer-string))))
           (split-response (split-string response "\n"))
           (status nil) (headers nil) (body nil)
           (status-regex (rx (and "HTTP/" (and digit "." digit)
                                  space (group (and digit digit digit))
                                  space (one-or-more (or letter space))
                                  line-end)))
           (header-regex (rx (and (group (one-or-more (or letter "-")))
                                  ": " (group (one-or-more not-newline))
                                  line-end)))
           (process-status (lambda (s)
                             (string-match status-regex s)
                             (match-string 1 s)))
           (process-header (lambda (h)
                             (string-match header-regex h)
                             (cons (match-string 1 h) (match-string 2 h)))))
      (setq status (apply process-status (list (pop split-response))))
      (while (not (string-equal (car split-response) ""))
        (push (apply process-header (list (pop split-response))) headers))
      (pop split-response)
      (setq body (mapconcat (lambda (x) x) split-response "\n"))
      (throw 'return (apply callback (list status headers body))))))

(defun url-resolves-p (url)
  "Test whether the given URL resolves."
  (get-url url (lambda (status headers body)
                 (let ((s1 (car (string-to-list status))))
                   (cond ((= s1 ?1) t)
                         ((= s1 ?2) t)
                         ((= s1 ?3) t)
                         ((= s1 ?4) t)
                         (t status))))))

(defun executable-exists-p (exec-name)
  "Check if the given executable (EXEC-NAME) exists."
  (unless (stringp exec-name)
    (error "Wrong argument: executable-exists-p takes a string"))
  (executable-find exec-name))

(defun lucid-emacs-p ()
  "Return non-nil if you are running Lucid Emacs."
  (string-match "XEmacs\\|Lucid" emacs-version))


;; --------------------------------------------------------------------------------
;; ----------------------------- check prerequisites ------------------------------
;; --------------------------------------------------------------------------------


(defun init-check-prerequisites ()
  "Check prerequisites for initialization."
  (setq exec-path (append exec-path init-extra-exec-path))
  
  (unless (executable-exists-p "git")
    (error "Git is not installed")))


;; --------------------------------------------------------------------------------
;; ---------------------------- autodetect capabilities ---------------------------
;; --------------------------------------------------------------------------------



(defun init-detect-system-capabilities ()
  "Detect the current system.  Run this first."
  (when (url-resolves-p init-test-url)      (add-cap "internet"))

  (cond ((eq system-type 'gnu/linux)        (add-cap "system-linux"))
        ((eq system-type 'darwin)           (add-cap "system-osx"))
        ((eq system-type 'windows-nt)       (add-cap "system-windows"))
        ((eq system-type 'ms-dos)           (add-cap "system-msdos"))
        ((eq system-type 'cygwin)           (add-cap "system-cygwin"))
        ((eq system-type 'gnu)              (add-cap "system-unix"))
        ((eq system-type 'gnu/kfreebsd)     (add-cap "system-unix"))
        ((eq system-type 'aix)              (add-cap "system-unix"))
        ((eq system-type 'irix)             (add-cap "system-unix"))
        ((eq system-type 'hpux)             (add-cap "system-unix"))
        ((eq system-type 'usg-unix-v)       (add-cap "system-unix"))
        ((eq system-type 'berkeley-unix)    (add-cap "system-unix"))
        (t                                  (add-cap "system-unknown")))
  (when (or (capabilityp "system-unix")
            (capabilityp "system-linux")
            (capabilityp "system-cygwin")
            (capabilityp "system-osx"))     (add-cap "system-posix")))

(defun init-detect-graphical-capabilities ()
  "Detect graphical and font capabilities."
  (when (display-graphic-p)                 (add-cap "graphics"))

  (when (capabilityp "graphics")
    (when (capabilityp "system-linux")      (add-cap "graphics-x11"))
    (when (capabilityp "system-unix")       (add-cap "graphics-x11"))
    (when (capabilityp "system-windows")    (add-cap "graphics-w32"))
    (when (capabilityp "system-osx")        (add-cap "graphics-osx")))

  (when (capabilityp "graphics")
    (when (x-list-fonts "Inconsolata")      (add-cap "font-inconsolata"))
    (when (x-list-fonts "Menlo")            (add-cap "font-menlo"))
    (when (x-list-fonts "Meslo")            (add-cap "font-meslo"))
    (when (x-list-fonts "DejaVu Sans Mono") (add-cap "font-dejavu"))
    (when (x-list-fonts "FreeMono")         (add-cap "font-freefont"))
    (when (x-list-fonts "Liberation Mono")  (add-cap "font-liberation"))
    (when (x-list-fonts "Source Code Pro")  (add-cap "font-sourcecodepro"))
    (when (x-list-fonts "Luxi Mono")        (add-cap "font-luxi"))
    (when (x-list-fonts "Consolas")         (add-cap "font-consolas"))))

(defun init-detect-emacs-capabilities ()
  "Detect the version of Emacs running."
  (cond ((lucid-emacs-p)                    (add-cap "emacs-lucid"))
        (t                                  (add-cap "emacs-gnu"))))

(defun init-detect-vcs-capabilities ()
  "Detect available VCS systems."
  (when (executable-exists-p "git")         (add-cap "vcs-git"))
  (when (executable-exists-p "cvs")         (add-cap "vcs-cvs"))
  (when (executable-exists-p "svn")         (add-cap "vcs-svn"))
  (when (executable-exists-p "hg")          (add-cap "vcs-hg"))
  (when (executable-exists-p "darcs")       (add-cap "vcs-darcs"))
  (when (executable-exists-p "bzr")         (add-cap "vcs-bzr")))

(defun init-detect-build-capabilities ()
  "Detect available build tools."
  (when (capabilityp "system-windows")      (add-cap "build-sln"))
  (when (executable-exists-p "make")        (add-cap "build-make"))
  (when (executable-exists-p "cmake")       (add-cap "build-make"))
  (when (executable-exists-p "latexmk")     (add-cap "build-latexmk"))
  (when (executable-exists-p "ant")         (add-cap "build-ant"))
  (when (executable-exists-p "mvn")         (add-cap "build-maven"))
  (when (executable-exists-p "gradle")      (add-cap "build-gradle"))
  (when (executable-exists-p "sbt")         (add-cap "build-sbt"))
  (when (executable-exists-p "cask")        (add-cap "build-cask"))
  (when (executable-exists-p "lein")        (add-cap "build-leiningen")))

(defun init-detect-package-manager-capabilities ()
  "Detect available package management systems."
  (when (executable-exists-p "apt-get")     (add-cap "package-apt"))
  (when (executable-exists-p "emerge")      (add-cap "package-emerge"))
  (when (executable-exists-p "pacman")      (add-cap "package-pacman"))
  (when (executable-exists-p "yum")         (add-cap "package-yum"))
  (when (executable-exists-p "ipkg")        (add-cap "package-ipkg"))
  (when (executable-exists-p "opkg")        (add-cap "package-opkg"))
  (when (executable-exists-p "pkcon")       (add-cap "package-pkcon"))
  (when (executable-exists-p "nix-env")     (add-cap "package-nix"))
  (when (executable-exists-p "brew")        (add-cap "package-brew"))
  (when (executable-exists-p "wpkg")        (add-cap "package-wpkg"))
  (when (executable-exists-p "0install")    (add-cap "package-0install"))

  (when (executable-exists-p "cpan")        (add-cap "package-cpan"))
  (when (executable-exists-p "pip")         (add-cap "package-pip"))
  (when (executable-exists-p "gem")         (add-cap "package-gem"))
  (when (executable-exists-p "bower")       (add-cap "package-bower"))
  (when (executable-exists-p "npm")         (add-cap "package-npm"))
  (when (executable-exists-p "cabal")       (add-cap "package-cabal"))
  (when (executable-exists-p "stack")       (add-cap "package-stack"))
  (when (executable-exists-p "opam")        (add-cap "package-opam"))
  (when (executable-exists-p "rebar")       (add-cap "package-rebar"))
  (when (executable-exists-p "cargo")       (add-cap "package-cargo"))
  (when (executable-exists-p "mingw-get")   (add-cap "package-mingw"))
  (when (executable-exists-p "composer")    (add-cap "package-composer"))
  (when (executable-exists-p "dub")         (add-cap "package-dub")))

(defun init-detect-dsl-lang-capabilities ()
  "Detect available domain specific language compilers/interpreters."
  (when (capabilityp "system-osx")          (add-cap "lang-applescript"))
  (when (capabilityp "system-windows")      (add-cap "lang-powershell"))
  (when (capabilityp "system-windows")      (add-cap "lang-msdos"))
  (when (executable-exists-p "httpd")       (add-cap "lang-apache"))
  (when (executable-exists-p "nagios")      (add-cap "lang-nagios"))
  (when (executable-exists-p "nginx")       (add-cap "lang-nginx"))
  (when (executable-exists-p "puppet")      (add-cap "lang-puppet"))
  (when (executable-exists-p "syslogd")     (add-cap "lang-syslog"))
  (when (executable-exists-p "syslog-ng")   (add-cap "lang-syslog"))
  (when (executable-exists-p "systemctl")   (add-cap "lang-syslog"))
  (when (executable-exists-p "systemctl")   (add-cap "lang-systemd"))
  (when (executable-exists-p "varnishd")    (add-cap "lang-varnish"))
  (when (executable-exists-p "cron")        (add-cap "lang-crontab"))
  (when (executable-exists-p "anacron")     (add-cap "lang-crontab"))

  (when (executable-exists-p "arduino")     (add-cap "lang-arduino"))
  (when (executable-exists-p "iverilog")    (add-cap "lang-verilog"))

  (when (executable-exists-p "blender")     (add-cap "lang-blender-python"))
  (when (executable-exists-p "povray")      (add-cap "lang-povray"))

  (when (executable-exists-p "bison")       (add-cap "lang-bison"))
  (when (executable-exists-p "lex")         (add-cap "lang-bison"))
  (when (executable-exists-p "flex")        (add-cap "lang-bison"))
  (when (executable-exists-p "yacc")        (add-cap "lang-bison"))
  (when (executable-exists-p "fortune")     (add-cap "lang-fortune"))
  (when (executable-exists-p "gengetopt")   (add-cap "lang-gengetopt"))
  (when (executable-exists-p "gettext")     (add-cap "lang-po"))
  (when (executable-exists-p "mutt")        (add-cap "lang-muttrc"))

  (when (executable-exists-p "docker")      (add-cap "lang-docker"))
  (when (executable-exists-p "gyp")         (add-cap "lang-gyp"))
  (when (executable-exists-p "gnuplot")     (add-cap "lang-gnuplot"))
  (when (executable-exists-p "dot")         (add-cap "lang-graphviz"))

  (when (executable-exists-p "bc")          (add-cap "lang-bc"))
  (when (executable-exists-p "matlab")      (add-cap "lang-matlab")) ;; not sure
  (when (executable-exists-p "sage")        (add-cap "lang-sage"))
  (when (executable-exists-p "R")           (add-cap "lang-r"))

  (when (executable-exists-p "qmake")       (add-cap "lang-qmake"))
  (when (executable-exists-p "qtdiag")      (add-cap "lang-qml"))

  (when (executable-exists-p "latex")       (add-cap "lang-latex"))
  (when (executable-exists-p "lilypond")    (add-cap "lang-lilypond"))
  (when (executable-exists-p "ledger")      (add-cap "lang-ledger"))
  (when (executable-exists-p "hledger")     (add-cap "lang-ledger")))

(defun init-detect-imperative-lang-capabilities ()
  "Detect available imperative compilers/interpreters."
  (when (executable-exists-p "gcc")         (add-cap "lang-c"))
  (when (executable-exists-p "clang")       (add-cap "lang-c"))
  (when (executable-exists-p "g++")         (add-cap "lang-c++"))
  (when (executable-exists-p "clang++")     (add-cap "lang-c++"))
  (when (executable-exists-p "gnat")        (add-cap "lang-ada"))
  (when (executable-exists-p "gfortran")    (add-cap "lang-fortran"))
  (when (executable-exists-p "gccgo")       (add-cap "lang-go"))
  (when (executable-exists-p "llc")         (add-cap "lang-llvm"))
  (when (executable-exists-p "rustc")       (add-cap "lang-rust"))
  (when (executable-exists-p "nim")         (add-cap "lang-nim"))
  (when (executable-exists-p "vala")        (add-cap "lang-vala"))
  (when (executable-exists-p "php")         (add-cap "lang-php"))
  (when (executable-exists-p "mono")        (add-cap "lang-csharp"))
  (when (capabilityp "system-windows")      (add-cap "lang-csharp")))

(defun init-detect-jvm-lang-capabilities ()
  "Detect available JVM compilers/interpreters."
  (when (executable-exists-p "java")        (add-cap "lang-jvm"))
  (when (executable-exists-p "javac")       (add-cap "lang-java"))
  (when (executable-exists-p "gcj")         (add-cap "lang-java"))
  (when (executable-exists-p "groovy")      (add-cap "lang-groovy"))
  (when (executable-exists-p "lein")        (add-cap "lang-clojure"))
  (when (executable-exists-p "sbt")         (add-cap "lang-scala"))
  (when (executable-exists-p "scalac")      (add-cap "lang-scala")))

(defun init-detect-scripting-lang-capabilities ()
  "Detect available scripting language compilers/interpreters."
  (when (executable-exists-p "python")      (add-cap "lang-python"))
  (when (executable-exists-p "ruby")        (add-cap "lang-ruby"))
  (when (executable-exists-p "perl")        (add-cap "lang-perl"))
  (when (executable-exists-p "julia")       (add-cap "lang-julia"))
  (when (executable-exists-p "jc")          (add-cap "lang-j"))
  (when (executable-exists-p "io")          (add-cap "lang-io"))
  (when (executable-exists-p "bash")        (add-cap "lang-bash"))
  (when (executable-exists-p "zsh")         (add-cap "lang-zsh")))

(defun init-detect-web-lang-capabilities ()
  "Detect available web language compilers/interpreters."
  (when (executable-exists-p "lessc")       (add-cap "lang-less"))
  (when (executable-exists-p "sassc")       (add-cap "lang-sass"))
  (when (executable-exists-p "opa")         (add-cap "lang-opa"))
  (when (executable-exists-p "node")        (add-cap "lang-node"))
  (when (executable-exists-p "psc")         (add-cap "lang-purescript"))
  (when (executable-exists-p "lsc")         (add-cap "lang-livescript"))
  (when (executable-exists-p "coffee")      (add-cap "lang-coffeescript")))

(defun init-detect-functional-lang-capabilities ()
  "Detect available functional language compilers/interpreters."
  (when (executable-exists-p "ocamlc")      (add-cap "lang-ocaml"))
  (when (executable-exists-p "erlc")        (add-cap "lang-erlang"))
  (when (executable-exists-p "ghc")         (add-cap "lang-haskell"))
  (when (executable-exists-p "idris")       (add-cap "lang-idris"))
  (when (executable-exists-p "elm")         (add-cap "lang-elm"))
  (when (executable-exists-p "nix-build")   (add-cap "lang-nix"))
  (when (executable-exists-p "jc")          (add-cap "lang-j"))
  (when (executable-exists-p "newlisp")     (add-cap "lang-newlisp"))
  (when (executable-exists-p "racket")      (add-cap "lang-racket"))
  (when (executable-exists-p "lua")         (add-cap "lang-lua"))
  (when (executable-exists-p "shen")        (add-cap "lang-shen"))
  (when (executable-exists-p "sml")         (add-cap "lang-sml"))
  (when (executable-exists-p "polyc")       (add-cap "lang-sml"))
  (when (executable-exists-p "mlton")       (add-cap "lang-sml"))
  (when (executable-exists-p "swift")       (add-cap "lang-swift"))
  (when (executable-exists-p "fsharpc")     (add-cap "lang-fsharp"))
  (when (executable-exists-p "kompile")     (add-cap "lang-kframework")))

(defun init-detect-misc-capabilities ()
  "Detect miscellaneous capabilities."
  (when (executable-exists-p "pandoc")      (add-cap "util-pandoc"))
  (when (executable-exists-p "pmd")         (add-cap "util-pmd"))
  (when (executable-exists-p "top")         (add-cap "util-top")))

(defun init-detect-all-capabilities ()
  "Run all the detect-*-capabilities functions."
  (init-detect-system-capabilities)
  (init-detect-graphical-capabilities)
  (init-detect-emacs-capabilities)
  (init-detect-vcs-capabilities)
  (init-detect-build-capabilities)
  (init-detect-package-manager-capabilities)
  (init-detect-dsl-lang-capabilities)
  (init-detect-imperative-lang-capabilities)
  (init-detect-jvm-lang-capabilities)
  (init-detect-scripting-lang-capabilities)
  (init-detect-web-lang-capabilities)
  (init-detect-functional-lang-capabilities)
  (init-detect-misc-capabilities))

(defun init-detect-capabilities ()
  "Detect available capabilities."
  (init-detect-all-capabilities)

  (when (eq nil my-capabilities) (error "Capability detection failed"))

  (message "Available capabilities: %S" my-capabilities))


;; --------------------------------------------------------------------------------
;; ----------------------------------- options ------------------------------------
;; --------------------------------------------------------------------------------



;; EDIT THESE TO YOUR SATISFACTION

(defun init-customize-options ()
  "Initialize the my-options variable with your customizations."
  (add-opt "ace-jump-mode"         nil)
  (add-opt "speedbar"              nil)
  (add-opt "projectile"            nil)
  (add-opt "perspective"           nil)
  (add-opt "autorevert"            nil)
  (add-opt "company"               nil)
  (add-opt "flycheck"              nil)
  (add-opt "smex"                  nil)
  (add-opt "helm"                  nil)
  (add-opt "rudel"                 t)

  (add-opt "org"                   nil)
  (add-opt "markdown"              nil)
  (add-opt "yaml"                  nil)

  (add-opt "iedit"                 nil)
  (add-opt "fill-column-indicator" nil)
  (add-opt "multi-term"            nil)
  (add-opt "rainbow-delimiters"    nil)

  (add-opt "smartparens"           nil)

  (add-opt "electric-indent"       nil)
  (add-opt "haskell-simple-indent" nil)

  (add-opt "fix-emacs-cruft"       nil)

  (add-opt "line-numbers"          nil)
  (add-opt "powerline"             nil)
  (add-opt "smart-mode-line"       nil)

  (add-opt "malabar"               t)

  ;; CONTROVERSIAL DEFAULTS:

  (add-opt "meta-switch-windows"   nil)
  (add-opt "super-resize-windows"  nil)

  (add-opt "custom-fonts"          nil)
  (add-opt "solarized-dark"        t)
  (add-opt "solarized-light"       t)
  (add-opt "zenburn"               nil)
  (add-opt "thin-cursor"           nil)

  (add-opt "free-up-keys"          nil)
  (add-opt "nano-yank-kill"        nil)
  (add-opt "cua-mode"              nil)

  (add-opt "indent-spaces"         nil)
  (add-opt "sane-c-tab-width"      nil)

  (add-opt "term-mode-switch"      nil)
  (add-opt "undo-tree"             t)

  (message "Available options: %S" my-options))


;; --------------------------------------------------------------------------------
;; ---------------------------------- cedet fix -----------------------------------
;; --------------------------------------------------------------------------------


;;(require 'cl)

;;(when (file-accessible-directory-p "~/.emacs.d/el-get/cedet-devel")
;;  (setq load-path (cl-remove-if (lambda (x) (string-match-p "cedet" x)) load-path))
;;  (load-file "~/.emacs.d/el-get/cedet-devel/cedet-devel-load.el"))


;; --------------------------------------------------------------------------------
;; ----------------------------------- packages -----------------------------------
;; --------------------------------------------------------------------------------


(defun init-generate-packages ()
  "Generate the list of packages to install."
  (add-to-packages '(cedet-devel el-get let-alist tramp diminish delight))

  (when (optionp "perspective")               (add-package 'perspective))
  (when (optionp "projectile")                (add-package 'projectile))

  (when (optionp "iedit")                     (add-package 'iedit))
  (when (optionp "fill-column-indicator")     (add-package 'fill-column-indicator))
  (when (optionp "multi-term")                (add-package 'multi-term))
  (when (optionp "rainbow-delimiters")        (add-package 'rainbow-delimiters))
  (when (optionp "speedbar")                  (add-package 'sr-speedbar))
  (when (optionp "ace-jump-mode")             (add-package 'ace-jump-mode))

  (when (optionp "org")                       (add-to-packages '(org-mode org-trello)))
  (when (optionp "yaml")                      (add-package 'yaml-mode))
  (when (optionp "markdown")                  (add-package 'markdown-mode))

  (when (optionp "rudel")                     (add-package 'rudel))
  (when (optionp "smartparens")               (add-package 'smartparens))
  (when (optionp "flycheck")                  (add-package 'flycheck))
  (when (optionp "undo-tree")                 (add-package 'undo-tree))

  (when (or (optionp "solarized-dark")
            (optionp "solarized-light"))      (add-package 'color-theme-solarized))
  (when (optionp "zenburn")                   (add-package 'color-theme-zenburn))
  (when (optionp "powerline")                 (add-package 'powerline))
  (when (optionp "smart-mode-line")           (add-package 'smart-mode-line))

  (when (optionp "smex")                      (add-package 'smex))
  (when (optionp "company")                   (add-package 'company-mode))
  (when (optionp "yasnippet")                 (add-package 'yasnippet))
  (when (optionp "helm")                      (add-package 'helm))
  (when (optionp "flx")                       (add-to-packages '(flx flx-ido)))

  (when (capabilityp "lang-haskell")          (add-to-packages '(haskell-mode
                                                                 flycheck-haskell
                                                                 company-ghc
                                                                 ghc-mod
                                                                 hi2)))
  (when (capabilityp "lang-git")              (add-package 'magit))
  (when (capabilityp "lang-ledger")           (add-package 'ledger-mode))
  (when (capabilityp "lang-latex")            (add-package 'auctex))
  (when (capabilityp "lang-ocaml")            (add-package 'tuareg-mode))
  (when (capabilityp "lang-nix")              (add-package 'nix-mode))
  (when (capabilityp "lang-purescript")       (add-package 'purescript-mode))
  (when (capabilityp "lang-elm")              (add-package 'elm-mode))
  (when (capabilityp "lang-kframework")       (add-package 'k3-mode))
  (when (capabilityp "lang-zsh")              (add-package 'zlc))
  (when (capabilityp "util-pmd")              (add-package 'flycheck-pmd))
  (when (capabilityp "lang-java")             (add-to-packages '(scala-mode groovy-mode)))
  (when (and (capabilitiesp '("lang-java" "lang-groovy" "build-gradle"))
             (optionp "malabar"))             (add-package 'malabar-mode))

  (delete-dups my-packages)

  (message "Packages to install: %s" my-packages))


;; --------------------------------------------------------------------------------
;; --------------------------------- el-get setup ---------------------------------
;; --------------------------------------------------------------------------------


(defun init-setup-el-get ()
  "Check if el-get is installed, and, if not, install it."
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")


  
  (unless (require 'el-get nil t)
    (unless (capabilityp "internet")
      (error "No internet connection available, cannot install el-get"))
    (with-current-buffer
        (url-retrieve-synchronously
         "https://github.com/dimitri/el-get/raw/master/el-get-install.el")
      (goto-char (point-max))
      (eval-print-last-sexp)))

  (require 'el-get)

  (defvar el-get-recipe-path)
  (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes"))

(defun init-sync-packages ()
  "Make the installed packages consistent with the contents of `my-packages'."
  (el-get 'sync my-packages)
  (el-get-cleanup my-packages))

(defun init-update-packages ()
  "Update all packages."
  (el-get-update-all))


;; --------------------------------------------------------------------------------
;; ----------------------------- require misc modules -----------------------------
;; --------------------------------------------------------------------------------


(defun init-require-modules ()
  "Require necessary modules for init.el."
  (require 'term)

  (when (optionp "flycheck")
    (require 'flycheck))
  (when (optionp "rainbow-delimiters")
    (require 'rainbow-delimiters))
  (when (optionp "smartparens")
    (require 'smartparens-config))
  (when (optionp "powerline")
    (require 'powerline))
  (when (optionp "smart-mode-line")
    (require 'smart-mode-line))
  (when (optionp "smex")
    (require 'smex))
  (when (optionp "company")
    (require 'company))
  (when (optionp "ace-jump-mode")
    (require 'ace-jump-mode))
  (when (optionp "helm")
    (require 'helm))
  (when (optionp "speedbar")
    (require 'sr-speedbar))
  (when (optionp "perspective")
    (require 'perspective))
  (when (and (optionp "projectile")
             (optionp "perspective"))
    (require 'persp-projectile))
  (when (optionp "undo-tree")
    (require 'undo-tree))

  (when (capabilityp "exec-haskell")
    (require 'haskell-mode)
    (require 'haskell-interactive-mode)
    (require 'haskell-simple-indent)
    (require 'hi2))

  (when (capabilityp "exec-lilypond") (require 'lilypond-mode)))


;; --------------------------------------------------------------------------------
;; ----------------------------------- themeing -----------------------------------
;; --------------------------------------------------------------------------------

(defun init-theme-options ()
  "Initialize graphical/theme-related options."

  ;; Disable various annoyances that come with Emacs
  (when (and (capabilityp "graphics") (optionp "fix-emacs-cruft"))
    (setq inhibit-splash-screen t)
    (column-number-mode 1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (menu-bar-mode -1))

  ;; Set default frame font
  (when (and (capabilityp "graphics") (optionp "custom-fonts"))
    (defvar default-frame-font
      (cond ((capabilityp "font-inconsolata")   "Inconsolata")
            ((capabilityp "font-menlo")         "Menlo")
            ((capabilityp "font-meslo")         "Meslo")
            ((capabilityp "font-dejavu")        "DejaVu Sans Mono")
            ((capabilityp "font-freefont")      "FreeMono")
            ((capabilityp "font-liberation")    "Liberation Mono")
            ((capabilityp "font-sourcecodepro") "Source Code Pro")
            ((capabilityp "font-luxi")          "Luxi Mono")
            ((capabilityp "font-consolas")      "Consolas")))
    (defvar default-frame-font-size 10)
    (setq default-frame-alist
          (list (cons 'font (format "%s-%d"
                                    default-frame-font
                                    default-frame-font-size)))))

  ;; Line numbers
  (when (and (capabilityp "graphics") (optionp "line-numbers"))
    (line-number-mode 1)
    (global-hl-line-mode)
    (global-linum-mode 1)
    (setq-default linum-format "%4d \u2502"))

  ;; Disable line numbers for various modes
  (when (and (capabilityp "graphics") (optionp "line-numbers"))
    (defvar linum-disable (lambda () (linum-mode -1)))
    (add-hook 'term-mode-hook                  linum-disable)
    (add-hook 'package-menu-mode-hook          linum-disable)
    (when (optionp "multi-term")
      (add-hook 'multi-term-mode-hook          linum-disable))
    (when (capabilityp "exec-haskell")
      (add-hook 'haskell-interactive-mode-hook linum-disable))
    (when (optionp "speedbar")
      (add-hook 'sr-speedbar-mode-hook         linum-disable)
      (add-hook 'speedbar-mode-hook            linum-disable)))

  ;; Disable horizontal autoscroll in sr-speedbar
  (when (optionp "speedbar")
    (defvar disable-auto-hscroll (lambda () (setq auto-hscroll-mode nil)))
    (add-hook 'sr-speedbar-mode-hook         disable-auto-hscroll)
    (add-hook 'speedbar-mode-hook            disable-auto-hscroll))

  ;; Enable zenburn theme
  (when (optionp "zenburn")
    (load-theme 'zenburn t))

  ;; Enable solarized-light theme
  (when (optionp "solarized-light")
    (load-theme 'zenburn t))

  ;; Enable solarized-dark theme
  (when (optionp "solarized-dark")
    (load-theme 'zenburn t))

  ;; Thin cursor
  (when (and (capabilityp "graphics") (optionp "thin-cursor"))
    (setq-default cursor-type 'bar))

  ;; Enable Powerline modeline
  (when (optionp "powerline")
    (powerline-default-theme))

  ;; Smart mode line
  (when (optionp "smart-mode-line")
    (setq-default sml/no-confirm-load-theme t)
    (sml/setup)))


;; --------------------------------------------------------------------------------
;; ------------------------------------ editing -----------------------------------
;; --------------------------------------------------------------------------------


(defun init-editing-options ()
  "Initialize editing options."

  ;; Move by subword in CamelCase
  (when (optionp "autorevert")
    (global-subword-mode))

  ;; Auto-revert buffers every so often
  (when (optionp "autorevert")
    (global-auto-revert-mode)
    (defvar auto-revert-check-vc-info t))

  ;; Smarter editing with matching delimiters
  (when (optionp "smartparens")
    (smartparens-global-mode))

  ;; On-the-fly syntax checking
  (when (optionp "flycheck")
    (global-flycheck-mode)
    (setq-default flycheck-emacs-lisp-load-path 'inherit))

  ;; Add multiple "perspectives" for buffers (i.e.: workspaces)
  (when (optionp "perspective")
    (persp-mode)
    (persp-turn-on-modestring))

  ;; Indent automagically
  (when (optionp "electric-indent")
    (electric-indent-mode +1))

  ;; Better indenting for Haskell
  (when (optionp "haskell-simple-indent")
    (turn-on-haskell-simple-indent))

  ;; Better autocompletion
  (when (optionp "company")
    (global-company-mode))

  ;; Better minibuffer autocompletion
  (when (optionp "smex")
    (smex-initialize))

  ;; In-buffer project browser
  (when (optionp "speedbar")
    (sr-speedbar-open))

  ;; Allow X11 copy-and-paste into buffers
  (when (capabilityp "graphics-x11")
    (setq x-select-enable-clipboard t))

  ;; Disable indenting with tabs by default
  (when (optionp "indent-spaces")
    (setq-default indent-tabs-mode nil))

  ;; Enable undo-tree
  (when (optionp "undo-tree")
    (global-undo-tree-mode))

  ;; Scroll compilation output
  (setq-default compilation-scroll-output t)

  ;; Set C tab width to 4
  (when (optionp "sane-c-tab-width")
    (defvar c-default-style "linux")
    (setq-default c-basic-offset 4
                  tab-width 4))

  ;; Enable malabar-mode
  (when (and (capabilitiesp '("exec-jdk" "exec-groovy" "exec-gradle"))
             (optionp "malabar"))
    (add-hook 'after-init-hook 'activate-malabar-mode)))


;; --------------------------------------------------------------------------------
;; ---------------------------------- keybindings ---------------------------------
;; --------------------------------------------------------------------------------


(defun init-keyboard-options ()
  "Initialize keyboard options."

  ;; Fix C-z weirdness
  (when (optionp "fix-emacs-cruft")
    (global-unset-key (kbd "C-z")))

  ;; Fix Ctrl-PgUp and Ctrl-PgDown weirdness
  (when (optionp "fix-emacs-cruft")
    (global-unset-key (kbd "C-<next>"))
    (global-set-key (kbd "C-<next>") 'scroll-down-command)
    (global-set-key (kbd "C-<prior>") 'scroll-up-command))

  ;; Fix C-x C-k and C-x f not being the same as C-x k and C-x C-f respectively
  (when (optionp "fix-emacs-cruft")
    (global-set-key (kbd "C-x C-k") 'kill-buffer)
    (global-set-key (kbd "C-x f") 'find-file))

  ;; Nano-style line killing/yanking
  (when (optionp "nano-yank-kill")
    (global-set-key (kbd "C-k") 'kill-whole-line)
    (global-set-key (kbd "C-u") 'yank))

  ;; Useful shortcuts for compile
  (when (optionp "fix-emacs-cruft")
    (global-set-key [f5] 'compile)
    (global-set-key [f6] 'recompile))

  ;; Resize windows with super + arrow keys
  (when (optionp "super-resize-windows")
    (global-set-key (kbd "s-<left>")  'shrink-window-horizontally)
    (global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
    (global-set-key (kbd "s-<down>")  'shrink-window)
    (global-set-key (kbd "s-<up>")    'enlarge-window))

  ;; Switch windows with meta + arrow keys
  (when (optionp "meta-switch-windows")
    (windmove-default-keybindings 'meta)
    (defvar windmove-wrap-around t))

  ;; Free up some keys for keybindings
  (when (optionp "free-up-keys")
    (global-unset-key (kbd "C-a")))

  ;; GNU screen-style keybindings for perspective
  (when (optionp "perspective")
    (global-set-key (kbd "C-a s") 'persp-switch)
    (global-set-key (kbd "C-a b") 'persp-add-buffer)
    (global-set-key (kbd "C-a a") 'persp-rename)
    (global-set-key (kbd "C-a k") 'persp-kill)
    (global-set-key (kbd "C-a C-s") 'persp-switch)
    (global-set-key (kbd "C-a C-b") 'persp-add-buffer)
    (global-set-key (kbd "C-a C-a") 'persp-rename)
    (global-set-key (kbd "C-a C-k") 'persp-kill))

  ;; Enable smex on M-x, M-X, and <menu>
  (when (optionp "smex")
    (global-set-key (kbd "M-x") 'smex)
    (global-set-key (kbd "M-X") 'smex-major-mode-commands)
    (global-set-key (kbd "<menu>") 'smex))

  ;; Enable CUA keybindings
  (when (optionp "cua-mode")
    (cua-mode))

  ;; Switch between line and char mode in term with C-'
  (when (optionp "term-mode-switch")
    (define-key term-raw-map  (kbd "C-'") 'term-line-mode)
    (define-key term-mode-map (kbd "C-'") 'term-char-mode))

  ;; Misc keybindings
  (when (optionp "fix-emacs-cruft")
    (global-set-key (kbd "C-x a r") 'align-regexp)
    (global-set-key (kbd "M-[") 'align))
  (when (optionp "ace-jump-mode")
    (define-key global-map (kbd "C-c SPC") 'ace-jump-mode))
  (when (capabilityp "exec-haskell")
    (define-key
      haskell-interactive-mode-map
      (kbd "C-c C-t")
      'haskell-mode-show-type-at)))


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
  "Run the function `smartparens-mode' in the minibuffer, during `eval-expression'."
  '(lambda () (when (eq this-command 'eval-expression) (smartparens-mode))))


;; --------------------------------------------------------------------------------
;; ------------------------------------- hooks ------------------------------------
;; --------------------------------------------------------------------------------


(defun init-hooks ()
  "Initialize hook options."
  (add-hook 'java-mode-hook                (hook-select-flycheck-checker 'java-pmd))
  (add-hook 'java-mode-hook                (create-dtw-hook))
  (add-hook 'haskell-mode-hook             (create-dtw-hook))
  (add-hook 'lisp-mode-hook                (create-dtw-hook))
  (add-hook 'minibuffer-setup-hook         (minibuffer-smartparens-mode))
  (add-hook 'prog-mode-hook                'rainbow-delimiters-mode)
  (add-hook 'haskell-mode-hook             'turn-on-hi2)
  (add-hook 'haskell-mode-hook             'interactive-haskell-mode)
  (add-hook 'flycheck-mode-hook            'flycheck-haskell-setup))


;; --------------------------------------------------------------------------------
;; --------------------------------- miscellaneous --------------------------------
;; --------------------------------------------------------------------------------


(defun init-fix-miscellany ()
  "Fix miscellaneous problems in Emacs."
  (when (optionp "fix-emacs-cruft")
    ;; Autosave into ~/.emacs.d/backups
    (setq backup-directory-alist
          `(("." . ,(expand-file-name "~/.emacs.d/backups"))))

    ;; Save all tempfiles in $TMPDIR/emacs$UID/
    (defconst emacs-tmp-dir (format "%s/%s%s/"
                                    temporary-file-directory
                                    "emacs"
                                    (user-uid)))
    (setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
    (setq auto-save-list-file-prefix emacs-tmp-dir)
    
    ;; Enable the upcase-region command
    (put 'upcase-region 'disabled nil)))


;; --------------------------------------------------------------------------------
;; -------------------------------- run everything --------------------------------
;; --------------------------------------------------------------------------------


(defun init-run-functions (functions)
  "Run some FUNCTIONS with profiling/debug/errors depending on the settings."
  (unless (listp functions)
    (error "Wrong argument: init-run-functions takes a list of functions"))
  (let ((run-funcs (lambda ()
                     (if init-profiling-enabled
                         (profile-functions functions)
                       (progn (dolist (element functions)
                                (apply element nil)))))))
    (if init-errors-enabled
        (apply run-funcs nil)
      (ignore-errors (apply run-funcs nil)))))

(init-run-functions '(init-check-prerequisites
                      init-detect-capabilities
                      init-customize-options
                      init-generate-packages
                      init-setup-el-get
                      init-sync-packages
                      init-require-modules
                      init-theme-options
                      init-editing-options
                      init-keyboard-options
                      init-hooks
                      init-fix-miscellany))


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
