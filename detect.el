;;; detect.el --- Autodetect capabilities of the current system

;; Copyright (C) 2015 Remy Goldschmidt
;; Author: Remy Goldschmidt <taktoa@gmail.com>
;; Maintainer: Remy Goldschmidt <taktoa@gmail.com>
;; Created: 02 Aug 2015
;; Keywords: maint
;; Homepage: http://github.com/taktoa/emacs-config

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation, either version 3 of the License, or
;;     (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public License
;;     along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This library provides functions for autodetection of system capabilities.

;;; Code:

;;(require 'detect-lib)

(defgroup detect nil
  "Customization for capability autodetection"
  :group 'emacs)

(defcustom detect-internet-test-url "http://gnu.org"
  "URL used to test whether or not a network connection is available."
  :type 'string
  :group 'detect)

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

;; (dl/defdetector
;;  detect/detect-system-capabilities
;;  "Detect the current system."
;;  (when (url-resolves-p detect-internet-test-url) (dl/add-cap 'internet))

;;  (cond ((eq system-type 'gnu/linux)        (dl/add-cap 'system-linux))
;;        ((eq system-type 'darwin)           (dl/add-cap 'system-osx))
;;        ((eq system-type 'windows-nt)       (dl/add-cap 'system-windows))
;;        ((eq system-type 'ms-dos)           (dl/add-cap 'system-msdos))
;;        ((eq system-type 'cygwin)           (dl/add-cap 'system-cygwin))
;;        ((eq system-type 'gnu)              (dl/add-cap 'system-unix))
;;        ((eq system-type 'gnu/kfreebsd)     (dl/add-cap 'system-unix))
;;        ((eq system-type 'aix)              (dl/add-cap 'system-unix))
;;        ((eq system-type 'irix)             (dl/add-cap 'system-unix))
;;        ((eq system-type 'hpux)             (dl/add-cap 'system-unix))
;;        ((eq system-type 'usg-unix-v)       (dl/add-cap 'system-unix))
;;        ((eq system-type 'berkeley-unix)    (dl/add-cap 'system-unix))
;;        (t                                  (dl/add-cap 'system-unknown)))
;;  (when (or (dl/capp 'system-unix)
;;            (dl/capp 'system-linux)
;;            (dl/capp 'system-cygwin)
;;            (dl/capp 'system-osx))          (dl/add-cap 'system-posix)))

;; (dl/defdetector
;;  detect/detect-graphical-capabilities
;;  "Detect graphical and font capabilities."
;;  (when (display-graphic-p)                 (dl/add-cap 'graphics))

;;  (when (dl/capp 'graphics)
;;    (when (dl/capp 'system-linux)           (dl/add-cap 'graphics-x11))
;;    (when (dl/capp 'system-unix)            (dl/add-cap 'graphics-x11))
;;    (when (dl/capp 'system-windows)         (dl/add-cap 'graphics-w32))
;;    (when (dl/capp 'system-osx)             (dl/add-cap 'graphics-osx)))

;;  (when (dl/capp 'graphics)
;;    (when (x-list-fonts "Inconsolata")      (dl/add-cap 'font-inconsolata))
;;    (when (x-list-fonts "Menlo")            (dl/add-cap 'font-menlo))
;;    (when (x-list-fonts "Meslo")            (dl/add-cap 'font-meslo))
;;    (when (x-list-fonts "DejaVu Sans Mono") (dl/add-cap 'font-dejavu))
;;    (when (x-list-fonts "FreeMono")         (dl/add-cap 'font-freefont))
;;    (when (x-list-fonts "Liberation Mono")  (dl/add-cap 'font-liberation))
;;    (when (x-list-fonts "Source Code Pro")  (dl/add-cap 'font-sourcecodepro))
;;    (when (x-list-fonts "Luxi Mono")        (dl/add-cap 'font-luxi))
;;    (when (x-list-fonts "Consolas")         (dl/add-cap 'font-consolas))))

;; (dl/defdetector
;;  detect/detect-emacs-capabilities
;;  "Detect the version of Emacs running."
;;  (cond ((lucid-emacs-p)                    (dl/add-cap 'emacs-lucid))
;;        (t                                  (dl/add-cap 'emacs-gnu))))

;; (dl/defdetector
;;  detect/detect-vcs-capabilities
;;  "Detect available VCS systems."
;;  (when (executable-exists-p "git")         (dl/add-cap 'vcs-git))
;;  (when (executable-exists-p "cvs")         (dl/add-cap 'vcs-cvs))
;;  (when (executable-exists-p "svn")         (dl/add-cap 'vcs-svn))
;;  (when (executable-exists-p "hg")          (dl/add-cap 'vcs-hg))
;;  (when (executable-exists-p "darcs")       (dl/add-cap 'vcs-darcs))
;;  (when (executable-exists-p "bzr")         (dl/add-cap 'vcs-bzr)))

;; (dl/defdetector
;;  detect/detect-build-capabilities
;;  "Detect available build tools."
;;  (when (dl/capp 'system-windows)           (dl/add-cap 'build-sln))
;;  (when (executable-exists-p "make")        (dl/add-cap 'build-make))
;;  (when (executable-exists-p "cmake")       (dl/add-cap 'build-make))
;;  (when (executable-exists-p "latexmk")     (dl/add-cap 'build-latexmk))
;;  (when (executable-exists-p "ant")         (dl/add-cap 'build-ant))
;;  (when (executable-exists-p "mvn")         (dl/add-cap 'build-maven))
;;  (when (executable-exists-p "gradle")      (dl/add-cap 'build-gradle))
;;  (when (executable-exists-p "sbt")         (dl/add-cap 'build-sbt))
;;  (when (executable-exists-p "cask")        (dl/add-cap 'build-cask))
;;  (when (executable-exists-p "lein")        (dl/add-cap 'build-leiningen)))

;; (dl/defdetector
;;  detect/detect-package-manager-capabilities
;;  "Detect available package management systems."
;;  (when (executable-exists-p "apt-get")     (dl/add-cap 'package-apt))
;;  (when (executable-exists-p "emerge")      (dl/add-cap 'package-emerge))
;;  (when (executable-exists-p "pacman")      (dl/add-cap 'package-pacman))
;;  (when (executable-exists-p "yum")         (dl/add-cap 'package-yum))
;;  (when (executable-exists-p "ipkg")        (dl/add-cap 'package-ipkg))
;;  (when (executable-exists-p "opkg")        (dl/add-cap 'package-opkg))
;;  (when (executable-exists-p "pkcon")       (dl/add-cap 'package-pkcon))
;;  (when (executable-exists-p "nix-env")     (dl/add-cap 'package-nix))
;;  (when (executable-exists-p "brew")        (dl/add-cap 'package-brew))
;;  (when (executable-exists-p "wpkg")        (dl/add-cap 'package-wpkg))
;;  (when (executable-exists-p "0install")    (dl/add-cap 'package-0install))

;;  (when (executable-exists-p "cpan")        (dl/add-cap 'package-cpan))
;;  (when (executable-exists-p "pip")         (dl/add-cap 'package-pip))
;;  (when (executable-exists-p "gem")         (dl/add-cap 'package-gem))
;;  (when (executable-exists-p "bower")       (dl/add-cap 'package-bower))
;;  (when (executable-exists-p "npm")         (dl/add-cap 'package-npm))
;;  (when (executable-exists-p "cabal")       (dl/add-cap 'package-cabal))
;;  (when (executable-exists-p "stack")       (dl/add-cap 'package-stack))
;;  (when (executable-exists-p "opam")        (dl/add-cap 'package-opam))
;;  (when (executable-exists-p "rebar")       (dl/add-cap 'package-rebar))
;;  (when (executable-exists-p "cargo")       (dl/add-cap 'package-cargo))
;;  (when (executable-exists-p "mingw-get")   (dl/add-cap 'package-mingw))
;;  (when (executable-exists-p "composer")    (dl/add-cap 'package-composer))
;;  (when (executable-exists-p "dub")         (dl/add-cap 'package-dub)))

;; (dl/defdetector
;;  detect/detect-dsl-lang-capabilities
;;  "Detect available domain specific language compilers/interpreters."
;;  (when (dl/capp 'system-osx)               (dl/add-cap 'lang-applescript))
;;  (when (dl/capp 'system-windows)           (dl/add-cap 'lang-powershell))
;;  (when (dl/capp 'system-windows)           (dl/add-cap 'lang-msdos))
;;  (when (executable-exists-p "httpd")       (dl/add-cap 'lang-apache))
;;  (when (executable-exists-p "nagios")      (dl/add-cap 'lang-nagios))
;;  (when (executable-exists-p "nginx")       (dl/add-cap 'lang-nginx))
;;  (when (executable-exists-p "puppet")      (dl/add-cap 'lang-puppet))
;;  (when (executable-exists-p "syslogd")     (dl/add-cap 'lang-syslog))
;;  (when (executable-exists-p "syslog-ng")   (dl/add-cap 'lang-syslog))
;;  (when (executable-exists-p "systemctl")   (dl/add-cap 'lang-syslog))
;;  (when (executable-exists-p "systemctl")   (dl/add-cap 'lang-systemd))
;;  (when (executable-exists-p "varnishd")    (dl/add-cap 'lang-varnish))
;;  (when (executable-exists-p "cron")        (dl/add-cap 'lang-crontab))
;;  (when (executable-exists-p "anacron")     (dl/add-cap 'lang-crontab))

;;  (when (executable-exists-p "arduino")     (dl/add-cap 'lang-arduino))
;;  (when (executable-exists-p "iverilog")    (dl/add-cap 'lang-verilog))

;;  (when (executable-exists-p "blender")     (dl/add-cap 'lang-blender-python))
;;  (when (executable-exists-p "povray")      (dl/add-cap 'lang-povray))

;;  (when (executable-exists-p "bison")       (dl/add-cap 'lang-bison))
;;  (when (executable-exists-p "lex")         (dl/add-cap 'lang-bison))
;;  (when (executable-exists-p "flex")        (dl/add-cap 'lang-bison))
;;  (when (executable-exists-p "yacc")        (dl/add-cap 'lang-bison))
;;  (when (executable-exists-p "fortune")     (dl/add-cap 'lang-fortune))
;;  (when (executable-exists-p "gengetopt")   (dl/add-cap 'lang-gengetopt))
;;  (when (executable-exists-p "gettext")     (dl/add-cap 'lang-po))
;;  (when (executable-exists-p "mutt")        (dl/add-cap 'lang-muttrc))

;;  (when (executable-exists-p "docker")      (dl/add-cap 'lang-docker))
;;  (when (executable-exists-p "gyp")         (dl/add-cap 'lang-gyp))
;;  (when (executable-exists-p "gnuplot")     (dl/add-cap 'lang-gnuplot))
;;  (when (executable-exists-p "dot")         (dl/add-cap 'lang-graphviz))

;;  (when (executable-exists-p "bc")          (dl/add-cap 'lang-bc))
;;  (when (executable-exists-p "matlab")      (dl/add-cap 'lang-matlab)) ;; not sure
;;  (when (executable-exists-p "sage")        (dl/add-cap 'lang-sage))
;;  (when (executable-exists-p "R")           (dl/add-cap 'lang-r))

;;  (when (executable-exists-p "qmake")       (dl/add-cap 'lang-qmake))
;;  (when (executable-exists-p "qtdiag")      (dl/add-cap 'lang-qml))

;;  (when (executable-exists-p "latex")       (dl/add-cap 'lang-latex))
;;  (when (executable-exists-p "lilypond")    (dl/add-cap 'lang-lilypond))
;;  (when (executable-exists-p "ledger")      (dl/add-cap 'lang-ledger))
;;  (when (executable-exists-p "hledger")     (dl/add-cap 'lang-ledger)))

;; (dl/defdetector
;;  detect/detect-imperative-lang-capabilities
;;  "Detect available imperative compilers/interpreters."
;;  (when (executable-exists-p "gcc")         (dl/add-cap 'lang-c))
;;  (when (executable-exists-p "clang")       (dl/add-cap 'lang-c))
;;  (when (executable-exists-p "g++")         (dl/add-cap 'lang-c++))
;;  (when (executable-exists-p "clang++")     (dl/add-cap 'lang-c++))
;;  (when (executable-exists-p "gnat")        (dl/add-cap 'lang-ada))
;;  (when (executable-exists-p "gfortran")    (dl/add-cap 'lang-fortran))
;;  (when (executable-exists-p "gccgo")       (dl/add-cap 'lang-go))
;;  (when (executable-exists-p "llc")         (dl/add-cap 'lang-llvm))
;;  (when (executable-exists-p "rustc")       (dl/add-cap 'lang-rust))
;;  (when (executable-exists-p "nim")         (dl/add-cap 'lang-nim))
;;  (when (executable-exists-p "vala")        (dl/add-cap 'lang-vala))
;;  (when (executable-exists-p "php")         (dl/add-cap 'lang-php))
;;  (when (executable-exists-p "mono")        (dl/add-cap 'lang-csharp))
;;  (when (dl/capp 'system-windows)           (dl/add-cap 'lang-csharp)))

;; (dl/defdetector
;;  detect/detect-jvm-lang-capabilities
;;  "Detect available JVM compilers/interpreters."
;;  (when (executable-exists-p "java")        (dl/add-cap 'lang-jvm))
;;  (when (executable-exists-p "javac")       (dl/add-cap 'lang-java))
;;  (when (executable-exists-p "gcj")         (dl/add-cap 'lang-java))
;;  (when (executable-exists-p "groovy")      (dl/add-cap 'lang-groovy))
;;  (when (executable-exists-p "lein")        (dl/add-cap 'lang-clojure))
;;  (when (executable-exists-p "sbt")         (dl/add-cap 'lang-scala))
;;  (when (executable-exists-p "scalac")      (dl/add-cap 'lang-scala)))

;; (dl/defdetector
;;  detect/detect-scripting-lang-capabilities
;;  "Detect available scripting language compilers/interpreters."
;;  (when (executable-exists-p "python")      (dl/add-cap 'lang-python))
;;  (when (executable-exists-p "ruby")        (dl/add-cap 'lang-ruby))
;;  (when (executable-exists-p "perl")        (dl/add-cap 'lang-perl))
;;  (when (executable-exists-p "julia")       (dl/add-cap 'lang-julia))
;;  (when (executable-exists-p "jc")          (dl/add-cap 'lang-j))
;;  (when (executable-exists-p "io")          (dl/add-cap 'lang-io))
;;  (when (executable-exists-p "bash")        (dl/add-cap 'lang-bash))
;;  (when (executable-exists-p "zsh")         (dl/add-cap 'lang-zsh)))

;; (dl/defdetector
;;  detect/detect-web-lang-capabilities
;;  "Detect available web language compilers/interpreters."
;;  (when (executable-exists-p "lessc")       (dl/add-cap 'lang-less))
;;  (when (executable-exists-p "sassc")       (dl/add-cap 'lang-sass))
;;  (when (executable-exists-p "opa")         (dl/add-cap 'lang-opa))
;;  (when (executable-exists-p "node")        (dl/add-cap 'lang-node))
;;  (when (executable-exists-p "psc")         (dl/add-cap 'lang-purescript))
;;  (when (executable-exists-p "lsc")         (dl/add-cap 'lang-livescript))
;;  (when (executable-exists-p "coffee")      (dl/add-cap 'lang-coffeescript)))

;; (dl/defdetector
;;  detect/detect-functional-lang-capabilities
;;  "Detect available functional language compilers/interpreters."
;;  (when (executable-exists-p "ocamlc")      (dl/add-cap 'lang-ocaml))
;;  (when (executable-exists-p "erlc")        (dl/add-cap 'lang-erlang))
;;  (when (executable-exists-p "ghc")         (dl/add-cap 'lang-haskell))
;;  (when (executable-exists-p "idris")       (dl/add-cap 'lang-idris))
;;  (when (executable-exists-p "elm")         (dl/add-cap 'lang-elm))
;;  (when (executable-exists-p "nix-build")   (dl/add-cap 'lang-nix))
;;  (when (executable-exists-p "jc")          (dl/add-cap 'lang-j))
;;  (when (executable-exists-p "newlisp")     (dl/add-cap 'lang-newlisp))
;;  (when (executable-exists-p "racket")      (dl/add-cap 'lang-racket))
;;  (when (executable-exists-p "guile")       (dl/add-cap 'lang-guile))
;;  (when (executable-exists-p "chicken")     (dl/add-cap 'lang-chicken))
;;  (when (executable-exists-p "lua")         (dl/add-cap 'lang-lua))
;;  (when (executable-exists-p "shen")        (dl/add-cap 'lang-shen))
;;  (when (executable-exists-p "sml")         (dl/add-cap 'lang-sml))
;;  (when (executable-exists-p "polyc")       (dl/add-cap 'lang-sml))
;;  (when (executable-exists-p "mlton")       (dl/add-cap 'lang-sml))
;;  (when (executable-exists-p "swift")       (dl/add-cap 'lang-swift))
;;  (when (executable-exists-p "fsharpc")     (dl/add-cap 'lang-fsharp))
;;  (when (executable-exists-p "kompile")     (dl/add-cap 'lang-kframework)))

;; (dl/defdetector
;;  detect/detect-misc-capabilities
;;  "Detect miscellaneous capabilities."
;;  (when (executable-exists-p "ocamlfind")   (dl/add-cap 'util-ocamlfind))
;;  (when (executable-exists-p "pandoc")      (dl/add-cap 'util-pandoc))
;;  (when (executable-exists-p "pmd")         (dl/add-cap 'util-pmd))
;;  (when (executable-exists-p "top")         (dl/add-cap 'util-top)))

;; (defmacro capabilityp (cap)
;;   "Is the given CAP an available capability?"
;;   (if (member cap (dl/get-available-capabilities))
;;       `(if (member ',cap (dl/get-tested-capabilities))
;;            (dl/capp ',cap)
;;          (mapcar (lambda (x) (apply x nil))
;;                  ',(gethash cap (dl/get-capability-declarer-hash-table))))
;;     nil))

;; (defmacro capabilitiesp (&rest caps)
;;   "Are the given CAPS available capabilities?"
;;   (let ((result nil))
;;     (dolist (element (reverse caps))
;;       (add-to-list 'result `(capabilityp ,element)))
;;     (cons 'and result)))

(defmacro capabilityp (cap) 't)
(defmacro capabilitiesp (&rest caps) 't)

(provide 'detect)
