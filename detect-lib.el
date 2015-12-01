;;; detect-lib.el --- Library for adding capability autodetection code

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
;; This library provides utilities for the detection of system capabilities as
;; outlined in `detect.el'.

;;; Code:

(require 'cl-lib)

(defun detect-lib/proper-list-p (value)
  "Is the given VALUE a proper list?"
  (and (listp value)
       (listp (cdr value))))

(defun detect-lib/improper-list-p (value)
  "Is the given VALUE not a proper list?"
  (not (detect-lib/proper-list-p value)))

(defun detect-lib/filter (pred list)
  "Remove all the elements not satisifying PRED from LIST."
  (cl-remove-if-not pred list))

(defun detect-lib/remove-quote (list)
  "Remove all elements of LIST that are equal to 'quote."
  (detect-lib/filter (lambda (x) (not (eq x 'quote))) list))

(defun detect-lib/search-sexpr (name func sexpr)
  "For sub-expressions beginning with NAME, apply FUNC, in SEXPR."
  (cond
   ((detect-lib/improper-list-p sexpr) '())

   ((eq (car sexpr) name) (let ((res (apply func (list sexpr))))
                            (if (listp res) res (list res))))

   (t (apply #'append (mapcar (lambda (x) (detect-lib/search-sexpr name func x))
                              (cdr sexpr))))))

(defun detect-lib/normalize-sexpr (sexpr &optional default-sym)
  "Normalize a SEXPR such that it always begins with a symbol.
If DEFAULT-SYM is given, that symbol will be DEFAULT-SYM.
Otherwise, it will default to `normalize'."
  (if (symbolp (car sexpr)) sexpr (cons 'normalize sexpr)))

(defun detect-lib/standard-search (name sexpr)
  "Return a list of all the arguments NAME was ever given in SEXPR."
  (delete-dups
   (detect-lib/remove-quote
    (apply #'append
           (detect-lib/search-sexpr name #'cdr
                                    (detect-lib/normalize-sexpr sexpr))))))

(defun detect-lib/search-add-cap (sexpr)
  "Search for 'dl/add-cap in SEXPR."
  (detect-lib/standard-search 'dl/add-cap sexpr))

(defun detect-lib/search-capabilityp (sexpr)
  "Search for 'dl/capp in SEXPR."
  (detect-lib/standard-search 'dl/capp sexpr))

(defun detect-lib/search-capabilitiesp (sexpr)
  "Search for 'dl/capsp in SEXPR."
  (detect-lib/standard-search 'dl/capsp sexpr))


;; Based on -partition from dash.el
(defun detect-lib/plist->alist (plist)
  "Convert a PLIST to an alist."
  (let ((res nil) (sl nil) (len 0))
    (while plist
      (setq sl (cons (car plist) sl))
      (setq plist (cdr plist))
      (setq len (1+ len))

      (when (= len 2)
        (setq res (cons (nreverse sl) res))
        (setq sl nil)
        (setq len 0)))
    (when sl (error "Error: detect-lib/plist->alist: invalid plist"))
    (nreverse res)))

(defun detect-lib/alist->hash-table (alist)
  "Convert an ALIST to a hash-table."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (pair alist h)
      (let ((key (car pair))
            (value (cadr pair)))
        (puthash key value h)))))

(defun detect-lib/plist->hash-table (plist)
  "Convert a PLIST to a hash-table."
  (detect-lib/alist->hash-table
   (detect-lib/plist->alist plist)))

(defconst detect-lib/updated nil
  "Have the capability symbols been updated?")

(defconst detect-lib/available-capabilities nil
  "A list of all the capability symbols.")

(defconst detect-lib/available-detectors nil
  "A list of all the detectors.")

(defconst detect-lib/capability-declarer-plist nil
  "A plist from capabilities to a list of the detectors that declare them.")

(defconst detect-lib/capability-requirer-plist nil
  "A plist from capabilities to a list of the detectors that require them.")

(defconst detect-lib/declared-capabilities-plist nil
  "A plist from detectors to a list of the capabilities they detect.")

(defconst detect-lib/required-capabilities-plist nil
  "A plist from detectors to a list of the capabilities they require.")

(defconst detect-lib/capability-declarer-hash-table nil
  "An table from capabilities to a list of the functions that detect them.")

(defconst detect-lib/capability-requirer-hash-table nil
  "An htable from capabilities to a list of the functions that require them.")

(defconst detect-lib/declared-capabilities-hash-table nil
  "An htable from detectors to a list of the capabilities they detect.")

(defconst detect-lib/required-capabilities-hash-table nil
  "An htable from detectors to a list of the capabilities they require.")

(defun detect-lib/add-cap-required (detector capability)
  "Declare the fact that DETECTOR requires CAPABILITY."
  (unless detect-lib/updated (setq detect-lib/updated t))
  (add-to-list 'detect-lib/available-capabilities capability)
  (add-to-list 'detect-lib/available-detectors detector)
  (setq detect-lib/capability-declarer-plist
        (plist-put detect-lib/capability-declarer-plist capability detector))
  (let ((get (plist-get detect-lib/declared-capabilities-plist detector)))
    (setq detect-lib/declared-capabilities-plist
          (plist-put detect-lib/declared-capabilities-plist detector
                     (cons capability get)))))

(defun detect-lib/add-cap-declared (detector capability)
  "Declare the fact that DETECTOR declared CAPABILITY."
  (unless detect-lib/updated (setq detect-lib/updated t))
  (add-to-list 'detect-lib/available-capabilities capability)
  (add-to-list 'detect-lib/available-detectors detector)
  (setq detect-lib/capability-declarer-plist
        (plist-put detect-lib/capability-declarer-plist capability detector))
  (let ((get (plist-get detect-lib/declared-capabilities-plist detector)))
    (setq detect-lib/declared-capabilities-plist
          (plist-put detect-lib/declared-capabilities-plist detector
                     (cons capability get)))))

(defun detect-lib/refresh ()
  "Refresh the contents of various variables that depend on other variables."
  (when detect-lib/updated
    (setq detect-lib/available-capabilities
          (delete-dups detect-lib/available-capabilities))
    (setq detect-lib/available-detectors
          (delete-dups detect-lib/available-detectors))
    (setq detect-lib/capability-declarer-hash-table
          (detect-lib/plist->hash-table detect-lib/capability-declarer-plist))
    (setq detect-lib/declared-capabilities-hash-table
          (detect-lib/plist->hash-table detect-lib/declared-capabilities-plist))
    (setq detect-lib/updated nil)))

(defun detect-lib/search-declared-caps (sexpr)
  "Find all the capabilities declared in SEXPR."
  `',(detect-lib/search-add-cap sexpr))

(defun detect-lib/search-required-caps (sexpr)
  "Find all the capabilities required in SEXPR."
  `',(delete-dups
      (append (detect-lib/search-capabilityp sexpr)
              (detect-lib/search-capabilitiesp sexpr))))

(defvar detect-lib/tested-capabilities nil
  "Capabilities tested so far.")

(defvar detect-lib/detected-capabilities nil
  "Capabilities detected so far.")


;; -----------------------------------------------------------------------------
;; ----------------------------- Public functions ------------------------------
;; -----------------------------------------------------------------------------

(defmacro dl/defdetector (name docstring &rest body)
  "Define a detector with NAME, DOCSTRING, and BODY."
  (mapc (lambda (x) (detect-lib/add-cap-declared name x))
        (detect-lib/search-declared-caps body))
  (mapc (lambda (x) (detect-lib/add-cap-required name x))
        (detect-lib/search-required-caps body))
  (detect-lib/refresh)
  `(defun ,name ()
     ,docstring
     ,@body
     (add-to-list 'detect-lib/tested-capabilities
                  ,(gethash name detect-lib/available-capabilities))))

(defun dl/add-cap (cap)
  "Add CAP to the set of existing capabilities."
  (unless (dl/capp cap)
    (push cap detect-lib/detected-capabilities)))

(defun dl/capp (cap)
  "Is the given CAP an available capability?"
  (member cap detect-lib/detected-capabilities))

(defun dl/capsp (caps)
  "Are the given CAPS available capabilities?"
  (catch 'return
    (dolist (element caps)
      (unless (dl/capp element)
        (throw 'return nil)))
    (throw 'return t)))



(defmacro dl/get-available-capabilities ()
  "A list of all the capability symbols."
  `',detect-lib/available-capabilities)

(defmacro dl/get-available-detectors ()
  "A list of all the detectors."
  `',detect-lib/available-detectors)

(defmacro dl/get-capability-declarer-plist ()
  "A plist from capabilities to a list of the detectors that declare them."
  `',detect-lib/capability-declarer-plist)

(defmacro dl/get-capability-requirer-plist ()
  "A plist from capabilities to a list of the detectors that require them."
  `',detect-lib/capability-requirer-plist)

(defmacro dl/get-declared-capabilities-plist ()
  "A plist from detectors to a list of the capabilities they declare."
  `',detect-lib/declared-capabilities-plist)

(defmacro dl/get-required-capabilities-plist ()
  "A plist from detectors to a list of the capabilities they require."
  `',detect-lib/required-capabilities-plist)

(defmacro dl/get-capability-declarer-hash-table ()
  "An htable from capabilities to a list of the detectors that declare them."
  `',detect-lib/capability-declarer-hash-table)

(defmacro dl/get-capability-requirer-hash-table ()
  "An htable from capabilities to a list of the detectors that require them."
  `',detect-lib/capability-requirer-hash-table)

(defmacro dl/get-declared-capabilities-hash-table ()
  "An htable from detectors to a list of the capabilities they declare."
  `',detect-lib/declared-capabilities-hash-table)

(defmacro dl/get-required-capabilities-hash-table ()
  "An htable from detectors to a list of the capabilities they require."
  `',detect-lib/required-capabilities-hash-table)

(defun dl/get-tested-capabilities ()
  "Return a list of all the capabilities tested."
  detect-lib/tested-capabilities)

(provide 'detect-lib)
;;; detect-lib.el ends here
