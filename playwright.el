;;; playwright.el --- Emacs interface for Playwright

;; Author: Martin Brignall <martinaloysiusbrignall@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience
;; URL: http://github.com/mbrignall/playwright.el

;;; Commentary:

;; This package provides an Emacs interface for the Playwright testing framework.
;; It allows you to run Playwright tests and view the results directly from Emacs.

;;; Code:

(require 'transient)

;; Define transient arguments (flags)
(transient-define-argument playwright:--debug ()
  :description "Toggle debug mode"
  :key "d"
  :argument "--debug")

(transient-define-argument playwright:--headed ()
  :description "Run tests in headed mode"
  :key "h"
  :argument "--headed")

(transient-define-argument playwright:--ui ()
  :description "Run tests in UI mode"
  :key "u"
  :argument "--ui")

(transient-define-infix playwright:--project ()
  :description "Set project"
  :key "p"
  :class 'transient-option
  :argument "--project="
  :reader (lambda (prompt _initial-input _history)
            (read-string prompt)))

(transient-define-infix playwright:--workers ()
  :description "Set number of workers"
  :key "w"
  :class 'transient-option
  :argument "--workers="
  :reader (lambda (prompt _initial-input _history)
            (read-string prompt)))

(transient-define-infix playwright:--reporter ()
  :description "Choose a reporter"
  :key "r"
  :class 'transient-option
  :argument "--reporter="
  :reader (lambda (prompt _initial-input _history)
            (completing-read prompt '("dot" "list" "line" "json" "null"))))

(transient-define-argument playwright:-g ()
  :description "Run tests with a title"
  :key "g"
  :class 'transient-option
  :argument "-g "
  :reader (lambda (prompt _initial-input _history)
	    (read-string prompt)))

(transient-define-infix playwright:--extras ()
  :description "Additional command options"
  :key "e"
:class 'transient-option
:argument ""
:reader (lambda (prompt _initial-input _history)
	  (read-string (concat prompt ": "))))

(defun playwright-run-multiple-test-files (args)
  "Run a set of Playwright test files specified by ARGS."
  (interactive (list (transient-args 'playwright)))
  (let* ((files (read-string "Enter space-separated test files: "))
         (command (concat "npx playwright test " files " " (string-join args " "))))
    (compile command)))

;; Define the transient prefix
(transient-define-prefix playwright ()
  "Transient for running Playwright commands."
  ["Arguments"
   (playwright:--debug)
   (playwright:--extras)
   (playwright:--headed)
   (playwright:-g)
   (playwright:--project)
   (playwright:--workers)
   (playwright:--reporter)
   (playwright:--ui)
   ]
  ["Actions"
   ("a" "Run all tests" playwright-run-all-tests)
   ("s" "Run single test file" playwright-run-single-test-file)
   ("m" "Run multiple test files" playwright-run-multiple-test-files)
   ])

;; Define functions to run tests
(defun playwright-run-all-tests (args)
  "Run all Playwright tests with specified ARGS."
  (interactive (list (transient-args 'playwright)))
  (let ((command (concat "npx playwright test " (string-join args " "))))
    (compile command)))

(defun playwright-run-single-test-file (args)
  "Run a single Playwright test file with specified ARGS."
  (interactive (list (transient-args 'playwright)))
  (let* ((file (expand-file-name (read-file-name "Select a test file: ")))
         (command (concat "npx playwright test " file " " (string-join args " "))))
    (compile command)))

;; Bind the transient to a key
(global-set-key (kbd "C-c t p") 'playwright)

(provide 'playwright)
;;; playwright.el ends here
