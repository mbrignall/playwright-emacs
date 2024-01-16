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

(defun playwright-run-command (command)
  "Run a Playwright COMMAND."
  (let ((output-buffer (get-buffer-create "*Playwright Output*")))
    (with-current-buffer output-buffer
      (erase-buffer)
      (insert (format "Running command: %s...\n\n" command)))
    (shell-command command output-buffer)
    (display-buffer output-buffer)))

(defun playwright-run-all-tests ()
  "Run all Playwright tests."
  (interactive)
  (playwright-run-command "npx playwright test"))

(defun playwright-run-tests-headed ()
  "Run all Playwright tests in headed mode."
  (interactive)
  (playwright-run-command "npx playwright test --headed"))

(defun playwright-run-tests-ui ()
  "Run all Playwright tests in UI mode."
  (interactive)
  (playwright-run-command "npx playwright test --ui"))

(defun playwright-run-tests-debug ()
  "Run all Playwright tests in debug mode."
  (interactive)
  (playwright-run-command "npx playwright test --debug"))

(defun playwright-run-single-test-file (file)
  "Run a single Playwright test file."
  (interactive "FSelect a test file: ")
  (let ((output-buffer (get-buffer-create "*Playwright Test File*")))
    (with-current-buffer output-buffer
      (erase-buffer)
      (insert (format "Running Playwright test file: %s...\n\n" file)))
    (shell-command (format "npx playwright test %s" file) output-buffer)
    (display-buffer output-buffer)))

(defun playwright-popup ()
  "Show a popup buffer with Playwright commands."
  (interactive)
  (let ((command (completing-read "Select Playwright Command: "
                                  '("Run All Tests"
                                    "Run Single Test File"
                                    "Run Tests in Headed Mode"
                                    "Run Tests in UI Mode"
                                    "Run Tests in Debug Mode"))))
    (cond ((equal command "Run All Tests") (call-interactively 'playwright-run-all-tests))
          ((equal command "Run Single Test File") (call-interactively 'playwright-run-single-test-file))
          ((equal command "Run Tests in Headed Mode") (call-interactively 'playwright-run-tests-headed))
          ((equal command "Run Tests in UI Mode") (call-interactively 'playwright-run-tests-ui))
          ((equal command "Run Tests in Debug Mode") (call-interactively 'playwright-run-tests-debug)))))

(global-set-key (kbd "C-c t a") 'playwright-popup)

(provide 'playwrightold)
;;; playwright.el ends here
