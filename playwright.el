;;; playwright.el --- Emacs interface for Playwright

;; Author: Your Name <martinaloysiusbrignall@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1")) note: haven't tested on older versions
;; Keywords: tools, convenience
;; URL: http://github.com/mbrignall/playwright.el

;;; Commentary:

;; This package provides an Emacs interface for the Playwright testing framework.
;; It allows you to run Playwright tests and view the results directly from Emacs.

;;; Code:

(defun playwright-run-all-tests ()
  "Run all Playwright tests."
  (interactive)
  (let ((output-buffer (get-buffer-create "*Playwright Tests*")))
    (with-current-buffer output-buffer
      (erase-buffer)
      (insert "Running Playwright tests...\n\n"))
    (shell-command "npx playwright test" output-buffer)
    (display-buffer output-buffer)))

(defun playwright-run-single-test-file (file)
  "Run a single Playwright test file."
  (interactive "FSelect a test file: ")
  (let ((output-buffer (get-buffer-create "*Playwright Test File*")))
    (with-current-buffer output-buffer
      (erase-buffer)
      (insert (format "Running Playwright test file: %s...\n\n" file)))
    (shell-command (format "npx playwright test %s" file) output-buffer)
    (display-buffer output-buffer)))

(defun playwright-run-tests-headed ()
  "Run all Playwright tests in headed browsers."
  (interactive)
  (let ((output-buffer (get-buffer-create "*Playwright Test File*")))
    (with-current-buffer output-buffer
      (erase-buffer)
      (insert "Running Playwright tests in headed browsers...\n\n"))
    (shell-command "npx playwright test --headed" output-buffer)
    (display-buffer output-buffer))
  )

(defun playwright-run-tests-ui ()
  "Run all Playwright tests in interactive UI mode."
  (interactive)
  (let ((output-buffer (get-buffer-create "*Playwright Test File*")))
    (with-current-buffer output-buffer
      (erase-buffer)
      (insert "Running Playwright tests in intereactive UI mode...\n\n"))
    (shell-command "npx playwright test --headed --ui" output-buffer)
    (display-buffer output-buffer))
  )

(defun playwright-run-tests-debug ()
  "Run all Playwright tests in debug mode."
  (interactive)
  (let ((output-buffer (get-buffer-create "*Playwright Test File*")))
    (with-current-buffer output-buffer
      (erase-buffer)
      (insert "Running Playwright tests in debug mode...\n\n"))
    (shell-command "npx playwright test --headed --debug" output-buffer)
    (display-buffer output-buffer))
)

(defun playwright-popup ()
  "Show a popup buffer with Playwright commands."
  (interactive)
  (let ((command (completing-read "Select Playwright Command: "
                                  '("Run All Tests"
                                    "Run Single Test File"
                                    "Run Tests in Headed Mode"
				    "Run Tests in UI Mode"
				    "Run Tests in Debug Mode"
                                    ))))
    (cond ((equal command "Run All Tests") (playwright-run-all-tests))
          ((equal command "Run Single Test File") (call-interactively 'playwright-run-single-test-file))
          ((equal command "Run Tests in Headed Mode") (playwright-run-tests-headed))
	  ((equal command "Run Tests in UI Mode") (playwright-run-tests-ui))
	  ((equal command "Run Tests in Debug Mode") (playwright-run-tests-debug))
          ;; Add more cond clauses for other commands
          )))

(global-set-key (kbd "C-c p w") 'playwright-popup)

(provide 'playwright)
;;; playwright.el ends here 

