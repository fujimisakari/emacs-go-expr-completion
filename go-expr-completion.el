;;; go-expr-completion.el --- Complement the return values for Go -*- lexical-binding: t -*-

;; Copyright (C) 2020 by Ryo Fujimoto

;; Author: Ryo Fujimoto <fujimisakri@gmail.com>
;; URL: https://github.com/fujimisakari/emacs-go-expr-completion
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; `go-expr-completion.el' to complete a left-hand side from given expression for Go.
;;

;; To use this package, add these lines to your init.el or .emacs file:
;;
;; (with-eval-after-load 'go-mode
;;   (define-key go-mode-map (kbd "C-c C-c") 'go-expr-completion))
;;
;; ----------------------------------------------------------------
;;
;; Usage
;; Navigate your cursor to the arbitrary expression, type `C-c C-c` or `M-x go-expr-completion`,
;; and then this plugin completes the left-hand side for given expression (and `if err...` if necessary).
;;

;;; Code:

(require 'json)

(defconst go-expr-completion--tmp-file-name "./go-expr-completion.go"
  "Tmp file name.")

(defun go-expr-completion--byte-position-at-point ()
  "Get byte-position at point."
  (1- (position-bytes (point))))

(defun go-expr-completion--point-at-byte-position (position)
  "Get point at byte-position.
Argument POSITION is current position."
  (1+ (byte-to-position position)))

(defun go-expr-completion--execute-command ()
  "Execute `go-expr-completion' command."
  (let ((cmd (format "go-expr-completion -pos %s -file %s"
                     (shell-quote-argument (int-to-string (go-expr-completion--byte-position-at-point)))
                     (shell-quote-argument go-expr-completion--tmp-file-name))))
    (shell-command-to-string cmd)))

(defun go-expr-completion--single-return (start-pos end-pos value)
  "Left-hand side is single return case.
Argument START-POS is start postion of expression.
Argument END-POS is end position of expression.
Argument VALUE is return value metadata."
  (let ((name (cdr (pop value)))
        (type (cdr (pop value))))
    (if (equal type "error")
        (progn
          (goto-char (go-expr-completion--point-at-byte-position end-pos))
          (insert "; err != nil {")
          (newline-and-indent)
          (delete-char -1)
          (insert "}")
          (goto-char (go-expr-completion--point-at-byte-position start-pos))
          (insert "if err := "))
      (goto-char (go-expr-completion--point-at-byte-position start-pos))
      (insert (format "%s := " name)))
    (goto-char (go-expr-completion--point-at-byte-position start-pos))))

(defun go-expr-completion--multiple-return (start-pos end-pos values)
  "Left-hand side is multiple return case.
Argument START-POS is start postion of expression.
Argument END-POS is end position of expression.
Argument VALUES is return value metadata."
  (let ((names '())
        (types '()))
    (dolist (v values)
      (setq names (append names (list (cdr (pop v)))))
      (setq types (append types (list (cdr (pop v))))))
    (when (equal (car (reverse types)) "error")
      (goto-char (go-expr-completion--point-at-byte-position end-pos))
      (newline-and-indent)
      (insert (format "if %s != nil {" (car (reverse names))))
      (newline-and-indent)
      (delete-char -1)
      (insert "}"))
    (goto-char (go-expr-completion--point-at-byte-position start-pos))
    (insert (format "%s := " (mapconcat #'identity names ", ")))
    (goto-char (go-expr-completion--point-at-byte-position start-pos))))

(defun go-expr-completion--procedure ()
  "Go-expr-completion procedure."
  (write-region (point-min) (point-max) go-expr-completion--tmp-file-name nil 'nomsg)
  (let* ((ret (json-read-from-string (go-expr-completion--execute-command)))
         (start-pos (cdr (pop ret)))
         (end-pos (cdr (pop ret)))
         (values (append (cdr (pop ret)) nil)))
    (if (= (length values) 1)
        (go-expr-completion--single-return start-pos end-pos (car values))
      (go-expr-completion--multiple-return start-pos end-pos values))))

;;;###autoload
(defun go-expr-completion ()
  "Invoke `go-expr-completion'."
  (interactive)
  (unwind-protect
      (go-expr-completion--procedure)
    (delete-file go-expr-completion--tmp-file-name)))

(provide 'go-expr-completion)

;;; go-expr-completion.el ends here
