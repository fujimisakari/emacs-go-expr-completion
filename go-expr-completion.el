;;; go-expr-completion --- complete a left-hand side from given expression for Go  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 by Ryo Fujimoto

;; Author: Ryo Fujimoto <fujimisakri@gmail.com>
;; URL: https://github.com/fujimisakari/emacs-go-expr-completion
;; Version: 1.0.0

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
;; (when (require 'go-expr-completion nil t)
;;   (require 'go-expr-completion)
;;   (define-key go-mode-map (kbd "C-c C-c") 'go-expr-completion))
;;
;; ----------------------------------------------------------------
;;
;; Usage
;; And navigate your cursor to the arbitrary expression, type `C-c C-c` or `M-x go-expr-completion`,
;; and then this plugin completes the left-hand side for given expression (and `if err...` if necessary).
;;

;;; Code:

(require 'json)

(defvar go-expr-completion--tmp-file-name "./go-expr-completion.go"
  "tmp file name")

(defun go-expr-completion--byte-offset-at-point ()
  (1- (position-bytes (point))))

(defun go-expr-completion--point-at-byte-offset (offset)
  (1+ (byte-to-position offset)))

(defun go-expr-completion--execute-command ()
  (let ((cmd (format "go-expr-completion -pos %d -file %s" (go-expr-completion--byte-offset-at-point) go-expr-completion--tmp-file-name)))
    (shell-command-to-string cmd)))

(defun go-expr-completion--single-expression (start-pos end-pos value)
  (let ((name (cdr (pop value)))
        (type (cdr (pop value))))
    (if (equal type "error")
        (progn
          (goto-char (go-expr-completion--point-at-byte-offset end-pos))
          (insert "; err != nil {")
          (newline-and-indent)
          (delete-char -1)
          (insert "}")
          (goto-char (go-expr-completion--point-at-byte-offset start-pos))
          (insert "if err := "))
      (progn
        (goto-char (go-expr-completion--point-at-byte-offset start-pos))
        (insert (format "%s := " name))))
    (goto-char (go-expr-completion--point-at-byte-offset start-pos))))

(defun go-expr-completion--multiple-expression (start-pos end-pos values)
  (let ((names '())
        (types '()))
    (dolist (v values)
      (setq names (append names (list (cdr (pop v)))))
      (setq types (append types (list (cdr (pop v))))))
    (when (equal (car (reverse types)) "error")
      (goto-char (go-expr-completion--point-at-byte-offset end-pos))
      (newline-and-indent)
      (insert (format "if %s != nil {" (car (reverse names))))
      (newline-and-indent)
      (delete-char -1)
      (insert "}"))
    (goto-char (go-expr-completion--point-at-byte-offset start-pos))
    (insert (format "%s := " (mapconcat #'identity names ", ")))
    (goto-char (go-expr-completion--point-at-byte-offset start-pos))))

(defun go-expr-completion--procedure ()
  (write-region (point-min) (point-max) go-expr-completion--tmp-file-name nil 'nomsg)
  (let* ((ret (json-read-from-string (go-expr-completion--execute-command)))
         (start-pos (cdr (pop ret)))
         (end-pos (cdr (pop ret)))
         (values (coerce (cdr (pop ret)) 'list)))
    (if (= (length values) 1)
        (go-expr-completion--single-expression start-pos end-pos (car values))
      (go-expr-completion--multiple-expression start-pos end-pos values))))

(defun go-expr-completion ()
  (interactive)
  (unwind-protect
      (go-expr-completion--procedure)
    (delete-file go-expr-completion--tmp-file-name)))

(provide 'go-expr-completion)
