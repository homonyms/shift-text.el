;;; shift-text-tests.el --- Tests for shift-text.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Dohna <pub@lya.moe>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(load (expand-file-name "./shift-text.el"))

(defvar shift-text-indent-modes)
(declare-function shift-text--state-create "shift-text")
(declare-function shift-text--forward-line "shift-text" (state n))
(declare-function shift-text-down "shift-text" (n))
(declare-function shift-text-up "shift-text" (n))

(defmacro st-test (name initial-text expected-result-text &rest body)
  (declare (indent defun))
  (let ((test-name (intern (concat "shift-text-"
                                   (symbol-name name)
                                   "-test"))))
    `(ert-deftest ,test-name ()
       (let ((shift-text-indent-modes
              (cons 'fundamental-mode shift-text-indent-modes)))
         (with-temp-buffer
           (let (p-mark m-mark nb-mark ne-mark)
             (insert ,initial-text)
             (goto-char (point-min))
             (while (re-search-forward "[<>|]" nil t)
               (let ((m (match-string 0))
                     (p (copy-marker (match-beginning 0))))
                 (cond ((string= m ">") (setq p-mark p))
                       ((string= m "<") (setq m-mark p))
                       ((string= m "|") (if nb-mark (setq ne-mark p) (setq nb-mark p))))
                 (delete-region (match-beginning 0) (match-end 0))))

             (transient-mark-mode 1)
             (goto-char (or p-mark (point-min)))
             (when m-mark
               (set-mark m-mark)
               (activate-mark))
             (when (and nb-mark ne-mark)
               (narrow-to-region nb-mark ne-mark))

             ,@body

             (let* ((act-p (point-marker))
                    (act-m (and (use-region-p) (mark-marker)))
                    (act-nb (copy-marker (point-min)))
                    (act-ne (copy-marker (point-max)))
                    (was-narrowed (or (/= (point-min) 1)
                                      (/= (point-max) (1+ (buffer-size)))))
                    res)
               (save-restriction
                 (widen)
                 (setq res (buffer-string))
                 (let* ((m-prec '(("|" . 1) ("<" . 2) (">" . 3)))
                        (markers (sort (delete nil (list (cons act-p ">")
                                                         (when act-m (cons act-m "<"))
                                                         (when was-narrowed (cons act-nb "|"))
                                                         (when was-narrowed (cons act-ne "|"))))
                                       (lambda (a b)
                                         (if (= (car a) (car b))
                                             (> (cdr (assoc (cdr a) m-prec))
                                                (cdr (assoc (cdr b) m-prec)))
                                           (> (car a) (car b)))))))
                   (dolist (m markers)
                     (let ((pos (min (length res) (1- (marker-position (car m))))))
                       (setq res (concat (substring res 0 pos)
                                         (cdr m)
                                         (substring res pos))))))
                 (should (string= res ,expected-result-text))))))))))

(defun st-test--state (&rest args)
  (apply #'shift-text--state-create args))

(st-test forward-down-1
         ">Line 1\nLine 2\nLine 3"
         "Line 1\n>Line 2\nLine 3"
         (shift-text--forward-line (st-test--state) 1))

(st-test forward-down-1-eob
         "Line 1\nLine 2\nLine 3>"
         "Line 1\nLine 2\nLine 3\n>"
         (shift-text--forward-line (st-test--state) 1))

(st-test forward-down-2-eob
         "Line 1\nLine 2\nLine 3>"
         "Line 1\nLine 2\nLine 3\n\n>"
         (shift-text--forward-line (st-test--state) 2))

(st-test forward-down-2
         ">Line 1\nLine 2\nLine 3"
         "Line 1\nLine 2\n>Line 3"
         (shift-text--forward-line (st-test--state) 2))

(st-test forward-down-3
         ">Line 1\nLine 2\nLine 3"
         "Line 1\nLine 2\nLine 3\n>"
         (shift-text--forward-line (st-test--state) 3))

(st-test forward-up-1
         "Line 1\nLine 2\n>Line 3"
         "Line 1\n>Line 2\nLine 3"
         (shift-text--forward-line (st-test--state) -1))

(st-test forward-up-2
         "Line 1\nLine 2\n>Line 3"
         ">Line 1\nLine 2\nLine 3"
         (shift-text--forward-line (st-test--state) -2))

(st-test forward-up-3
         "Line 1\nLine 2\n>Line 3"
         ">\nLine 1\nLine 2\nLine 3"
         (shift-text--forward-line (st-test--state) -3))

(st-test forward-up-1-bob
         ">Line 1\nLine 2\nLine 3"
         ">\nLine 1\nLine 2\nLine 3"
         (shift-text--forward-line (st-test--state) -1))

(st-test forward-up-2-bob
         ">Line 1\nLine 2\nLine 3"
         ">\n\nLine 1\nLine 2\nLine 3"
         (shift-text--forward-line (st-test--state) -2))

(st-test move-down-1
         ">Line 1\nLine 2\nLine 3"
         "Line 2\n>Line 1\nLine 3"
         (shift-text-down 1))

(st-test move-down-1-1
         "L>ine 1\nLine 2\nLine 3"
         "Line 2\nL>ine 1\nLine 3"
         (shift-text-down 1))

(st-test move-down-1-eob
         "Line 1\nLine 2\nLine 3>"
         "Line 1\nLine 2\n\nLine 3>"
         (shift-text-down 1))

(st-test move-down-1-eob-1
         "Line 1\nLine 2\nL>ine 3"
         "Line 1\nLine 2\n\nL>ine 3"
         (shift-text-down 1))

(st-test move-down-2
         ">Line 1\nLine 2\nLine 3"
         "Line 2\nLine 3\n>Line 1\n"
         (shift-text-down 2))

(st-test move-down-2-1
         "L>ine 1\nLine 2\nLine 3"
         "Line 2\nLine 3\nL>ine 1\n"
         (shift-text-down 2))

(st-test move-down-2-eob
         "Line 1\nLine 2\nLine 3>"
         "Line 1\nLine 2\n\n\nLine 3>"
         (shift-text-down 2))

(st-test move-down-2-eob-1
         "Line 1\nLine 2\nL>ine 3"
         "Line 1\nLine 2\n\n\nL>ine 3"
         (shift-text-down 2))

(st-test move-down-3
         ">Line 1\nLine 2\nLine 3"
         "Line 2\nLine 3\n\n>Line 1\n"
         (shift-text-down 3))

(st-test move-down-region-1
         "<Line> 1\nLine 2\nLine 3"
         "1\n<Line>\nLine 2\nLine 3"
         (shift-text-down 1))

(st-test move-down-region-1-eob
         "Line 1\nLine 2\n<Line> 3"
         "Line 1\nLine 2\n3\n<Line>"
         (shift-text-down 1))

(st-test move-down-region-2
         "<Line> 1\nLine 2\nLine 3"
         "1\nLine 2\n<Line>\nLine 3"
         (shift-text-down 2))

(st-test move-down-region-trim-1
         "Line 1\n <Line 2>\nLine 3"
         "Line 1\nLine 3\n<Line 2>\n"
         (shift-text-down 1))

(st-test move-up-1
         "Line 1\nLine 2\n>Line 3"
         "Line 1\n>Line 3\nLine 2\n"
         (shift-text-up 1))

(st-test move-up-1-1
         "Line 1\nLine 2\nL>ine 3"
         "Line 1\nL>ine 3\nLine 2\n"
         (shift-text-up 1))

(st-test move-up-1-bob
         ">Line 1\nLine 2\nLine 3"
         ">Line 1\n\nLine 2\nLine 3"
         (shift-text-up 1))

(st-test move-up-1-bob-1
         "L>ine 1\nLine 2\nLine 3"
         "L>ine 1\n\nLine 2\nLine 3"
         (shift-text-up 1))

(st-test move-up-2
         "Line 1\nLine 2\n>Line 3"
         ">Line 3\nLine 1\nLine 2\n"
         (shift-text-up 2))

(st-test move-up-2-1
         "Line 1\nLine 2\nL>ine 3"
         "L>ine 3\nLine 1\nLine 2\n"
         (shift-text-up 2))

(st-test move-up-2-bob
         ">Line 1\nLine 2\nLine 3"
         ">Line 1\n\n\nLine 2\nLine 3"
         (shift-text-up 2))

(st-test move-up-2-bob-1
         "L>ine 1\nLine 2\nLine 3"
         "L>ine 1\n\n\nLine 2\nLine 3"
         (shift-text-up 2))

(st-test move-up-3
         "Line 1\nLine 2\n>Line 3"
         ">Line 3\n\nLine 1\nLine 2\n"
         (shift-text-up 3))

(st-test move-up-region-1
         "Line 1\nLine 2\n<Line> 3"
         "Line 1\nLine 2\n<Line>\n3"
         (shift-text-up 1))

(st-test move-up-region-1-bob
         "<Line> 1\nLine 2\nLine 3"
         "<Line>\n1\nLine 2\nLine 3"
         (shift-text-up 1))

(st-test move-up-region-2
         "Line 1\nLine 2\n<Line> 3"
         "Line 1\n<Line>\nLine 2\n3"
         (shift-text-up 2))

(st-test move-up-region-2-bob
         "<Line> 1\nLine 2\nLine 3"
         "<Line>\n\n1\nLine 2\nLine 3"
         (shift-text-up 2))

(st-test move-up-region-trim-1
         "Line 1\n <Line 2>\nLine 3"
         "<Line 2>\nLine 1\nLine 3"
         (shift-text-up 1))

(st-test move-down-no-indent
         "  >Line 1\nLine 2"
         "Line 2\n  >Line 1\n"
         (let ((shift-text-indent-modes nil))
           (shift-text-down 1)))

(st-test move-up-no-indent
         "Line 1\n  >Line 2"
         "  >Line 2\nLine 1\n"
         (let ((shift-text-indent-modes nil))
           (shift-text-up 1)))

(ert-deftest shift-text-read-only-test ()
  (with-temp-buffer
    (insert "Line 1\nLine 2")
    (setq buffer-read-only t)
    (goto-char (point-min))
    (should-error (shift-text-down 1) :type 'buffer-read-only)))

(st-test move-down-multi-line-region
         "<Line 1\nLine 2>\nLine 3"
         "Line 3\n<Line 1\nLine 2>\n"
         (shift-text-down 1))

(st-test move-up-multi-line-region
         "Line 1\n<Line 2\nLine 3>"
         "<Line 2\nLine 3>\nLine 1\n"
         (shift-text-up 1))

(st-test move-down-1-narrowing
         "Line 1\n|>Line 2\nLine 3\n|Line 4"
         "Line 1\n|Line 3\n>Line 2\n|Line 4"
         (shift-text-down 1))

(st-test move-up-1-narrowing
         "Line 1\n|Line 2\n>Line 3\n|Line 4"
         "Line 1\n|>Line 3\nLine 2\n|Line 4"
         (shift-text-up 1))

;;; New Scenarios

(st-test move-down-empty-line
         ">\nLine 2"
         "Line 2\n>\n"
         (shift-text-down 1))

(st-test move-up-empty-line
         "Line 1\n>"
         ">\nLine 1\n"
         (shift-text-up 1))

(st-test move-down-whitespace-line
         "  >\nLine 2"
         "Line 2\n  >\n"
         (let ((shift-text-indent-modes nil))
           (shift-text-down 1)))

(st-test move-down-partial-line
         "Prefix <Selected> Suffix\nLine 2"
         "Prefix  Suffix\n<Selected>\nLine 2"
         (shift-text-down 1))

(st-test move-up-partial-line
         "Line 1\nPrefix <Selected> Suffix"
         "Line 1\n<Selected>\nPrefix  Suffix"
         (shift-text-up 1))

(st-test move-down-multi-line-with-empty
         "<Line 1\n\nLine 2>\nLine 3"
         "Line 3\n<Line 1\n\nLine 2>\n"
         (shift-text-down 1))

(st-test move-up-at-narrowing-top
         "Line 1\n|>Line 2\nLine 3|"
         "Line 1\n|>Line 2\n\nLine 3|"
         (shift-text-up 1))

(st-test move-down-at-narrowing-bottom
         "|Line 1\nLine 2>|\nLine 3"
         "|Line 1\n\nLine 2|>\nLine 3"
         (shift-text-down 1))

(st-test move-up-past-bob
         "Line 1\nLine 2\n>Line 3"
         ">Line 3\n\n\n\nLine 1\nLine 2\n"
         (shift-text-up 5))

(st-test move-down-past-eob
         ">Line 1\nLine 2\nLine 3"
         "Line 2\nLine 3\n\n\n\n>Line 1\n"
         (shift-text-down 5))

(st-test move-down-point-at-eol-whitespace
         "Line 1   >\nLine 2"
         "Line 2\nLine 1   >\n"
         (shift-text-down 1))

(st-test move-up-point-at-eol-whitespace
         "Line 1\nLine 2   >"
         "Line 2   >\nLine 1\n"
         (shift-text-up 1))

(st-test move-down-zero
         "  >Line 1"
         "  >Line 1"
         (shift-text-down 0))

(provide 'shift-text-tests)
;;; shift-text-tests.el ends here
