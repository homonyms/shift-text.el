;;; shift-text.el --- Shift text up/down DWIM        -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Dohna <pub@lya.moe>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Dohna <pub@lya.moe>
;; Keywords: convenience
;; URL: https://codeberg.org/dohna/shift-text.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

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

;;; Commentary:

;; This package provides commands to move the current line or active region
;; up or down, often referred to as "dragging" lines.  It "Does What I Mean"
;; by automatically detecting whether to move a single line or the selected
;; region.
;;
;; Key features:
;; - Move current line or active region up/down with `shift-text-up' and
;;   `shift-text-down'.
;; - Automatically re-indents the moved text in supported major modes
;;   (see `shift-text-indent-modes').
;;
;; Usage:
;;
;; Bind the commands to your preferred keys, for example:
;;
;;   (global-set-key (kbd "M-<up>") #'shift-text-up)
;;   (global-set-key (kbd "M-<down>") #'shift-text-down)
;;
;; You can also provide a prefix argument to move by multiple lines:
;; `C-u 5 M-<up>' will move the text 5 lines up.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defgroup shift-text nil
  "Text shifting operations."
  :prefix "shift-text-"
  :group 'convenience
  :group 'tools)

(defcustom shift-text-indent-modes '(prog-mode)
  "List of major modes in which to allow text indentation.
The check is performed using `derived-mode-p`, so including a
parent mode like `prog-mode` will cover all its children."
  :type '(repeat symbol)
  :group 'shift-text)

(cl-defstruct (shift-text--state
               (:constructor shift-text--state-create))
  use-region-p
  trim-start-p
  trim-end-p
  move-up-p
  point-offset
  mark-offset
  indent-enabled-p
  indent-start-marker)

(defun shift-text--trim-both-p (state)
  (and (shift-text--state-trim-start-p state)
       (shift-text--state-trim-end-p state)))

(defsubst shift-text--line-beginning-position ()
  (max (line-beginning-position) (point-min)))

(defsubst shift-text--line-end-position ()
  (min (line-end-position) (point-max)))

(defun shift-text--backward-whitespaces ()
  (skip-chars-backward "[:space:]" (shift-text--line-beginning-position)))

(defun shift-text--forward-whitespaces ()
  (skip-chars-forward "[:space:]" (shift-text--line-end-position)))

(defun shift-text--get-bounds ()
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (cons (shift-text--line-beginning-position)
          (shift-text--line-end-position))))

(defun shift-text--prepare-state (n)
  (let* ((bounds (shift-text--get-bounds))
         (origin (car bounds))
         (use-region (use-region-p)))
    (shift-text--state-create
     :use-region-p use-region
     :move-up-p (< n 0)
     :point-offset (- (point) origin)
     :mark-offset (when use-region
                    (- (mark) origin))
     :indent-enabled-p (derived-mode-p shift-text-indent-modes)
     :indent-start-marker (make-marker))))

(defun shift-text--forward-line (state n)
  (when (shift-text--state-indent-enabled-p state)
    (set-marker (shift-text--state-indent-start-marker state)
                (if (shift-text--state-move-up-p state)
                    (shift-text--line-end-position)
                  (shift-text--line-beginning-position))))

  (beginning-of-line)
  (let* (;; not empty line
         (bolp (and (bolp) (not (eolp))))
         (l (forward-line n)))
    ;; move down: moves to eol, there is no newline character
    (when (and bolp (not (bolp))
               (eobp))
      (setq l (1+ l)))
    (unless (zerop l)
      (save-excursion
        (insert-char ?\n (abs l)))
      (forward-line l))))

(defun shift-text--extract-region (state &optional beg end)
  (unless (and beg end)
    (let ((bounds (shift-text--get-bounds)))
      (setq beg (car bounds)
            end (cdr bounds))))

  (goto-char beg)
  (shift-text--backward-whitespaces)
  (setf (shift-text--state-trim-start-p state) (bolp))

  (goto-char end)
  (shift-text--forward-whitespaces)
  (setf (shift-text--state-trim-end-p state) (eolp))
  (when (and (shift-text--state-indent-enabled-p state)
             (shift-text--state-trim-end-p state))
    (delete-region (point) end))

  (if (and (shift-text--trim-both-p state)
           (eq (char-after) ?\n))
      ;; | <Line 1> \n| -> |<Line 1\n>|
      (setq end (1+ end))
    ;; |<Line> 1\n| -> |<Line\n> 1\n| or |Line <1>\n| -> |Line <1\n>\n|
    (goto-char end)
    (when (or (shift-text--state-move-up-p state)
              (not (save-excursion
                     (end-of-line)
                     (eobp))))
      (insert "\n")
      (setq end (point))))
  (let ((ret (delete-and-extract-region beg end)))
    (when (and (shift-text--state-indent-enabled-p state)
               (shift-text--state-trim-start-p state))
      (delete-region (point) (shift-text--line-beginning-position)))
    ret))

(defun shift-text--insert-text (state text)
  (let ((beg (point)))
    (insert text)
    (goto-char (+ beg (shift-text--state-point-offset state)))
    (when (shift-text--state-use-region-p state)
      (set-marker
       (mark-marker)
       (+ beg (shift-text--state-mark-offset state))))))

(defun shift-text--indent (state)
  (when (shift-text--state-indent-enabled-p state)
    (let* ((has-region (use-region-p))
           (bounds (shift-text--get-bounds))
           (beg (car bounds))
           (end (cdr bounds))
           (start-point (marker-position
                         (shift-text--state-indent-start-marker state)))
           (inhibit-message t))
      (unless (shift-text--trim-both-p state)
        (setq beg (min beg start-point)
              end (max end start-point)))
      (indent-region beg end)
      (when has-region
        (when (> (mark) (point))
          (back-to-indentation))))))

(defun shift-text--move (n)
  (unless (zerop n)
    (let ((state (shift-text--prepare-state n)))
      (unwind-protect
          (atomic-change-group
            (let ((auto-fill-function nil)
                  (text (shift-text--extract-region state))
                  (n (if (and (shift-text--state-move-up-p state)
                              (not (shift-text--trim-both-p state)))
                         (1+ n)
                       n)))
              (shift-text--forward-line state n)
              (shift-text--insert-text state text)
              (shift-text--indent state)
              (when (shift-text--state-use-region-p state)
                (setq deactivate-mark nil))))
        (set-marker (shift-text--state-indent-start-marker state) nil)))))

;;;###autoload
(defun shift-text-up (n)
  "Move the current line or region up N lines."
  (interactive "p")
  (shift-text--move (- n)))

;;;###autoload
(defun shift-text-down (n)
  "Move the current line or region down N lines."
  (interactive "p")
  (shift-text--move n))

(provide 'shift-text)
;;; shift-text.el ends here
