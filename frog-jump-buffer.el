;;; frog-jump-buffer.el --- The fastest buffer-jumping Emacs lisp package around.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Justin Talbott

;; Author: Justin Talbott
;; URL: https://github.com/waymondo/frog-jump-buffer
;; Version: 0.1.0
;; Package-Requires: ((avy "0.4.0") (dash "2.4.0") (frog-menu "0.1") (projectile "2.0.0"))
;; License: GNU General Public License version 3, or (at your option) any later version
;; Keywords: convenience, tools

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

;; `frog-jump-buffer' allows you to hop to any Emacs buffer in 2-3 key strokes.

;; `(frog-jump-buffer)' is the main entry-point. Bind it to your preferred key-binding.

;; It opens the `frog-menu' buffer selector. The buffers appear in order
;; of most recent display or selection.

;; Selecting the `avy' character next to a buffer switches to that
;; buffer.

;; Use `0' to toggle between opening in the same window or
;; `(other-window)'.

;; The numbers 1 through 4 will cycle through the default buffer filters.

;;; Code:

(require 'avy)
(require 'dash)
(require 'frog-menu)
(require 'projectile)

(defcustom frog-jump-buffer-ignore-buffers '("\\` ")
  "This is a list of regexps of buffer names to ignore or buffer-matching filter functions to use."
  :type '(repeat (choice regexp function)))

(defcustom frog-jump-buffer-max-buffers 12
  "This is the maximum number of buffers to show in the `frog-menu'."
  :type 'number)

(defcustom frog-jump-buffer-filter-actions
  '(("1" "same project" frog-jump-buffer-filter-same-project)
    ("2" "same mode" frog-jump-buffer-filter-same-mode)
    ("3" "files" frog-jump-buffer-filter-file-buffers)
    ("4" "all" frog-jump-buffer-filter-all))
  "These are the built-in buffer filter actions available during `frog-jump-buffer'.
Each action is a list of the form: (KEY DESCRIPTION FILTER-FUNCTION)."
  :type 'list)

(defun frog-jump-buffer-filter-same-project (buffer)
  "Check if a buffer is the same project."
  (let ((project-root (projectile-project-root)))
    (with-current-buffer buffer
      (projectile-project-buffer-p buffer project-root))))

(defun frog-jump-buffer-filter-same-mode (buffer)
  "Check if a buffer is the same as the current major mode."
  (let ((current-mode major-mode))
    (with-current-buffer buffer
      (eq major-mode current-mode))))

(defun frog-jump-buffer-filter-file-buffers (buffer)
  "Check if a buffer is backed by a real file."
  (buffer-file-name buffer))

(defun frog-jump-buffer-filter-all (_buffer)
  "Return all possible buffers."
  t)

(defvar frog-jump-buffer-current-ignore-buffers nil
  "This is a placeholder variable for the currently active ignore buffer filters.")

(defun frog-jump-buffer-match (buffers)
  "Process the `frog-jump-buffer-current-ignore-buffers' filters for all buffers."
  (cl-remove-if
   (lambda (buf)
     (cl-find-if
      (lambda (f-or-r)
        (if (functionp f-or-r)
            (not (funcall f-or-r (get-buffer buf)))
          (string-match-p f-or-r buf)))
      frog-jump-buffer-current-ignore-buffers))
   buffers))

(defmacro frog-jump-buffer-with-settings (&rest body)
  "Wrap `frog-jump-buffer' with `frog-menu' overrides."
  `(let* ((frog-menu-avy-padding t)
          (frog-menu-grid-column-function (lambda () 1)))
     ,@body))

(defun frog-jump-buffer-buffer-names ()
  "Filter and limit the number of buffers to show."
  (-take frog-jump-buffer-max-buffers
         (frog-jump-buffer-match (mapcar #'buffer-name (buffer-list)))))

(defvar frog-jump-buffer-target-other-window nil
  "This is a placeholder variable for determining which window to open the chosen buffer in.")

(defun frog-jump-buffer-actions ()
  "Determine the list of actions to show in `frog-jump-buffer'’s `frog-menu'."
  (let ((target-window-option
         (if frog-jump-buffer-target-other-window
             '(("0" "same window" frog-jump-buffer-same-window))
           '(("0" "other window" frog-jump-buffer-other-window)))))
    (append frog-jump-buffer-filter-actions target-window-option)))

(defun frog-jump-buffer-handle-result (filter-function res)
  "Handle the result of `frog-menu-read' for `frog-jump-buffer'."
  (cond
   ((stringp res)
    (if frog-jump-buffer-target-other-window
        (switch-to-buffer-other-window res)
      (switch-to-buffer res)))
   ((or (eq res 'frog-jump-buffer-other-window)
        (eq res 'frog-jump-buffer-same-window))
    (funcall res filter-function))
   (t
    (frog-jump-buffer res))))

;;;###autoload
(defun frog-jump-buffer (&optional filter-function)
  "Present a `frog-menu' for jumping to an open buffer.
If FILTER-FUNCTION is present, filter the buffer-list with it."
  (interactive)
  (frog-jump-buffer-with-settings
   (let* ((frog-jump-buffer-current-ignore-buffers
           (-non-nil (append frog-jump-buffer-ignore-buffers (list filter-function))))
          (buffer-names (frog-jump-buffer-buffer-names))
          (actions (frog-jump-buffer-actions))
          (res (frog-menu-read "" buffer-names actions)))
     (unless res
       (error "Quit"))
     (frog-jump-buffer-handle-result filter-function res))))

;;;###autoload
(defun frog-jump-buffer-other-window (&optional filter-function)
  "Run `frog-jump-buffer' with the intention of opening the selected buffer in `other-window'."
  (let ((frog-jump-buffer-target-other-window t))
    (frog-jump-buffer filter-function)))

;;;###autoload
(defun frog-jump-buffer-same-window (&optional filter-function)
  "Run `frog-jump-buffer' with the intention of opening the selected buffer in the current window."
  (let ((frog-jump-buffer-target-other-window nil))
    (frog-jump-buffer filter-function)))

(provide 'frog-jump-buffer)
;;; frog-jump-buffer.el ends here
