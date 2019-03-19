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

(defgroup frog-jump-buffer nil
  "Fast buffer switching interface."
  :group 'convenience
  :prefix "frog-jump-buffer-")

(defcustom frog-jump-buffer-ignore-buffers '("\\` ")
  "This is a list of regexps of buffer names to ignore or buffer-matching filter functions to use."
  :type '(repeat (choice regexp function)))

(defcustom frog-jump-buffer-max-buffers 12
  "This is the maximum number of buffers to show in the `frog-menu'."
  :type 'number)

(defcustom frog-jump-buffer-default-filter 'frog-jump-buffer-filter-all
  "This is the default filter to use when invoking
`frog-jump-buffer'. Shows all buffers by default."
  :type 'symbol)

(defcustom frog-jump-buffer-filter-actions
  '(("1" "[all]" frog-jump-buffer-filter-all)
    ("2" "[project]" frog-jump-buffer-filter-same-project)
    ("3" "[mode]" frog-jump-buffer-filter-same-mode)
    ("4" "[files]" frog-jump-buffer-filter-file-buffers))
  "These are the built-in buffer filter actions available during `frog-jump-buffer'.
Each action is a list of the form: (KEY DESCRIPTION FILTER-FUNCTION)."
  :type 'list)

(defun frog-jump-buffer-get-current-filter-name ()
  (condition-case err
      (nth 1 (car (-filter
                   (lambda (list)
                     (equal (symbol-name (car (last list)))
                            (symbol-name frog-jump-buffer-current-filter-function)))
                   frog-jump-buffer-filter-actions)))
    (error "[all]")))

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

(defun frog-jump-buffer-buffer-names ()
  "Filter and limit the number of buffers to show."
  (-take frog-jump-buffer-max-buffers
         (frog-jump-buffer-match (mapcar #'buffer-name (buffer-list)))))

(defvar frog-jump-buffer-target-other-window nil
  "This is a placeholder variable for determining which window to open the chosen buffer in.")

(defvar frog-jump-buffer-current-filter-function frog-jump-buffer-default-filter
  "This is a placeholder variable for determining which function to filter buffers by.")

(defun frog-jump-buffer-target-window-action ()
  "Return the `frog-menu' action for which window to target."
  (if frog-jump-buffer-target-other-window
      '(("0" "[same]" frog-jump-buffer-same-window))
    '(("0" "[other]" frog-jump-buffer-other-window))))

(defun frog-jump-buffer-actions ()
  "Determine the list of actions to show in `frog-jump-buffer'’s `frog-menu'."
  (let ((target-window-option (frog-jump-buffer-target-window-action)))
    (append frog-jump-buffer-filter-actions target-window-option)))

(defun frog-jump-buffer-handle-result (res)
  "Handle the result of `frog-menu-read' for `frog-jump-buffer'."
  (cond
   ((stringp res)
    (if frog-jump-buffer-target-other-window
        (switch-to-buffer-other-window res)
      (switch-to-buffer res)))
   ((eq res 'frog-jump-buffer-other-window)
    (let ((frog-jump-buffer-target-other-window t))
      (frog-jump-buffer)))
   ((eq res 'frog-jump-buffer-same-window)
    (let ((frog-jump-buffer-target-other-window nil))
      (frog-jump-buffer)))
   (t
    (let ((frog-jump-buffer-current-filter-function res))
      (frog-jump-buffer)))))

(defun frog-jump-buffer-prompt ()
  "This is the `frog-menu' prompt for `frog-menu-buffer'."
  (let ((filter-name (frog-jump-buffer-get-current-filter-name))
        (window-target (if frog-jump-buffer-target-other-window "[other]" "[same]")))
    (format "Showing Buffers: %s\nTarget Window: %s\n" filter-name window-target)))

(defun frog-jump-buffer-current-ignore-buffers ()
  "Return all the filters and regex rejections."
  (-non-nil (append frog-jump-buffer-ignore-buffers (list frog-jump-buffer-current-filter-function))))

;;;###autoload
(defun frog-jump-buffer ()
  "Present a `frog-menu' for jumping to an open buffer.
If FILTER-FUNCTION is present, filter the buffer-list with it."
  (interactive)
  (let* ((frog-menu-avy-padding t)
         (frog-menu-grid-column-function (lambda () 1))
         (frog-jump-buffer-current-ignore-buffers (frog-jump-buffer-current-ignore-buffers))
         (buffer-names (frog-jump-buffer-buffer-names))
         (actions (frog-jump-buffer-actions))
         (prompt (frog-jump-buffer-prompt))
         (res (frog-menu-read prompt buffer-names actions)))
    (unless res
      (error "Quit"))
    (frog-jump-buffer-handle-result res)))

(provide 'frog-jump-buffer)
;;; frog-jump-buffer.el ends here
