;;; frog-jump-buffer.el --- The fastest buffer-jumping Emacs lisp package around.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Justin Talbott

;; Author: Justin Talbott
;; URL: https://github.com/waymondo/frog-jump-buffer
;; Version: 0.1.5
;; Package-Requires: ((emacs "24") (avy "0.4.0") (dash "2.4.0") (frog-menu "0.2.8"))
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

;; `(frog-jump-buffer)' is the main entry-point.  Bind it to your preferred key-binding.

;; It opens the `frog-menu' buffer selector.  The buffers appear in order
;; of most recent display or selection.

;; Selecting the `avy' character next to a buffer switches to that
;; buffer.

;; Use `0' to toggle between opening in the same window or
;; `(other-window)'.

;; The numbers 1 through 6 will cycle through the default buffer filters.

;;; Code:

(require 'avy)
(require 'dash)
(require 'frog-menu)

(defgroup frog-jump-buffer nil
  "Fast buffer switching interface."
  :group 'convenience
  :prefix "frog-jump-buffer-")

(defcustom frog-jump-buffer-sort '(lambda(one two) nil)
  "User defined sorting function"
  :type 'function)

(defcustom frog-jump-buffer-ignore-buffers '("\\` ")
  "This is a list of regexps of buffer names to ignore or buffer-matching filter functions to use."
  :type '(repeat (choice regexp function)))

(defcustom frog-jump-buffer-max-buffers 12
  "This is the maximum number of buffers to show in the `frog-menu'."
  :type 'number)

(defcustom frog-jump-buffer-default-filter 'frog-jump-buffer-filter-all
  "This is the default filter to use when invoking `frog-jump-buffer'.
Shows all buffers by default."
  :type 'symbol)

(defcustom frog-jump-buffer-include-current-buffer t
  "Set to `nil' to remove the current buffer from always being the first option."
  :type 'boolean)

(defcustom frog-jump-buffer-posframe-parameters frog-menu-posframe-parameters
  "Explicit frame parameters to be used by the posframe `frog-jump-buffer' creates."
  :type 'list)

(defcustom frog-jump-buffer-posframe-handler 'posframe-poshandler-point-bottom-left-corner
  "This is the posframe handler that `frog-jump-buffer' should use."
  :type 'function)

(defcustom frog-jump-buffer-default-filters-capital-letters t
  "If non-nil, use capital letters instead of numbers for the default filters."
  :type 'boolean)

(defcustom frog-jump-buffer-use-default-filter-actions t
  "Set to nil to only use the user-defined filter actions value of `frog-jump-buffer-filter-actions'."
  :type 'boolean)

(defcustom frog-jump-buffer-filter-actions '()
  "User defined buffer filter actions available during `frog-jump-buffer'.
Each action is a list of the form: (KEY DESCRIPTION FILTER-FUNCTION)."
  :type 'list)

(defcustom frog-jump-buffer-use-all-the-icons-ivy nil
  "Enable support for adding icons to the `frog-jump-buffer'.
Warning: experimental. Requires installing `all-the-icons-ivy'."
  :type 'boolean)

(defcustom frog-jump-buffer-project-package (if (require 'projectile nil t) 'projectile 'project)
  "Set the project package to use. Defaults to the popular `projectile' if installed.
You can set it to `project' if you have Emacs 28+ installed, or
this will be the default if `projectile' not available."
  :type 'symbol)

(defun frog-jump-buffer-default-filter-actions ()
  "The built-in buffer filter actions available during `frog-jump-buffer'.
Each action is a list of the form: (KEY DESCRIPTION FILTER-FUNCTION)."
  `((,(if frog-jump-buffer-default-filters-capital-letters "A" "1")
     "[all]" frog-jump-buffer-filter-all)
    (,(if frog-jump-buffer-default-filters-capital-letters "M" "2")
     "[mode]" frog-jump-buffer-filter-same-mode)
    (,(if frog-jump-buffer-default-filters-capital-letters "F" "3")
     "[files]" frog-jump-buffer-filter-file-buffers)
    (,(if frog-jump-buffer-default-filters-capital-letters "R" "4")
     "[recentf]" frog-jump-buffer-filter-recentf)
    (,(if frog-jump-buffer-default-filters-capital-letters "P" "5")
     "[project]" frog-jump-buffer-filter-same-project)
    (,(if frog-jump-buffer-default-filters-capital-letters "S" "6")
     "[similar]" frog-jump-buffer-filter-similar-name)))

(defun frog-jump-buffer-filter-actions ()
  "Construct the full list of filter actions to use during `frog-jump-buffer'."
  (if frog-jump-buffer-use-default-filter-actions
      (append (frog-jump-buffer-default-filter-actions) frog-jump-buffer-filter-actions)
    frog-jump-buffer-filter-actions))

(defvar frog-jump-buffer-current-filter-function nil
  "This is a placeholder variable for determining which function to filter buffers by.")

(defun frog-jump-buffer-get-current-filter-name ()
  "Get the current filter’s name."
  (condition-case _err
      (nth 1 (car (-filter
                   (lambda (list)
                     (equal (symbol-name (car (last list)))
                            (symbol-name frog-jump-buffer-current-filter-function)))
                   (frog-jump-buffer-filter-actions))))
    (error "[all]")))

(defun frog-jump-buffer-filter-same-project (buffer)
  "Check if a BUFFER is the same project."
  (cond
   ((eq frog-jump-buffer-project-package 'projectile)
    (let ((project-root (projectile-project-root)))
      (when project-root
        (with-current-buffer buffer
          (projectile-project-buffer-p buffer project-root)))))
   ((eq frog-jump-buffer-project-package 'project)
    (let ((project-root (project-current)))
      (when project-root
        (member buffer (project-buffers (project-current))))))))

(defun frog-jump-buffer-filter-same-mode (buffer)
  "Check if a BUFFER is the same as the current major mode."
  (let ((current-mode major-mode))
    (with-current-buffer buffer
      (eq major-mode current-mode))))

(defun frog-jump-buffer-name-substrings (buffer-name)
  "Split a buffer name into substrings minus the extention."
  (let ((buffer-name-without-extention
         (car (split-string buffer-name (rx (and "." (one-or-more alphanumeric) line-end))))))
    (split-string buffer-name-without-extention "[[:blank:]-_.]+")))

(defun frog-jump-buffer-filter-similar-name (buffer)
  "Check if the BUFFER is similarly named as the current buffer."
  (let ((current-buffer-name-substrings
         (frog-jump-buffer-name-substrings (buffer-name (current-buffer)))))
    (with-current-buffer buffer
      (> (length (-intersection (frog-jump-buffer-name-substrings (buffer-name buffer))
                                current-buffer-name-substrings)) 0))))

(defun frog-jump-buffer-filter-file-buffers (buffer)
  "Check if a BUFFER is backed by a real file."
  (buffer-file-name buffer))

(defun frog-jump-buffer-filter-all (_buffer)
  "Return all possible buffers."
  t)

(defvar frog-jump-buffer-include-virtual-buffers nil
  "This is a placeholder variable using `recentf' instead of `buffer-list'.")

(defun frog-jump-buffer-filter-recentf (_buffer)
  "Return all buffers from `recentf'."
  t)

(defun frog-jump-buffer-recentf-buffers ()
  "Adapted from `ivy--virtual-buffers'."
  (unless recentf-mode
    (recentf-mode 1))
  (let (buffers)
    (dolist (head recentf-list)
      (let* ((file-name (if (stringp head) head (cdr head)))
             (name (file-name-nondirectory file-name)))
        (when (equal name "")
          (setq name
                (if (consp head)
                    (car head)
                  (file-name-nondirectory (directory-file-name file-name)))))
        (unless (or (equal name "")
                    (assoc name buffers))
          (push (cons (copy-sequence name) file-name) buffers))))
    (when buffers
      (nreverse buffers))))

(defvar frog-jump-buffer-current-ignore-buffers nil
  "This is a placeholder variable for the currently active ignore buffer filters.")

(defun frog-jump-buffer-match (buffers)
  "Process the variable `frog-jump-buffer-current-ignore-buffers' filters for all BUFFERS."
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
  "Filter the buffers to show."
  (if frog-jump-buffer-include-virtual-buffers
      (frog-jump-buffer-match (mapcar #'car (frog-jump-buffer-recentf-buffers)))
    (frog-jump-buffer-match (mapcar #'buffer-name (buffer-list)))))

(defvar frog-jump-buffer-target-other-window nil
  "This is a placeholder variable for determining which window to open the chosen buffer in.")

(defun frog-jump-buffer-target-window-action ()
  "Return the `frog-menu' action for which window to target."
  (if frog-jump-buffer-target-other-window
      '(("0" "[same]" frog-jump-buffer-same-window))
    '(("0" "[other]" frog-jump-buffer-other-window))))

(defun frog-jump-buffer-actions ()
  "Determine the list of actions to show in `frog-jump-buffer'’s `frog-menu'."
  (let ((target-window-option (frog-jump-buffer-target-window-action)))
    (append (frog-jump-buffer-filter-actions) target-window-option)))

(defun frog-jump-buffer-find-or-create-recentf-buffer (res)
  "Visit the file in the `recentf' list."
  (find-file-noselect (assoc-default res (frog-jump-buffer-recentf-buffers))))

(defun frog-jump-buffer-find-or-create-buffer (res)
  "Switch to buffer, or if closed, find and create it first."
  (let ((buffer (if frog-jump-buffer-include-virtual-buffers
                    (frog-jump-buffer-find-or-create-recentf-buffer res)
                  res)))
    (if frog-jump-buffer-target-other-window
        (switch-to-buffer-other-window buffer)
      (switch-to-buffer buffer))))

(defun frog-jump-buffer-handle-result (res)
  "Handle the result (RES) of `frog-menu-read' for `frog-jump-buffer'."
  (cond
   ((stringp res)
    (frog-jump-buffer-find-or-create-buffer res))
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
  (-non-nil
   (append frog-jump-buffer-ignore-buffers
           (list frog-jump-buffer-current-filter-function)
           (unless frog-jump-buffer-include-current-buffer
             (list (concat "^" (buffer-name (current-buffer)) "$"))))))

(defun frog-jump-buffer-maybe-iconify-buffer-names (buffer-names)
  "Add an icon before each buffer name when `all-the-icons-ivy' is available."
  (if (and frog-jump-buffer-use-all-the-icons-ivy (featurep 'all-the-icons-ivy))
      (mapcar #'(lambda (buffer-name)
                  (cons
                   (if (buffer-file-name (get-buffer buffer-name))
                       (all-the-icons-ivy-file-transformer buffer-name)
                     (all-the-icons-ivy-buffer-transformer buffer-name))
                   buffer-name))
              buffer-names)
    buffer-names))

;;;###autoload
(defun frog-jump-buffer ()
  "Present a `frog-menu' for jumping to an open buffer.
If FILTER-FUNCTION is present, filter the `buffer-list' with it."
  (interactive)
  (let* ((frog-menu-avy-padding t)
         (frog-menu-grid-column-function (lambda () 1))
         (frog-menu-posframe-parameters frog-jump-buffer-posframe-parameters)
         (frog-jump-buffer-current-filter-function
          (or frog-jump-buffer-current-filter-function frog-jump-buffer-default-filter))
         (frog-jump-buffer-display-option-alist (copy-alist frog-menu-display-option-alist))
         (_ (rplacd (assq 'avy-posframe frog-jump-buffer-display-option-alist) frog-jump-buffer-posframe-handler))
         (frog-menu-display-option-alist frog-jump-buffer-display-option-alist)
         (frog-jump-buffer-include-virtual-buffers
          (eq frog-jump-buffer-current-filter-function 'frog-jump-buffer-filter-recentf))
         (frog-jump-buffer-current-ignore-buffers (frog-jump-buffer-current-ignore-buffers))
         (buffer-names (-take frog-jump-buffer-max-buffers (frog-jump-buffer-buffer-names)))
         (actions (frog-jump-buffer-actions))
         (filter-keys (-map #'string-to-char (-map #'car actions)))
         (frog-menu-avy-keys (-difference frog-menu-avy-keys filter-keys))
         (prompt (frog-jump-buffer-prompt))
         (res (frog-menu-read prompt (frog-jump-buffer-maybe-iconify-buffer-names
                                      (cl-sort buffer-names frog-jump-buffer-sort))
                              actions)))
    (if res
        (frog-jump-buffer-handle-result res)
      (message "No action or candidate selected"))))

;;;###autoload
(defun frog-jump-buffer-other-window ()
  "Launch `frog-jump-buffer' with `other-window' being the default target window."
  (interactive)
  (let ((frog-jump-buffer-target-other-window t))
    (frog-jump-buffer)))

(provide 'frog-jump-buffer)
;;; frog-jump-buffer.el ends here
