;;; maple-explorer-buffer.el ---  maple imenu configuration.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/emacs-maple-explorer

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; maple imenu configuration.
;;

;;; Code:
(require 'maple-explorer-core)

(defgroup maple-explorer-buffer nil
  "Display imenu in window side."
  :group 'maple-explorer)

(defcustom maple-explorer-buffer-display-alist '((side . right) (slot . -1))
  "Whether auto update imenu when file save or window change."
  :type '(list)
  :group 'maple-explorer-buffer)

(defcustom maple-explorer-buffer-name-function 'maple-explorer-buffer-name
  "Whether auto update imenu when file save or window change."
  :type 'function
  :group 'maple-explorer-buffer)

(defcustom maple-explorer-buffer-filter-function 'maple-explorer-buffer-filter
  "Whether auto update imenu when file save or window change."
  :type 'function
  :group 'maple-explorer-buffer)

(defcustom maple-explorer-buffer-group-function 'maple-explorer-buffer-group
  "Whether auto update imenu when file save or window change."
  :type 'function
  :group 'maple-explorer-buffer)

(defun maple-explorer-buffer-group(buffer)
  "Group BUFFER."
  (let ((name (buffer-name buffer))
        (project (when (bound-and-true-p projectile-mode) (with-current-buffer buffer (projectile-project-root)))))
    (cond ((string-match "^magit" name) "MAGIT")
          ((string-match "^.*\\*" name) "HELP")
          (project (list "PROJECT" (file-name-nondirectory (directory-file-name project))))
          (t nil))))

(defun maple-explorer-buffer-name(buffer)
  "Format BUFFER name."
  (string-trim (buffer-name buffer)))

(defun maple-explorer-buffer-info(buffer)
  "Plist BUFFER."
  (list :name  (funcall maple-explorer-buffer-name-function buffer)
        :face  'font-lock-keyword-face
        :click 'maple-explorer-buffer-click
        :value buffer))

(defun maple-explorer-buffer-filter(buffer)
  "Filter BUFFER."
  (and (buffer-live-p buffer)
       (not (string= (substring (buffer-name buffer) 0 1) " "))))

(defun maple-explorer-buffer-list()
  "Get list."
  (maple-explorer-list
   (buffer-list)
   'maple-explorer-buffer-info maple-explorer-buffer-filter-function maple-explorer-buffer-group-function))

(defun maple-explorer-buffer-click(&optional point)
  "Open buffer on POINT."
  (interactive)
  (let* ((point (or point (point)))
         (info  (get-char-property point 'maple-explorer)))
    (unless info (error "No buffer info found"))
    (pop-to-buffer (plist-get info :value))))

(maple-explorer-define buffer)

(provide 'maple-explorer-buffer)
;;; maple-explorer-buffer.el ends here
