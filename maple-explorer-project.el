;;; maple-explorer-project.el ---  maple imenu configuration.	-*- lexical-binding: t -*-

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
(require 'maple-explorer-file)

(defgroup maple-explorer-project nil
  "Display imenu in window side."
  :group 'maple-explorer)

(defcustom maple-explorer-project-display-alist '((side . right) (slot . -1))
  "Whether auto update imenu when file save or window change."
  :type '(list)
  :group 'maple-explorer-project)

(defcustom maple-explorer-project-name-function 'maple-explorer-project-name
  "Whether auto update imenu when file save or window change."
  :type 'function
  :group 'maple-explorer-project)

(defcustom maple-explorer-project-filter-function 'maple-explorer-project-filter
  "Whether auto update imenu when file save or window change."
  :type 'function
  :group 'maple-explorer-project)

(defun maple-explorer-project-name(project)
  "PROJECT."
  (string-trim project))

(defun maple-explorer-project-info(project)
  "PROJECT."
  (list :name (maple-explorer-project-name project)
        :face 'font-lock-constant-face
        :children
        (let ((default-directory project)
              (maple-explorer-file-show-updir-line nil))
          (maple-explorer-file-list t))))

(defun maple-explorer-project-list()
  "Get list."
  (maple-explorer-list
   (when (bound-and-true-p projectile-mode) (projectile-open-projects))
   'maple-explorer-project-info))

(defun maple-explorer-project-click(&optional point)
  "Open buffer on POINT."
  (interactive)
  (let* ((point (or point (point)))
         (info  (get-char-property point 'maple-explorer)))
    (unless info (error "No buffer info found"))
    (pop-to-buffer (plist-get info :value))))

(maple-explorer-define project)

(provide 'maple-explorer-project)
;;; maple-explorer-project.el ends here
