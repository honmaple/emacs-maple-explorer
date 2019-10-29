;;; maple-explorer-icon.el ---  maple imenu configuration.	-*- lexical-binding: t -*-

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
(require 'all-the-icons)
(require 'maple-explorer-file)
(require 'maple-explorer-imenu)
(require 'maple-explorer-buffer)
(require 'maple-explorer-recentf)

(defun maple-explorer-icon (str icon)
  "STR ICON."
  (format "%s %s" (propertize "\t" 'display icon) str))

(defun maple-explorer-icon-file-name(info)
  "Format INFO name."
  (let ((name  (plist-get info :name))
        (value (plist-get info :value)))
    (plist-put info :indent 5)
    (cond ((or (string= name ".") (string= name ".."))
           (maple-explorer-icon name (all-the-icons-faicon "folder")))
          ((file-directory-p value)
           (maple-explorer-icon
            name
            (if (maple-explorer--is-open (plist-get info :status))
                (all-the-icons-faicon "folder-open")
              (all-the-icons-faicon "folder"))))
          (t (maple-explorer-icon name (all-the-icons-icon-for-file value))))))

(defun maple-explorer-icon-buffer-name(info)
  "Format INFO name."
  (let ((name (plist-get info :name))
        (value (plist-get info :value)))
    (plist-put info :indent 5)
    (maple-explorer-icon
     name
     (if (plist-get info :children)
         (if (maple-explorer--is-open (plist-get info :status)) (all-the-icons-faicon "folder-open") (all-the-icons-faicon "folder"))
       (with-current-buffer value
         (let ((icon (all-the-icons-icon-for-buffer)))
           (if (symbolp icon) (all-the-icons-faicon "file-text" :height 0.95 :v-adjust 0.05) icon)))))))

(defun maple-explorer-icon-imenu-name(info)
  "Format INFO name."
  (let ((name (plist-get info :name)))
    (plist-put info :indent 5)
    (maple-explorer-icon
     name
     (if (plist-get info :children)
         (cond ((string= name "Variables")
                (all-the-icons-octicon "tag"))
               ((string= name "Class")
                (all-the-icons-material "settings_input_component"))
               (t (all-the-icons-material "filter_center_focus")))
       (all-the-icons-faicon "cube")))))

(defun maple-explorer-icon-recentf-name(info)
  "Format INFO name."
  (let ((name (plist-get info :name))
        (value (plist-get info :value))
        (status (plist-get info :status)))
    (plist-put info :indent 5)
    (if (plist-get info :children)
        (maple-explorer-icon
         name (if (maple-explorer--is-open status) (all-the-icons-faicon "folder-open") (all-the-icons-faicon "folder")))
      (maple-explorer-icon (file-name-nondirectory value) (all-the-icons-icon-for-file value)))))

(setq maple-explorer-file-name-function 'maple-explorer-icon-file-name)
(setq maple-explorer-imenu-name-function 'maple-explorer-icon-imenu-name)
(setq maple-explorer-buffer-name-function 'maple-explorer-icon-buffer-name)
(setq maple-explorer-recentf-name-function 'maple-explorer-icon-recentf-name)

(provide 'maple-explorer-icon)
;;; maple-explorer-icon.el ends here
