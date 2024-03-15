;;; maple-explorer.el ---  maple explorer configuration.	-*- lexical-binding: t -*-

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
;; maple explorer configuration.
;;

;;; Code:
(require 'all-the-icons nil t)
(require 'maple-explorer-file)
(require 'maple-explorer-imenu)
(require 'maple-explorer-buffer)
(require 'maple-explorer-recentf)
(require 'maple-explorer-project)
(require 'maple-explorer-search)

(defun maple-explorer-icon (str icon)
  "The ICON of maple-explorer STR."
  (format "%s %s" (propertize "\t" 'display icon) str))

(defun maple-explorer-icon-file-name(info)
  "Custom maple-explorer-file INFO icon name."
  (let ((name  (plist-get info :name))
        (value (plist-get info :value)))
    (plist-put info :indent 5)
    (cond ((or (string= name ".") (string= name ".."))
           (maple-explorer-icon name (all-the-icons-faicon "folder")))
          ((file-directory-p value)
           (maple-explorer-icon
            name
            (if (maple-explorer--is-open info)
                (all-the-icons-faicon "folder-open")
              (all-the-icons-faicon "folder"))))
          (t (maple-explorer-icon name (all-the-icons-icon-for-file value))))))

(defun maple-explorer-icon-buffer-name(info)
  "Custom maple-explorer-buffer INFO icon name."
  (let ((name (plist-get info :name))
        (value (plist-get info :value)))
    (plist-put info :indent 5)
    (maple-explorer-icon
     name
     (if (plist-get info :children)
         (if (maple-explorer--is-open info) (all-the-icons-faicon "folder-open") (all-the-icons-faicon "folder"))
       (with-current-buffer value
         (let ((icon (all-the-icons-icon-for-buffer)))
           (if (symbolp icon) (all-the-icons-faicon "file-text" :height 0.95 :v-adjust 0.05) icon)))))))

(defun maple-explorer-icon-imenu-name(info)
  "Custom maple-explorer-imenu INFO icon name."
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
  "Custom maple-explorer-recentf INFO icon name."
  (let ((name (plist-get info :name))
        (value (plist-get info :value)))
    (plist-put info :indent 5)
    (if (plist-get info :children)
        (maple-explorer-icon
         name (if (maple-explorer--is-open info) (all-the-icons-faicon "folder-open") (all-the-icons-faicon "folder")))
      (maple-explorer-icon (file-name-nondirectory value) (all-the-icons-icon-for-file value)))))

;;;###autoload
(define-minor-mode maple-explorer-icon-mode
  "Maple explorer icon mode."
  :group      'maple-explorer
  :global     t
  (let ((funcs '((file . maple-explorer-icon-file-name)
                 (imenu . maple-explorer-icon-imenu-name)
                 (buffer . maple-explorer-icon-buffer-name)
                 (recentf . maple-explorer-icon-recentf-name)
                 (project . maple-explorer-icon-file-name))))
    (if maple-explorer-icon-mode
        (dolist (func funcs)
          (add-to-list 'maple-explorer-name-alist func))
      (dolist (func funcs)
        (setq maple-explorer-name-alist (delete func maple-explorer-name-alist))))))

(provide 'maple-explorer)
;;; maple-explorer.el ends here
