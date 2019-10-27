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

(defface maple-explorer-project-face
  '((t (:inherit maple-explorer-face)))
  "Default face for maple-imenu.")

(defface maple-explorer-project-item-face
  '((t (:inherit maple-explorer-item-face)))
  "Default item face for maple-imenu.")

(defun maple-explorer-project-info(project)
  "PROJECT."
  (list :name project
        :face 'maple-explorer-project-face
        :click 'maple-explorer-fold
        :children
        (let ((default-directory project)
              (maple-explorer-file-show-updir-line nil))
          (maple-explorer-file-list t))
        :value project))

(defun maple-explorer-project-list()
  "Get list."
  (when (bound-and-true-p projectile-mode)
    (maple-explorer-list
     (projectile-open-projects)
     'maple-explorer-project-face
     'maple-explorer-project-info
     maple-explorer-project-filter-function
     maple-explorer-project-group-function)))

(defun maple-explorer-project-click(&optional point)
  "Open buffer on POINT."
  (interactive)
  (let* ((point (or point (point)))
         (info  (get-char-property point 'maple-explorer)))
    (unless info (error "No buffer info found"))
    (pop-to-buffer (plist-get info :value))))

(maple-explorer-define project
  (setq maple-explorer-project-name-function maple-explorer-file-name-function))

(provide 'maple-explorer-project)
;;; maple-explorer-project.el ends here
