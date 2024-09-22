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
;; maple explorer project configuration.
;;

;;; Code:
(require 'maple-explorer-file)

(defgroup maple-explorer-project nil
  "Display opened projects in window side."
  :group 'maple-explorer)

(defface maple-explorer-project-face
  '((t (:inherit maple-explorer-face)))
  "Default face for maple-imenu.")

(defface maple-explorer-project-item-face
  '((t (:inherit maple-explorer-item-face)))
  "Default item face for maple-imenu.")

(defun maple-explorer-project-info(project)
  "PROJECT."
  (let ((name (expand-file-name (directory-file-name project))))
    (list :name name
          :face 'maple-explorer-project-face
          :click 'maple-explorer-fold
          :children (lambda() (let ((maple-explorer-file-show-updir-line nil)
                                    (projectile-mode nil)
                                    (default-directory name))
                                (setq maple-explorer-kind 'file)
                                (maple-explorer-file-list)))
          :status 'close
          :value name)))

(defun maple-explorer-project-list(&optional isroot)
  "Get list ISROOT."
  (when (bound-and-true-p projectile-mode)
    (maple-explorer-list
     (projectile-open-projects)
     'maple-explorer-project-face
     'maple-explorer-project-info)))

(maple-explorer-define project)

(provide 'maple-explorer-project)
;;; maple-explorer-project.el ends here
