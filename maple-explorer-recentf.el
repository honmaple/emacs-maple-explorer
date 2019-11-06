;;; maple-explorer-recentf.el ---  maple imenu configuration.	-*- lexical-binding: t -*-

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
;; maple explorer recentf configuration.
;;

;;; Code:
(require 'maple-explorer-core)

(defgroup maple-explorer-recentf nil
  "Display recentf files in window side."
  :group 'maple-explorer)

(defcustom maple-explorer-recentf-number 49
  "The limit number of recentf files."
  :type 'integer
  :group 'maple-explorer-recentf)

(defface maple-explorer-recentf-face
  '((t (:inherit maple-explorer-face)))
  "Default face for maple-explorer-recentf.")

(defface maple-explorer-recentf-item-face
  '((t (:inherit maple-explorer-item-face)))
  "Default item face for maple-explorer-recentf.")

(defun maple-explorer-recentf-group(file)
  "Group FILE."
  (let* ((default-directory (file-name-directory file))
         (project (when (bound-and-true-p projectile-mode) (projectile-project-root))))
    (when project (file-name-nondirectory (directory-file-name project)))))

(defun maple-explorer-recentf-info(file)
  "Plist FILE."
  (list :name (file-name-nondirectory file)
        :face  'maple-explorer-recentf-item-face
        :click 'maple-explorer-recentf-click
        :value file))

(defun maple-explorer-recentf-subseq(files)
  "Subseq FILES."
  (cl-subseq files 0 (min (length files) maple-explorer-recentf-number)))

(defun maple-explorer-recentf-list(&optional isroot)
  "Get list ISROOT."
  (maple-explorer-list
   (maple-explorer-recentf-subseq
    (progn (unless recentf-mode (recentf-mode)) recentf-list))
   'maple-explorer-recentf-face
   'maple-explorer-recentf-info maple-explorer-recentf-filter-function maple-explorer-recentf-group-function))

(defun maple-explorer-recentf-click(&optional point)
  "Open buffer on POINT."
  (interactive)
  (let* ((point (or point (point)))
         (info  (get-char-property point 'maple-explorer)))
    (unless info (error "No buffer info found"))
    (find-file-other-window (plist-get info :value))))

(maple-explorer-define recentf
  (setq maple-explorer-recentf-group-function 'maple-explorer-recentf-group))

(provide 'maple-explorer-recentf)
;;; maple-explorer-recentf.el ends here
