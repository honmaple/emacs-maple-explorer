;;; maple-explorer-file.el ---  maple imenu configuration.	-*- lexical-binding: t -*-

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

(defgroup maple-explorer-file nil
  "Display imenu in window side."
  :group 'maple-explorer)

(defcustom maple-explorer-file-show-updir-line t
  "Whether auto update imenu when file save or window change."
  :type 'boolean
  :group 'maple-explorer-file)

(defface maple-explorer-file-face
  '((t (:inherit maple-explorer-item-face)))
  "Default face for maple-imenu.")

(defface maple-explorer-file-dir-face
  '((t (:inherit maple-explorer-face)))
  "Default item face for maple-imenu.")

(defvar maple-explorer-file--expand-list nil)

(defun maple-explorer-file-name(info)
  "Format INFO name."
  (let ((value (plist-get info :value)))
    (cond ((string= value ".") ".")
          ((string= value "..") "..")
          ((file-directory-p value) (file-name-nondirectory (directory-file-name value)))
          (t (file-name-nondirectory value)))))

(defun maple-explorer-file-info(file)
  "FILE."
  (let ((lists (list :name "" :value file)))
    (cond ((string= file "..")
           (append lists (list :click 'maple-explorer-file-updir :face 'maple-explorer-file-dir-face)))
          ((file-directory-p file)
           (append lists
                   (list :click 'maple-explorer-file-opendir
                         :face 'maple-explorer-file-dir-face
                         :children (lambda() (let ((maple-explorer-file-show-updir-line nil)
                                                   (projectile-mode nil)
                                                   (default-directory file))
                                               (maple-explorer-file-list t))))
                   (if (member file maple-explorer-file--expand-list)
                       (list :status 'open) (list :status 'close))))
          (t
           (append lists (list :click 'maple-explorer-file-openfile :face 'maple-explorer-file-face))))))

(defun maple-explorer-file-filter(file)
  "Filter FILE."
  (if (string= file "..") maple-explorer-file-show-updir-line (not (string= file "."))))

(defun maple-explorer-file-list(&optional noparent)
  "Get list NOPARENT."
  (let* ((dir   (maple-explorer-file-find-dir))
         (files (cl-loop for f in (directory-files dir)
                         when (funcall maple-explorer-file-filter-function f)
                         collect (maple-explorer-file-info (if (or (string= f ".") (string= f "..")) f (expand-file-name f dir))))))
    (if noparent files (list :isheader t :name dir :value dir :face 'maple-explorer-file-dir-face :children files))))

(defun maple-explorer-file-find-dir()
  "Find dir."
  (if (and (bound-and-true-p projectile-mode) (projectile-project-p))
      (or (projectile-project-root) default-directory)
    default-directory))

(defun maple-explorer-file-click(&optional point)
  "Open buffer on POINT."
  (interactive)
  (let* ((point (or point (point)))
         (info  (get-char-property point 'maple-explorer))
         (value (plist-get info :value)))
    (unless info (error "No buffer info found"))
    (pop-to-buffer (plist-get info :value))))

(defun maple-explorer-file-openfile(&optional point)
  "POINT."
  (interactive)
  (let* ((point (or point (point)))
         (info  (get-char-property point 'maple-explorer)))
    (unless info (error "No buffer info found"))
    (find-file-other-window (plist-get info :value))))

(defun maple-explorer-file-opendir(&optional point)
  "POINT."
  (interactive)
  (let* ((info (get-char-property (or point (line-beginning-position)) 'maple-explorer))
         (status (plist-get info :status))
         (value (plist-get info :value)))
    (if (maple-explorer--is-open status)
        (progn
          (setq maple-explorer-file--expand-list (delete value maple-explorer-file--expand-list))
          (maple-explorer-fold-off point))
      (add-to-list 'maple-explorer-file--expand-list value)
      (maple-explorer-fold-on point))))

(defun maple-explorer-file-updir(&optional point)
  "POINT."
  (interactive)
  (let* ((info (get-char-property (point-min) 'maple-explorer))
         (value (plist-get info :value))
         (updir (file-name-directory (directory-file-name value)))
         (projectile-mode nil)
         (default-directory updir))
    (add-to-list 'maple-explorer-file--expand-list (directory-file-name value))
    (maple-explorer-file-refresh)))

(defun maple-explorer-file-rename(&optional point)
  "POINT."
  (interactive))

(defun maple-explorer-file-move(&optional point)
  "POINT."
  (interactive))

(defun maple-explorer-file-copy(&optional point)
  "POINT."
  (interactive))

(defun maple-explorer-file--finish()
  "Run when close."
  (setq maple-explorer-file--expand-list nil))

(maple-explorer-define file
  (setq maple-explorer-file-name-function 'maple-explorer-file-name)
  (setq maple-explorer-file-filter-function 'maple-explorer-file-filter)
  (add-hook 'maple-explorer-file-finish-hook 'maple-explorer-file--finish))

(provide 'maple-explorer-file)
;;; maple-explorer-file.el ends here
