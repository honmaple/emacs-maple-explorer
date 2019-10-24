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

(defcustom maple-explorer-file-display-alist '((side . right) (slot . -1))
  "Whether auto update imenu when file save or window change."
  :type '(list)
  :group 'maple-explorer-file)

(defcustom maple-explorer-file-show-updir-line t
  "Whether auto update imenu when file save or window change."
  :type 'boolean
  :group 'maple-explorer-file)

(defcustom maple-explorer-file-name-function 'maple-explorer-file-name
  "Whether auto update imenu when file save or window change."
  :type 'function
  :group 'maple-explorer-file)

(defcustom maple-explorer-file-filter-function 'maple-explorer-file-filter
  "Whether auto update imenu when file save or window change."
  :type 'function
  :group 'maple-explorer-file)

(defvar maple-explorer-file--expand-list nil)

(defun maple-explorer-file-name(file)
  "Format FILE name."
  (string-trim (file-name-nondirectory file)))

(defun maple-explorer-file-info(file)
  "FILE."
  (let ((lists (list :name (funcall maple-explorer-file-name-function file) :value file)))
    (cond ((string= file "..")
           (append lists (list :click 'maple-explorer-file-updir :face 'font-lock-constant-face)))
          ((file-directory-p file)
           (let ((files (append lists (list :click 'maple-explorer-file-opendir :face 'font-lock-constant-face)))
                 (maple-explorer-file-show-updir-line nil)
                 (projectile-mode nil)
                 (default-directory file))
             (when (member file maple-explorer-file--expand-list)
               (setq files (append files (list :children (maple-explorer-file-list t)))))
             files))
          (t
           (append lists (list :click 'maple-explorer-file-openfile))))))

(defun maple-explorer-file-filter(file)
  "Filter FILE."
  (if (string= file "..") maple-explorer-file-show-updir-line (not (string= file "."))))

(defun maple-explorer-file-list(&optional noparent)
  "Get list NOPARENT."
  (let* ((dir   (maple-explorer-file-find-dir))
         (files (cl-loop for f in (directory-files dir)
                         when (funcall maple-explorer-file-filter-function f)
                         collect (maple-explorer-file-info (if (or (string= f ".") (string= f "..")) f (expand-file-name f dir))))))
    (if noparent files (list :name dir :value dir :children files))))

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
  (let* ((point    (or point (point)))
         (info     (get-char-property point 'maple-explorer))
         (value    (plist-get info :value))
         (children (plist-get info :children))
         (inhibit-read-only t))
    (unless info (error "No buffer info found"))
    (unless (file-directory-p value) (error "No dir found"))
    ;; (maple-explorer--with maple-explorer-file-buffer
    (save-excursion
      (if children (progn
                     (plist-put info :children nil)
                     (setq maple-explorer-file--expand-list (delete value maple-explorer-file--expand-list))
                     (delete-region (line-end-position) (maple-explorer--point)))
        (let* ((maple-explorer-file-show-updir-line nil)
               (projectile-mode nil)
               (default-directory value)
               (items  (maple-explorer-file-list t))
               (indent (maple-explorer--indent)))
          (add-to-list 'maple-explorer-file--expand-list value)
          (plist-put info :children items)
          (if (<= (+ (line-end-position) 1) (point-max))
              (forward-line)
            (goto-char (line-end-position))
            (insert "\n"))
          (maple-explorer-insert-text (list :children items) (+ 2 indent)))))))

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
  (add-hook 'maple-explorer-file-finish-hook 'maple-explorer-file--finish))

(provide 'maple-explorer-file)
;;; maple-explorer-file.el ends here
