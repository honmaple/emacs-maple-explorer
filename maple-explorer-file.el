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

(defcustom maple-explorer-file-show-hidden-files nil
  "Whether auto update imenu when file save or window change."
  :type 'list
  :group 'maple-explorer-file)

(defcustom maple-explorer-file-hidden-regexp-list
  '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.o$")
  "*The regexp list matching hidden files."
  :type  '(repeat (choice regexp))
  :group 'maple-explorer-file)

(defface maple-explorer-file-face
  '((t (:inherit maple-explorer-item-face)))
  "Default face for maple-imenu.")

(defface maple-explorer-file-dir-face
  '((t (:inherit maple-explorer-face)))
  "Default item face for maple-imenu.")

(defvar maple-explorer-file--opened-dir-list nil)

(defmacro maple-explorer-file-with(&optional point &rest body)
  "POINT &REST BODY."
  (declare (indent 1))
  `(let* ((info  (get-char-property (or ,point (point)) 'maple-explorer))
          (name  (plist-get info :name))
          (file  (plist-get info :value)))
     (unless info (error "No file found at point")) ,@body))

(defmacro maple-explorer-file-save-excursion(&rest body)
  "POINT &REST BODY."
  (declare (indent 1))
  `(maple-explorer-file-with (point-min)
     (let ((projectile-mode nil) (default-directory (directory-file-name file))) ,@body)))

(defun maple-explorer-file-info(file)
  "FILE."
  (cond ((or (string= file ".") (string= file ".."))
         (list :name file
               :face 'maple-explorer-file-dir-face
               :click 'maple-explorer-file-updir
               :value file))
        ((file-directory-p file)
         (list :name (file-name-nondirectory (directory-file-name file))
               :face 'maple-explorer-file-dir-face
               :value file
               :click 'maple-explorer-file-opendir
               :children (lambda() (let ((maple-explorer-file-show-updir-line nil)
                                         (projectile-mode nil)
                                         (default-directory file))
                                     (maple-explorer-file-list)))
               :status  (if (member file maple-explorer-file--opened-dir-list) 'open 'close)))
        (t (list :name (file-name-nondirectory file)
                 :face 'maple-explorer-file-face
                 :value file
                 :click 'maple-explorer-file-openfile))))

(defun maple-explorer-file-filter(file)
  "Filter FILE."
  (cond ((string= file ".") nil)
        ((string= file "..") maple-explorer-file-show-updir-line)
        (maple-explorer-file-show-hidden-files t)
        (t (not (cl-loop for i in maple-explorer-file-hidden-regexp-list
                         when (string-match-p i file) return t)))))

(defun maple-explorer-file-list(&optional isroot)
  "Get list ISROOT."
  (let* ((dir   (maple-explorer-file-find-dir))
         (files (cl-loop for f in (directory-files dir)
                         when (funcall maple-explorer-file-filter-function f)
                         collect (maple-explorer-file-info (if (or (string= f ".") (string= f "..")) f (expand-file-name f dir))))))
    (if isroot
        (list :isroot t
              :name dir
              :face 'maple-explorer-file-dir-face
              :value dir
              :children files)
      files)))

(defun maple-explorer-file-find-opened-dir(dir)
  "Find and set opended DIR when root dir is different."
  (let ((d (directory-file-name default-directory)))
    (unless (string= d (directory-file-name dir))
      (add-to-list 'maple-explorer-file--opened-dir-list d)
      (let ((default-directory (file-name-directory d)))
        (maple-explorer-file-find-opened-dir dir)))))

(defun maple-explorer-file-find-dir()
  "Find dir."
  (let ((dir (if (and (bound-and-true-p projectile-mode) (projectile-project-p))
                 (projectile-project-root) default-directory)))
    (maple-explorer-file-find-opened-dir dir) dir))

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
          (setq maple-explorer-file--opened-dir-list (delete value maple-explorer-file--opened-dir-list))
          (maple-explorer-fold-off point))
      (add-to-list 'maple-explorer-file--opened-dir-list value)
      (maple-explorer-fold-on point))))

(defun maple-explorer-file-updir(&optional point)
  "POINT."
  (interactive)
  (maple-explorer-file-with (point-min)
    (let ((projectile-mode nil)
          (default-directory (file-name-directory (directory-file-name file))))
      (add-to-list 'maple-explorer-file--opened-dir-list (directory-file-name file))
      (maple-explorer-file-refresh))))

(defun maple-explorer-file-rename(&optional point)
  "POINT."
  (interactive)
  (maple-explorer-file-with point
    (let ((new-name (read-file-name (format "Rename [%s] to: " name) (file-name-directory file))))
      (if (get-buffer new-name)
          (error "A buffer named '%s' already exists!" new-name)
        (rename-file file new-name 1))
      (maple-explorer-file-refresh)
      (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name)))))

(defun maple-explorer-file-copy(&optional point)
  "POINT."
  (interactive)
  (maple-explorer-file-with point
    (let ((new-name (read-file-name (format "Copy [%s] to: " name) (file-name-directory file))))
      (if (file-directory-p file)
          (copy-directory file new-name)
        (copy-file file new-name))
      (maple-explorer-file-refresh)
      (message "File '%s' successfully copy to '%s'" name (file-name-nondirectory new-name)))))

(defun maple-explorer-file-root(&optional point)
  "POINT."
  (interactive)
  (maple-explorer-file-with point
    (let ((projectile-mode nil)
          (default-directory (directory-file-name file)))
      (maple-explorer-file-refresh))))

(defun maple-explorer-file-omit(&optional point)
  "Hidden POINT."
  (interactive)
  (maple-explorer-file-save-excursion
      (setq maple-explorer-file-show-hidden-files (not maple-explorer-file-show-hidden-files))
    (maple-explorer-file-refresh)))

(defun maple-explorer-file--finish()
  "Run when close."
  (setq maple-explorer-file--opened-dir-list nil))

(maple-explorer-define file
  (setq maple-explorer-file-filter-function 'maple-explorer-file-filter)
  (let ((map maple-explorer-file-mode-map))
    (define-key map (kbd "R") 'maple-explorer-file-rename)
    (define-key map (kbd "C") 'maple-explorer-file-root)
    (define-key map (kbd "c") 'maple-explorer-file-copy)
    (define-key map (kbd "H") 'maple-explorer-file-omit))
  (add-hook 'maple-explorer-file-finish-hook 'maple-explorer-file--finish))

(provide 'maple-explorer-file)
;;; maple-explorer-file.el ends here
