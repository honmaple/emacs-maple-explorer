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
;; maple explorer file configuration.
;;

;;; Code:

(require 'maple-explorer-core)

(defgroup maple-explorer-file nil
  "Display files in window side."
  :group 'maple-explorer)

(defcustom maple-explorer-file-open-action nil
  "How to open selected file."
  :type '(choice (const vertical)
                 (const horizontal))
  :group 'maple-explorer-file)

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

(defmacro maple-explorer-file-with(&rest body)
  "POINT &REST BODY."
  (declare (indent defun))
  `(maple-explorer-with
     (let ((name  (plist-get info :name))
           (file  (plist-get info :value)))
       ,@body)))

(defun maple-explorer-file--dir(dir)
  "Get true DIR name."
  (directory-file-name (file-truename (substitute-in-file-name dir))))

(defun maple-explorer-file-info(file)
  "FILE."
  (cond ((string= file ".")
         (list :name file
               :face 'maple-explorer-file-dir-face
               :click 'maple-explorer-file-refresh
               :value file))
        ((string= file "..")
         (list :name file
               :face 'maple-explorer-file-dir-face
               :click 'maple-explorer-file-updir
               :value file))
        ((file-directory-p file)
         (list :name (file-name-nondirectory (maple-explorer-file--dir file))
               :face 'maple-explorer-file-dir-face
               :value file
               :click 'maple-explorer-fold
               :children (lambda() (let ((maple-explorer-file-show-updir-line nil)
                                         (projectile-mode nil)
                                         (default-directory file))
                                     (maple-explorer-file-list)))
               :status 'close))
        (t (list :name (file-name-nondirectory file)
                 :face 'maple-explorer-file-face
                 :value file
                 :click 'maple-explorer-file-open))))

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
              :click 'maple-explorer-fold
              :value dir
              :children files)
      files)))

(defun maple-explorer-file-find-opened-dir(dir)
  "Find and set opended DIR when root dir is different."
  (let ((d (maple-explorer-file--dir default-directory)))
    (unless (string= d (maple-explorer-file--dir dir))
      (add-to-list 'maple-explorer-opened-list d)
      (let ((default-directory (file-name-directory d)))
        (maple-explorer-file-find-opened-dir dir)))))

(defun maple-explorer-file-find-dir()
  "Find root dir."
  (let ((dir (if (and (bound-and-true-p projectile-mode) (projectile-project-p))
                 (projectile-project-root) default-directory)))
    (maple-explorer-file-find-opened-dir dir) dir))

(defun maple-explorer-file-open()
  "Open file at point."
  (interactive)
  (maple-explorer-file-with
    (select-window (get-mru-window))
    (cond ((eq maple-explorer-file-open-action 'vertical)
           (split-window-right) (windmove-right))
          ((eq maple-explorer-file-open-action 'horizontal)
           (split-window-below) (windmove-down)))
    (find-file file)))

(defun maple-explorer-file-open-vertical-split()
  "Open file at point with vertical split."
  (interactive)
  (let ((maple-explorer-file-open-action 'vertical))
    (call-interactively 'maple-explorer-file-open)))

(defun maple-explorer-file-open-horizontal-split()
  "Open file at point with horizontal split."
  (interactive)
  (let ((maple-explorer-file-open-action 'horizontal))
    (call-interactively 'maple-explorer-file-open)))

(defun maple-explorer-file-rename()
  "Rename file at point."
  (interactive)
  (maple-explorer-file-with
    (let ((new-name (read-file-name (format "Rename [%s] to: " name) (file-name-directory file))))
      (if (get-buffer new-name)
          (error "A buffer named '%s' already exists!" new-name)
        (rename-file file new-name 1))
      (maple-explorer-file-refresh)
      (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name)))))

(defun maple-explorer-file-copy()
  "Copy file at point."
  (interactive)
  (maple-explorer-file-with
    (let ((new-name (read-file-name (format "Copy [%s] to: " name) (file-name-directory file))))
      (if (file-directory-p file)
          (copy-directory file new-name)
        (copy-file file new-name))
      (maple-explorer-file-refresh)
      (message "File '%s' successfully copy to '%s'" name (file-name-nondirectory new-name)))))

(defun maple-explorer-file-create()
  "Make new directory."
  (interactive)
  (let* ((file  (read-file-name "Create file: "))
         (isdir (string-suffix-p "/" file))
         (typ   (if isdir "directory" "file")))
    (if (file-exists-p file)
        (error "Cannot create %s %s: file exists" typ file)
      (if isdir (make-directory file t) (with-temp-buffer (write-file file))))
    (maple-explorer-file-refresh)
    (message "Create %s '%s' successfully" typ file)))

(defun maple-explorer-file-remove()
  "Remove file or directory at point."
  (interactive)
  (maple-explorer-file-with
    (let ((isdir (file-directory-p file)))
      (when (yes-or-no-p (format "Do you really want to delete [%s]?" file))
        (if isdir (delete-directory file t) (delete-file file))
        (maple-explorer-file-refresh)))))

(defun maple-explorer-file-switch-root()
  "Switch to current buffer root dir."
  (interactive)
  (setq default-directory (with-selected-window (get-mru-window) (maple-explorer-file-find-dir)))
  (maple-explorer-file-refresh))

(defun maple-explorer-file-root()
  "Change root to dir at point."
  (interactive)
  (maple-explorer-file-with
    (setq default-directory (maple-explorer-file--dir file))
    (setq maple-explorer-closed-list (delete default-directory maple-explorer-closed-list))
    (maple-explorer-file-refresh)))

(defun maple-explorer-file-updir()
  "Change root to updir."
  (interactive)
  (let ((dir (maple-explorer-file--dir default-directory)))
    (setq default-directory (file-name-directory dir))
    (add-to-list 'maple-explorer-opened-list dir)
    (maple-explorer-file-refresh)))

(defun maple-explorer-file-right-menu()
  "Explorer file right menu."
  (easy-menu-create-menu
   'maple-explorer-file-right-menu
   (list (vector "New" 'maple-explorer-file-create)
         ["--" #'ignore t]
         (vector "Open" 'maple-explorer-file-open)
         (list "Open With"
               (vector "Open With MRU" 'maple-explorer-file-open-vertical-split)
               (vector "Open With Vertical" 'maple-explorer-file-open-vertical-split)
               (vector "Open With Horizontal" 'maple-explorer-file-open-horizontal-split))
         ["--" #'ignore t]
         (vector "Mark" 'maple-explorer-mark-or-unmark)
         (vector "Copy" 'maple-explorer-file-copy)
         (vector "Rename" 'maple-explorer-file-rename)
         (vector "Delete" 'maple-explorer-file-remove)
         (vector "Refresh" 'maple-explorer-file-refresh))))

(defun maple-explorer-file-omit()
  "Toggle hide or show hidden files."
  (interactive)
  (setq maple-explorer-file-show-hidden-files (not maple-explorer-file-show-hidden-files))
  (maple-explorer-file-refresh))

(defun maple-explorer-file--refresh(func &optional first)
  "Around FUNC file refresh FIRST."
  (let ((projectile-mode (when (bound-and-true-p projectile-mode) first)))
    (funcall func first)))

(maple-explorer-define file
  (setq maple-explorer-file-filter-function 'maple-explorer-file-filter
        maple-explorer-file-right-menu-function 'maple-explorer-file-right-menu)
  (let ((map maple-explorer-file-mode-map))
    (define-key map (kbd "m") 'maple-explorer-mark-or-unmark)
    (define-key map (kbd "u") 'maple-explorer-mark-or-unmark)
    (define-key map (kbd "U") 'maple-explorer-unmark-all)
    (define-key map [mouse-3] 'maple-explorer-file-right-click)
    (define-key map (kbd "R") 'maple-explorer-file-rename)
    (define-key map (kbd "C") 'maple-explorer-file-copy)
    (define-key map (kbd "D") 'maple-explorer-file-remove)
    (define-key map (kbd "+") 'maple-explorer-file-create)
    (define-key map (kbd "^") 'maple-explorer-file-updir)
    (define-key map (kbd "f") 'maple-explorer-file-root)
    (define-key map (kbd "F") 'maple-explorer-file-switch-root)
    (define-key map (kbd "H") 'maple-explorer-file-omit))
  (advice-add 'maple-explorer-file-refresh :around 'maple-explorer-file--refresh))

(provide 'maple-explorer-file)
;;; maple-explorer-file.el ends here
