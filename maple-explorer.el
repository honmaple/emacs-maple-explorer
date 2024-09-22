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
(require 'maple-explorer-file)
(require 'maple-explorer-imenu)
(require 'maple-explorer-buffer)
(require 'maple-explorer-recentf)
(require 'maple-explorer-project)
(require 'maple-explorer-search)

(declare-function nerd-icons-faicon 'nerd-icons)
(declare-function nerd-icons-codicon 'nerd-icons)
(declare-function nerd-icons-icon-for-dir 'nerd-icons)
(declare-function nerd-icons-icon-for-file 'nerd-icons)
(declare-function nerd-icons-icon-for-buffer 'nerd-icons)

(declare-function all-the-icons-faicon 'all-the-icons)
(declare-function all-the-icons-icon-for-dir 'all-the-icons)
(declare-function all-the-icons-icon-for-file 'all-the-icons)
(declare-function all-the-icons-icon-for-buffer 'all-the-icons)

(defcustom maple-explorer-icon 'nerd-icons
  "Show icon with special font."
  :type '(choice (symbol nerd-icons)
                 (symbol all-the-icons))
  :group 'maple-explorer)

(defvar maple-explorer-icon--nerd-icons-alist
  '((file . maple-explorer-icon--nerd-icons-file)
    (imenu . maple-explorer-icon--nerd-icons-imenu)
    (buffer . maple-explorer-icon--nerd-icons-buffer)
    (folder . maple-explorer-icon--nerd-icons-folder)))

(defvar maple-explorer-icon--all-the-icons-alist
  '((file . maple-explorer-icon--all-the-icons-file)
    (imenu . maple-explorer-icon--all-the-icons-imenu)
    (buffer . maple-explorer-icon--all-the-icons-buffer)
    (folder . maple-explorer-icon--all-the-icons-folder)))

(defun maple-explorer-icon--nerd-icons-file (info)
  "Use `nerd-icons` get file icon by INFO."
  (nerd-icons-icon-for-file (plist-get info :value)))

(defun maple-explorer-icon--nerd-icons-imenu (info)
  "Use `nerd-icons` get imenu icon by INFO."
  (let ((cand (plist-get info :name)))
    (cond ((string-match-p "\\(Variables?\\)\\|\\(Fields?\\)" cand)
           (nerd-icons-codicon "nf-cod-symbol_variable"))
          ((string-match-p "\\(Class\\)\\|\\(Structs?\\)" cand)
           (nerd-icons-codicon "nf-cod-symbol_class"))
          ((string-match-p "\\(Functions?\\)\\|\\(Methods?\\)" cand)
           (nerd-icons-codicon "nf-cod-symbol_method"))
          ((string-match-p "\\(Constants?\\)" cand)
           (nerd-icons-codicon "nf-cod-symbol_constant"))
          (t (nerd-icons-codicon "nf-cod-symbol_field")))))

(defun maple-explorer-icon--nerd-icons-folder (info)
  "Use `nerd-icons` get folder icon by INFO."
  (if (maple-explorer--is-open info)
      (nerd-icons-faicon "nf-fa-folder_open")
    (nerd-icons-icon-for-dir (plist-get info :value))))

(defun maple-explorer-icon--nerd-icons-buffer (info)
  "Use `nerd-icons` get buffer icon by INFO."
  (if (plist-get info :children)
      (if (maple-explorer--is-open info)
          (nerd-icons-faicon "nf-fa-folder_open")
        (nerd-icons-faicon "nf-fa-folder"))
    (with-current-buffer (plist-get info :value)
      (nerd-icons-icon-for-buffer))))

(defun maple-explorer-icon--all-the-icons-file (info)
  "Use `all-the-icons` get file icon by INFO."
  (all-the-icons-icon-for-file (plist-get info :value)))

(defun maple-explorer-icon--all-the-icons-imenu (info)
  "Use `all-the-icons` get imenu icon by INFO."
  (let ((cand (plist-get info :name)))
    (cond ((string-match-p "\\(Variables?\\)\\|\\(Fields?\\)" cand)
           (all-the-icons-faicon "tag"))
          ((string-match-p "\\(Class\\)\\|\\(Structs?\\)" cand)
           (all-the-icons-faicon "cogs"))
          ((string-match-p "\\(Functions?\\)\\|\\(Methods?\\)" cand)
           (all-the-icons-faicon "cube"))
          ((string-match-p "\\(Constants?\\)" cand)
           (all-the-icons-faicon "bars"))
          (t (all-the-icons-faicon "code")))))

(defun maple-explorer-icon--all-the-icons-folder (info)
  "Use `all-the-icons` get folder icon by INFO."
  (if (maple-explorer--is-open info)
      (all-the-icons-faicon "folder-open")
    (all-the-icons-icon-for-dir (plist-get info :value))))

(defun maple-explorer-icon--all-the-icons-buffer (info)
  "Use `all-the-icons` get buffer icon by INFO."
  (if (plist-get info :children)
      (if (maple-explorer--is-open info)
          (all-the-icons-faicon "folder-open")
        (all-the-icons-faicon "folder"))
    (with-current-buffer (plist-get info :value)
      (all-the-icons-icon-for-buffer))))

(defun maple-explorer-icon--format (str sym info)
  "Format STR with icon by SYM and INFO."
  (let* ((alist (pcase maple-explorer-icon
                  ('nerd-icons maple-explorer-icon--nerd-icons-alist)
                  ('all-the-icons maple-explorer-icon--all-the-icons-alist)))
         (icon (funcall (cdr (assq sym alist)) info)))
    (format "%s %s" (propertize "\t" 'display icon) str)))

(defun maple-explorer-icon-file-name(info)
  "Custom `maple-explorer-file` INFO icon name."
  (let ((name  (plist-get info :name))
        (value (plist-get info :value)))
    (plist-put info :indent 5)
    (cond ((or (string= name ".") (string= name ".."))
           (maple-explorer-icon--format name 'folder info))
          ((file-directory-p value)
           (maple-explorer-icon--format name 'folder info))
          (t (maple-explorer-icon--format name 'file info)))))

(defun maple-explorer-icon-buffer-name(info)
  "Custom `maple-explorer-buffer` INFO icon name."
  (let ((name (plist-get info :name)))
    (plist-put info :indent 5)
    (maple-explorer-icon--format name 'buffer info)))

(defun maple-explorer-icon-imenu-name(info)
  "Custom `maple-explorer-imenu` INFO icon name."
  (let ((name (plist-get info :name)))
    (plist-put info :indent 5)
    (maple-explorer-icon--format name 'imenu info)))

(defun maple-explorer-icon-recentf-name(info)
  "Custom `maple-explorer-recentf` INFO icon name."
  (let ((name (plist-get info :name))
        (value (plist-get info :value)))
    (plist-put info :indent 5)
    (if (plist-get info :children)
        (maple-explorer-icon--format name 'folder info)
      (maple-explorer-icon--format (file-name-nondirectory value) 'file info))))

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
