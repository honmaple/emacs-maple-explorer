;;; maple-explorer-imenu.el ---  maple imenu configuration.	-*- lexical-binding: t -*-

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
;; maple explorer imenu configuration.
;;

;;; Code:
(require 'maple-explorer-core)

(defgroup maple-explorer-imenu nil
  "Display imenu in window side."
  :group 'maple-explorer)

(defcustom maple-explorer-imenu-autoupdate t
  "Whether auto update imenu when file save or window change."
  :type 'boolean
  :group 'maple-explorer-imenu)

(defcustom maple-explorer-imenu-modes '(prog-mode org-mode)
  "The modes that be allowed to display `."
  :type 'list
  :group 'maple-explorer-imenu)

(defface maple-explorer-imenu-face
  '((t (:inherit maple-explorer-face)))
  "Default face for maple-imenu.")

(defface maple-explorer-imenu-item-face
  '((t (:inherit maple-explorer-item-face)))
  "Default item face for maple-imenu.")

(defun maple-explorer-imenu-info(item)
  "Plist ITEM."
  (let ((name (car item))
        (value (cdr item)))
    (append (list :name name :isimenu t)
            (if (listp value)
                (list :face 'maple-explorer-imenu-face
                      :click 'maple-explorer-fold
                      :value name
                      :children (mapcar 'maple-explorer-imenu-info value))
              (list :face 'maple-explorer-imenu-item-face
                    :click 'maple-explorer-imenu-click
                    :value value)))))

(defun maple-explorer-imenu--items (items)
  "Categorize all the functions of imenu with ITEMS."
  (if-let ((fns (cl-remove-if #'listp items :key #'cdr)))
      (nconc (cl-remove-if #'nlistp items :key #'cdr)
             `(("Functions" ,@fns)))
    items))

(defun maple-explorer-imenu-list(&optional isroot)
  "List ISROOT."
  (with-selected-window (get-mru-window)
    (unless (featurep 'imenu) (require 'imenu))
    (let* ((imenu-max-item-length "Unlimited")
           (imenu-auto-rescan t)
           (imenu-auto-rescan-maxout (buffer-size))
           (items (or (ignore-errors (imenu--make-index-alist t)) (list))))
      (maple-explorer-list
       (maple-explorer-imenu--items
        (delete (assoc "*Rescan*" items) items))
       'maple-explorer-imenu-face
       'maple-explorer-imenu-info))))

(defun maple-explorer-imenu-click(&optional point)
  "Open buffer on POINT."
  (interactive)
  (let* ((point (or point (point)))
         (info  (get-char-property point 'maple-explorer))
         (value (plist-get info :value)))
    (unless info (error "No buffer info found"))
    (unless (markerp value) (error "No marker info found"))
    (pop-to-buffer (marker-buffer value))
    (goto-char (marker-position value))))

(defun maple-explorer-imenu--refresh()
  "Auto refresh imenu."
  (interactive)
  (when (and (maple-explorer-imenu-window)
             (apply 'derived-mode-p maple-explorer-imenu-modes))
    (maple-explorer-imenu-refresh)))

(defun maple-explorer-imenu--finish()
  "Run when close."
  (remove-hook 'after-save-hook 'maple-explorer-imenu--refresh)
  (remove-hook 'window-configuration-change-hook 'maple-explorer-imenu--refresh))

(defun maple-explorer-imenu--mode()
  "Run when mode enable."
  (when maple-explorer-imenu-autoupdate
    (add-hook 'after-save-hook 'maple-explorer-imenu--refresh)
    (add-hook 'window-configuration-change-hook 'maple-explorer-imenu--refresh)))

(maple-explorer-define imenu
  (add-hook 'maple-explorer-imenu-mode-hook 'maple-explorer-imenu--mode)
  (add-hook 'maple-explorer-imenu-finish-hook 'maple-explorer-imenu--finish))

(provide 'maple-explorer-imenu)
;;; maple-explorer-imenu.el ends here
