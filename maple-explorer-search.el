;;; maple-explorer-search.el ---  maple imenu configuration.	-*- lexical-binding: t -*-

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
;; maple explorer search configuration.
;;

;;; Code:
(require 'maple-explorer-core)

(defgroup maple-explorer-search nil
  "Display search results list in window side."
  :group 'maple-explorer)

(defcustom maple-explorer-search-command "ag %s"
  "Search base command."
  :type 'string
  :group 'maple-explorer-search)

(defface maple-explorer-search-face
  '((t (:inherit maple-explorer-face)))
  "Default face for search results.")

(defface maple-explorer-search-item-face
  '((t (:inherit maple-explorer-item-face)))
  "Default item face for search results.")

(defface maple-explorer-search-highlight-face
  '((t (:inherit maple-explorer-mark-face)))
  "Default highlight face for search results.")

(defvar maple-explorer-search-keyword nil)

(defun maple-explorer-search-group(result)
  "Group search RESULT."
  (car (split-string result ":")))

(defun maple-explorer-search-info(result)
  "Set search RESULT info."
  (let* ((strs (split-string result ":"))
         (file (car strs))
         (line (cadr strs)))
    (list :name (string-join (cdr strs) ":")
          :face  'maple-explorer-search-item-face
          :click 'maple-explorer-search-click
          :value (cons file (string-to-number line)))))

(defun maple-explorer-search-filter(result)
  "Filter search RESULT."
  (not (string= (string-trim result) "")))

(defun maple-explorer-search-list(&optional isroot)
  "Get ISROOT."
  (unless maple-explorer-search-keyword
    (error "Search keyword is nil"))
  (maple-explorer-list
   (split-string (shell-command-to-string (format maple-explorer-search-command maple-explorer-search-keyword)) "\n")
   'maple-explorer-search-face
   'maple-explorer-search-info maple-explorer-search-filter-function maple-explorer-search-group-function))

(defun maple-explorer-search-click()
  "Open buffer on POINT."
  (interactive)
  (maple-explorer-with
    (let ((value (plist-get info :value)))
      (select-window (get-mru-window))
      (find-file (car value))
      (goto-line (cdr value)))))

(defun maple-explorer-search--finish()
  "Run when close."
  (setq maple-explorer-search-keyword nil))

(maple-explorer-define search
  (setq maple-explorer-search-group-function 'maple-explorer-search-group)
  (setq maple-explorer-search-filter-function 'maple-explorer-search-filter)
  (setq maple-explorer-search-display-alist '((side . left) (slot . -1)))
  (add-hook 'maple-explorer-search-finish-hook 'maple-explorer-search--finish))

(defun maple-explorer-search(keyword)
  "Custom search KEYWORD command."
  (interactive (list (read-from-minibuffer "Search: ")))
  (if (< (length keyword) 3)
      (message "keyword's length must > 2")
    (setq maple-explorer-search-keyword keyword)
    (if (maple-explorer-search-window)
        (maple-explorer-search-refresh)
      (maple-explorer-search-show))))

(provide 'maple-explorer-search)
;;; maple-explorer-search.el ends here
