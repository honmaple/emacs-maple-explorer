;;; maple-explorer-core.el ---  maple imenu configuration.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/emacs-maple-imenu

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
(defgroup maple-explorer nil
  "Display imenu in window side."
  :group 'maple)

(defun maple-explorer--keymap (action)
  "Make keymap with ACTION."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]
      `(lambda (_)
         (interactive "e")
         (call-interactively (or (command-remapping ,action) ,action))))
    (define-key map (kbd "RET") action
      )
    ;; `(lambda ()
    ;;    (interactive)
    ;;    (call-interactively (or (command-remapping ,action) ,action))))
    map))

(defun maple-explorer-table-merge(key value table)
  "Set KEY VALUE TABLE."
  (unless (listp key) (setq key (list key)))
  (let* ((x (car key))
         (v (or (gethash x table) (list))))
    (cl-loop for n in (reverse (cdr key))
             do (setq value (list :name n :children (list value) :click 'maple-explorer-fold)))
    (let* ((k (plist-get value :name))
           (m (cl-loop for n in v when (string= (plist-get n :name) k) return n)))
      (if m (plist-put m :children (append (plist-get m :children) (plist-get value :children)))
        (setq v (append v (list value)))))
    (puthash x v table)))

(defun maple-explorer-list(lists &optional info-function filter-function group-function)
  "LISTS &OPTIONAL INFO-FUNCTION FILTER-FUNCTION GROUP-FUNCTION."
  (let ((table   (make-hash-table :test 'equal)) children)
    (dolist (child lists)
      (when (or (not filter-function) (funcall filter-function child))
        (let ((group (when group-function (funcall group-function child)))
              (info  (if info-function (funcall info-function child) child)))
          (if group (maple-explorer-table-merge group info table) (push info children)))))
    (maphash
     (lambda(key value)
       (setq children (append children (list (list :name key :children value :click 'maple-explorer-fold)))))
     table)
    (list :children children)))

(defun maple-explorer--text(info &optional indent)
  "Get button with BUFFER ACTION &OPTIONAL FACE."
  (let* ((name     (plist-get info :name))
         (face     (plist-get info :face))
         (click    (plist-get info :click)))
    (propertize
     (format "%s%s" (make-string (or indent 0) ?\s) name)
     'face (or face 'font-lock-variable-name-face)
     'mouse-face 'highlight
     'maple-explorer info
     'keymap (maple-explorer--keymap click))))

(defun maple-explorer-insert-text(info &optional indent)
  "Insert INFO &OPTIONAL INDENT."
  (let ((name     (plist-get info :name))
        (children (plist-get info :children))
        (indent   (or indent 0)))
    (when (and name (not (string= name "")))
      (insert (maple-explorer--text info indent) "\n")
      (setq indent (+ indent 2)))
    (when (functionp children)
      (setq children (funcall children)))
    (when children
      (dolist (child children) (maple-explorer-insert-text child indent)))))

(defun maple-explorer-display-buffer (buffer _alist)
  "Display BUFFER _ALIST."
  (display-buffer-in-side-window buffer '((side . right) (slot . -1))))

(defmacro maple-explorer-with-click(&rest body)
  "Execute the forms in BODY with buffer."
  (declare (indent defun))
  `(let ((info (get-pos-property (point) 'maple-explorer))) ,@body))

(defmacro maple-explorer--with-buffer (name &rest body)
  "Execute the forms in BODY with buffer."
  (declare (indent 1) (doc-string 2))
  `(let ((buffer (get-buffer-create ,name)))
     (with-current-buffer buffer ,@body)))

(defmacro maple-explorer--with-window (name &rest body)
  "Execute the forms in BODY with window."
  (declare (indent 1) (doc-string 2))
  `(let ((window (get-buffer-window ,name t)))
     (when window (with-selected-window window ,@body))))

(defmacro maple-explorer--with (name &rest body)
  "Execute the forms in BODY with window."
  (declare (indent 1) (doc-string 2))
  `(maple-explorer--with-buffer ,name
     (let ((inhibit-read-only t))
       (save-excursion ,@body))))

(defun maple-explorer--indent()
  "Get current line level."
  (let ((text (thing-at-point 'line t)))
    (- (string-width text) (string-width (string-trim-left text)))))

(defun maple-explorer--point()
  "Get point."
  (let* ((level (maple-explorer--indent))
         (point (line-end-position))
         stop)
    (save-excursion
      (while (not stop)
        (forward-line 1)
        (if (and (> (maple-explorer--indent) level) (< point (point-max)))
            (setq point (line-end-position)) (setq stop t))))
    point))

(defun maple-explorer-fold-off(&optional point)
  "Open buffer."
  (interactive)
  (let ((overlay (get-char-property (or point (line-end-position)) 'maple-explorer-overlay)))
    (when (or (not overlay) (< (overlay-start overlay) (line-beginning-position)))
      (setq overlay (make-overlay (line-end-position) (maple-explorer--point))))
    (overlay-put overlay 'maple-explorer-overlay overlay)
    (overlay-put overlay 'invisible t)))

(defun maple-explorer-fold-on(&optional point)
  "Open buffer."
  (interactive)
  (let* ((info (get-char-property (line-beginning-position) 'maple-explorer))
         (dynamic (plist-get info :dynamic))
         (overlay (get-char-property (or point (line-end-position)) 'maple-explorer-overlay)))
    (if dynamic
        (let ((children (funcall dynamic))
              (inhibit-read-only t))
          (plist-put info :children children)
          (forward-line)
          (maple-explorer-insert-text children (+ 2 (maple-explorer--indent))))
      (when overlay (overlay-put overlay 'invisible nil)))))

(defun maple-explorer-fold(&optional point)
  "BODY."
  (interactive)
  (let* ((point (or point (line-end-position)))
         (invisible (get-char-property point 'invisible)))
    (if invisible (maple-explorer-fold-on point) (maple-explorer-fold-off point))))

(defmacro maple-explorer-define(name &rest body)
  "NAME &REST BODY."
  (declare (indent 1) (doc-string 2))
  (let* ((prefix (format "maple-explorer-%s" name))
         (togg-function (intern prefix))
         (show-function (intern (format "%s-show" prefix)))
         (hide-function (intern (format "%s-hide" prefix)))
         (mode-function (intern (format "%s-mode" prefix)))
         (list-function (intern (format "%s-list" prefix)))
         (window-function (intern (format "%s-window" prefix)))
         (resize-function (intern (format "%s-window-resize" prefix)))
         (display-function (intern (format "%s-display" prefix)))
         (refresh-function (intern (format "%s-refresh" prefix)))

         (init-hook (intern (format "%s-init-hook" prefix)))
         (finish-hook (intern (format "%s-finish-hook" prefix)))

         (mode-map (intern (format "%s-mode-map" prefix)))
         (auto-resize (intern (format "%s-autoresize" prefix)))
         (buffer-name (intern (format "%s-buffer" prefix)))
         (buffer-width (intern (format "%s-width" prefix)))
         (display-alist (intern (format "%s-display-alist" prefix))))
    `(progn
       (defcustom ,buffer-name ,(format "*%s*" prefix)
         "Buffer name."
         :type 'string
         :group ',togg-function)

       (defcustom ,buffer-width '(15 . 36)
         "Whether auto resize imenu window when item's length is long."
         :type 'cons
         :group ',togg-function)

       (defcustom ,auto-resize t
         "Whether auto resize imenu window when item's length is long."
         :type 'boolean
         :group ',togg-function)

       (defcustom ,init-hook nil
         "Init hook."
         :type 'list
         :group ',togg-function)

       (defcustom ,finish-hook nil
         "Finish hook."
         :type 'list
         :group ',togg-function)

       (defun ,window-function()
         "Get window."
         (get-buffer-window ,buffer-name t))

       (defun ,resize-function()
         "Get window."
         (maple-explorer--with-window ,buffer-name
           (setq window-size-fixed nil)
           (let* ((min-width (max (car ,buffer-width) window-min-width))
                  (max-width (cdr ,buffer-width)))
             (when (and ,auto-resize (not (= min-width max-width)))
               (let ((fit-window-to-buffer-horizontally t)) (fit-window-to-buffer)))
             (if (> (window-width) max-width)
                 (shrink-window-horizontally (- (window-width) max-width))
               (when (< (window-width) min-width)
                 (enlarge-window-horizontally (- min-width (window-width))))))
           (setq window-size-fixed 'width)))

       (defun ,display-function(buffer _alist)
         "Display BUFFER _ALIST."
         (display-buffer-in-side-window buffer ,display-alist))

       (defun ,show-function()
         "Show."
         (interactive)
         (run-hooks ',init-hook)
         (,refresh-function t))

       (defun ,hide-function()
         "Hide."
         (interactive)
         (let ((window (,window-function)))
           (when window (delete-window window)))
         (run-hooks ',finish-hook))

       (defun ,togg-function()
         "Toggle."
         (interactive)
         (if (,window-function) (,hide-function) (,show-function)))

       (defun ,refresh-function(&optional first)
         "Refresh when FIRST enable mode."
         (interactive)
         (let ((items  (,list-function))
               (buffer ,buffer-name))
           (maple-explorer--with buffer
             (erase-buffer)
             (maple-explorer-insert-text items)
             (when first
               (select-window (display-buffer buffer ',display-function))
               (,mode-function)))
           (,resize-function)))

       (defvar ,mode-map
         (let ((map (make-sparse-keymap)))
           (define-key map (kbd "q") ',hide-function)
           (define-key map (kbd "r") ',refresh-function)
           map)
         ,(format "%s-mode-map keymap." prefix))

       (define-derived-mode ,mode-function special-mode ,prefix
         ,(format "%s-mode." prefix)
         (setq indent-tabs-mode nil
               buffer-read-only t
               truncate-lines -1
               cursor-in-non-selected-windows nil)

         (when (bound-and-true-p evil-mode)
           (evil-make-overriding-map maple-explorer-buffer-mode-map 'normal)))

       ,@body)))

(provide 'maple-explorer-core)
;;; maple-explorer-core.el ends here
