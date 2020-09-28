;;; fourclojure.el --- 4Clojure problems at your fingertips! -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Andrew Lai
;;
;; Author: Andrew Lai <http://github/ALai57>
;; Maintainer: Andrew Lai <andrew.s.lai5@gmail.com>
;; Version: 0.0.1
;; Keywords: clojure, 4clojure
;; Homepage: https://github.com/ALai57/fourclojure
;; Package-Requires: ((emacs "27.0.91") (cl-lib "0.5") (dom "1.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; fourclojure.el imports 4clojure problems directly into Emacs.
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Requirements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dom)
(require 'org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq fourclojure-buffer "fourclojure.clj")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun third (x) (nth 2 x))

(defun curl (url)
  (let* ((b (url-retrieve-synchronously url t)))
    (with-current-buffer b
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point) (point-min))
      ;;(buffer-string)
      (libxml-parse-html-region (point-min) (point-max)))))

;;(curl "http://4clojure.com/problems")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Retrieving a single 4c problem and add to clj file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-4clojure-test-cases (full-dom)
  (mapcar (lambda (x) (nth 2 (nth 0 (dom-by-tag x 'pre))))
          (dom-by-tag
           (dom-by-class full-dom "testcases")
           'tr)))

(defun flatten (xs)
  (when xs
    (cond
     ;; remove (symbol . string)
     ((and (listp xs)
           (symbolp (car xs))
           (stringp (cdr xs))) "")
     ;; remove (table ...)
     ((and (listp xs)
           (symbolp (car xs))
           (eq 'table (car xs))) "")
     ((listp xs) (concat (flatten (car xs)) (flatten (cdr xs))))
     ((stringp xs) xs))))

(defun linewrap-and-comment (data width)
  (let ((acc ""))
    (while (< 0 (length data))
      (setq acc (concat acc ";; "(substring data 0 (min width (length data))) "\n"))
      (if (< width (length data))
          (setq data (substring data width nil))
        (setq data nil)))
    acc))

(defun print-4clojure-problem-to-clj-file (4c-problem-endpoint clj-buffer)

  ;; insert namespace declaration
  (if (get-buffer clj-buffer)
      (setq buff (get-buffer clj-buffer))
    (progn (setq buff (get-buffer-create clj-buffer))
           (set-buffer buff)
           (insert (format "(ns %s)\n\n" clj-buffer))))

  (let* ((url (concat "http://4clojure.com" 4c-problem-endpoint))
         (n (car (last (split-string 4c-problem-endpoint "/"))))
         (parsed-body (curl url))
         (title (nth 2 (car (dom-by-id parsed-body "prob-title"))))
         (problem (dom-by-id parsed-body "prob-desc"))
         (problem-text (replace-regexp-in-string "\r\n" "" (flatten problem)))
         (test-cases (extract-4clojure-test-cases parsed-body))
         )
    (set-buffer buff)
    (clojure-mode)
    (insert (apply 'concat (make-list 80 ";")) "\n")
    (insert (concat ";; " title "\n"))
    (insert (concat ";; " url "\n;;\n"))
    (insert (linewrap-and-comment problem-text 77))
    (insert (apply 'concat (make-list 80 ";")) "\n")
    (insert "\n\n")
    (insert (concat "(defn problem-" n " [& xs]\n\t)\n\n"))
    (mapc (lambda (x)
            (when (stringp x)
              (insert (replace-regexp-in-string "__"
                                                (concat "problem-" n)
                                                (replace-regexp-in-string "\r" "" x))
                      "\n\n\n")))
          test-cases))
  (concat "Imported " 4c-problem-endpoint " to " clj-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Retrieve 4c problem list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun follow-4clojure-link (event)
  (let ((problem (thing-at-point 'symbol 'no-properties)))
    (print-4clojure-problem-to-clj-file problem fourclojure-buffer)))

(define-button-type '4clojure-hyperlink
  'action 'follow-4clojure-link
  'follow-link t
  'help-echo "Go to "
  'help-args "test")

;; Extract into add-row function and format-4c function
(defun add-problem (problem)
  (cl-flet ((create-hyperlink ()
                              (progn (goto-char (point-max))
                                     (backward-sexp)
                                     (backward-sexp)
                                     (make-text-button (point) (point-max)
                                                       :type '4clojure-hyperlink)
                                     (goto-char (point-max))
                                     (insert "\n"))))
    (let ((name (nth 0 problem))
          (url (nth 1 problem))
          (difficulty (nth 2 problem)))
      (insert "|" name " | " difficulty " | " url " |")
      (create-hyperlink))))

(defun dom->4clojure-problems (full-dom)
  (cl-flet* ((get-link (row) (cdr (car (nth 1 (nth 2 (nth 2 row))))))
             (get-problem-name (row) (nth 2 (nth 2 (nth 2 row))))
             (get-difficulty (row) (nth 2 (nth 3 row)))
             (extract-problem (row) (let ((link (get-link row))
                                          (problem-name (get-problem-name row))
                                          (difficulty (get-difficulty row)))
                                      (list problem-name link difficulty))))
    (let ((all-rows (cdr (dom-by-tag full-dom 'tr))))
      (mapcar (lambda (x) (extract-problem x)) all-rows))))

(defun populate-buffer-with-4clojure-problems (b)
  (let* ((dom (curl "http://4clojure.com/problems"))
         (problems (dom->4clojure-problems dom))
         (problems-sorted-by-difficulty (cl-sort problems
                                                 'string-lessp
                                                 :key
                                                 'third)))
    ;; Add header
    (set-buffer b)
    (insert "| name | difficulty | url |\n")
    (insert "|---|---|---|\n")

    ;; Add all problems to buffer
    (mapc (lambda (x) (add-problem x)) problems-sorted-by-difficulty))

  ;; Format as org mode table
  (switch-to-buffer b)
  (markdown-mode)
  (org-table-align))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialize 4clojure package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun 4clojure ()
  (interactive)
  (populate-buffer-with-4clojure-problems (generate-new-buffer "*4clojure problems*"))
  (get-buffer-create fourclojure-buffer)
  (spacemacs/layout-double-columns)
  (evil-window-move-far-right)
  (switch-to-buffer fourclojure-buffer)
  (insert (format "(ns %s)\n\n" fourclojure-buffer)))

(provide 'fourclojure)

;;; fourclojure.el ends here
