;;; org-enhanced-priorities.el --- Rank TODOs by priority, impact, and difficulty  -*- lexical-binding: t; -*-

;; Author: Robert Forrest <robertforrest@live.com>
;; Maintainer: Robert Forrest <robertforrest@live.com>
;; Url: https://github.com/Robert-Forrest/org-enhanced-priorities
;; Version: 0.0.1
;; Package-Requires:  ((emacs "26.1") (org "9.0"))
;; Keywords: calendar agenda priority task sort

;;; Commentary:

;;; Enhances Org-mode's default priority-based ranking with impact and difficulty

;;; License:

;; BSD 3-Clause License

;; Copyright (c) 2022, Robert Forrest
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:

;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.

;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.

;; 3. Neither the name of the copyright holder nor the names of its
;;    contributors may be used to endorse or promote products derived from
;;    this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;;; Code:

;;;; Requirements

(require 'org)

;;;; Customization

(defgroup org-enhanced-priorities nil
  "Settings for `org-enhanced-priorities'."
  :group 'org
  :link '(url-link "http://github.com/robert-forrest/org-enhanced-priorities"))

(defcustom org-enhanced-priorities-property-weights
  '(("PRIORITY" . 7.5)
    ("DEADLINE" . 4)
    ("SCHEDULED" . 4)
    ("impact" . 2)
    ("difficulty" . -1))
  "Association list of property names and weights to be considered while ranking."
  :type 'alist
  :group 'org-enhanced-priorities)

(defcustom org-enhanced-priorities-deadline-leadup 14
  "Number of days ahead of a DEADLINE to start increasing the overall priority."
  :type 'integer
  :group 'org-enhanced-priorities)

;;;; Constants

(defconst org-enhanced-priorities-deadline-gradient (/ -3.0 org-enhanced-priorities-deadline-leadup)
  "Gradient of linear function mapping days-to-deadline to 0 to 3 scale.")

;;;; Functions

;;;###autoload
(defun org-enhanced-priorities--get-property (property &optional pos)
  "Get PROPERTY from org item at POS or under cursor."
  (org-entry-get (or pos (point)) property))

;;;###autoload
(defun org-enhanced-priorities--get-numerical-property (property &optional pos)
  "Convert string PROPERTY values for item at POS or under cursor to numbers."
  (let*((property-value (or (org-enhanced-priorities--get-property property pos) "0")))
    (cond ((string= property "PRIORITY") (org-enhanced-priorities--get-numerical-priority pos))
          ((string= property "DEADLINE") (org-enhanced-priorities--get-deadline-contribution pos))
          ((string= property "SCHEDULED") (org-enhanced-priorities--get-scheduled-contribution pos))
          (t (string-to-number property-value)))))

;;;###autoload
(defun org-enhanced-priorities--get-numerical-priority (&optional pos)
  "Map Org entry (at POS or under cursor) priority to its index between `org-lowest-priority` and `org-highest-priority`."
  (let ((priority (org-entry-get (or pos (point)) "PRIORITY")))
    (if priority
        (/ (float (- org-lowest-priority (string-to-char priority))) (- org-lowest-priority org-highest-priority))
      0)))

;;;###autoload
(defun org-enhanced-priorities--get-deadline-contribution (&optional pos)
  "Send DEADLINE property for entry at POS or under cursor through gradient function to get priority contribution, return zero otherwise."
  (let ((deadline (org-enhanced-priorities--get-property "DEADLINE" pos)))
    (if deadline (+ (* org-enhanced-priorities-deadline-gradient (org-time-stamp-to-now deadline)) 3) 0)))

;;;###autoload
(defun org-enhanced-priorities--get-scheduled-contribution (&optional pos)
  "Ramp a priority debuff as scheduled date for task at POS or under cursor approaches."
  (let ((scheduled (org-enhanced-priorities--get-property "SCHEDULED" pos)))
    (if scheduled (* org-enhanced-priorities-deadline-gradient (org-time-stamp-to-now scheduled)) 0)))

;;;###autoload
(defun org-enhanced-priorities--get-marker (item)
  "Get the org-marker from text properties of ITEM."
  (or (get-text-property 0 'org-marker item)
      (get-text-property 0 'org-hd-marker item)))

;;;###autoload
(defun org-enhanced-priorities--calculate-priority-components (&optional pos)
  "Calculate the overall priority of an Org item either at POS or under cursor."
  (cl-loop for (property . weight) in org-enhanced-priorities-property-weights
           collect (cons property (* (org-enhanced-priorities--get-numerical-property property pos) weight))))

;;;###autoload
(defun org-enhanced-priorities--calculate-overall-priority (&optional pos)
  "Calculate the overall priority of an Org item either at POS or under cursor."
  (cl-loop for (property . value) in (org-enhanced-priorities--calculate-priority-components pos) sum value))

;;;###autoload
(defun org-enhanced-priorities--agenda-sort-overall-priority (a b)
  "Compare the overall priority values of items A and B."
  (let*((ma (org-enhanced-priorities--get-marker a))
        (mb (org-enhanced-priorities--get-marker b))
        (total-a (org-enhanced-priorities--calculate-overall-priority ma))
        (total-b (org-enhanced-priorities--calculate-overall-priority mb)))
    (cond ((> total-a total-b) +1)
          ((< total-a total-b) -1))))

;;;###autoload
(defun org-enhanced-priorities--highest-priority (&optional match scope)
  "Open the Org entry with the highest priority.
MATCH specifies the entries to match, defaulting to \"/TODO\".
SCOPE specifies where org entries will be sourced from, defaulting to 'agenda"
  (interactive (list (read-string "Match: " "/TODO") (intern (read-string "Scope: " "agenda"))))
  (let ((max-priority -999)
        max-heading max-marker)
    (org-map-entries
     (lambda ()
       (let ((priority (org-enhanced-priorities--calculate-overall-priority)))
         (when (> priority max-priority)
           (setq max-priority priority
                 max-heading (org-get-heading t t t t)
                 max-marker (point-marker)))))
     (or match "/TODO") (or scope 'agenda))
    (when max-marker
      (switch-to-buffer (marker-buffer max-marker))
      (goto-char max-marker)
      (org-fold-show-entry)
      (message "\"%s\" (priority: %d)" max-heading max-priority))))

;;;###autoload
(defun org-enhanced-priorities--sorted-entries (&optional match scope)
  "Display Org entries sorted by their priority.
MATCH specifies the entries to match, defaulting to \"/TODO\".
SCOPE specifies where org entries will be sourced from, defaulting to 'agenda."
  (interactive (list (read-string "Match: " "/TODO") (intern (read-string "Scope: " "agenda"))))
  (let (entries)
    (org-map-entries
     (lambda ()
       (let* ((todo-state (org-no-properties (org-get-todo-state)))
              (priority (org-enhanced-priorities--calculate-overall-priority))
              (heading (org-get-heading t t t t))
              (entry-text (concat "* " todo-state " [#" (number-to-string (round priority)) "] " heading))
              (marker (point-marker)))
         (when todo-state
           (push (list entry-text marker priority) entries))))
     (or match "/TODO") scope)
    ;; Sort entries by priority (first element in each sublist)
    (setq entries (reverse (sort entries (lambda (a b) (< (caddr a) (caddr b))))))
    (with-temp-buffer-window "*Org Entries Sorted by Priority*" nil nil
      (with-current-buffer "*Org Entries Sorted by Priority*"
        (switch-to-buffer-other-window "*Org Entries Sorted by Priority*")
        (let ((inhibit-read-only t))
          (org-mode)
          (dolist (entry entries)
            (let ((entry-text (car entry))
                  (marker (cadr entry)))
              (goto-char (point-max))
              (put-text-property 0 (length entry-text) 'org-marker marker entry-text)
              (insert (concat entry-text "\n")))))
        (setq buffer-read-only t)
        ;; Define a local keymap with bindings for 'q' and 'C-g' to quit the buffer
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "q") 'quit-window)
          (define-key map (kbd "C-g") 'quit-window)
          (define-key map (kbd "RET")
                      (lambda ()
                        (interactive)
                        (let ((marker (get-text-property (point) 'org-marker)))
                          (when marker
                            (switch-to-buffer (marker-buffer marker))
                            (goto-char marker)
                            (org-fold-show-subtree)))))
          (use-local-map map))))))
;;;; Footer

(provide 'org-enhanced-priorities)

;;; org-enhanced-priorities.el ends here

