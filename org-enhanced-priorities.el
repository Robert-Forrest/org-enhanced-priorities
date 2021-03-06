;;; org-enhanced-priorities.el --- Rank TODOs by priority, impact, and difficulty  -*- lexical-binding: t; -*-

;; Author: Robert Forrest <robertforrest@live.com>
;; Url: https://github.com/Robert-Forrest/org-enhanced-priorities
;; Version: 0.1-pre
;; Package-Requires:  ((emacs "26.1") (org "9.0"))
;; Keywords: calendar

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

(defcustom org-enhanced-priorities-property-weights '(("PRIORITY" . 2.5) ("DEADLINE" . 2) ("impact" . 2) ("difficulty" . -1))
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

(defun org-enhanced-priorities--get-property (property &optional pos)
  "Get PROPERTY from org item at POS or under cursor."
  (org-entry-get (or pos (point)) property))

(defun org-enhanced-priorities--get-numerical-property (property &optional pos)
  "Convert string PROPERTY values for item at POS or under cursor to numbers."
  (let*((property-value (or (org-enhanced-priorities--get-property property pos) "0")))
    (cond ((string= property "PRIORITY") (cond ((string= property-value "A") 3)
                                               ((string= property-value "B") 2)
                                               ((string= property-value "C") 1)))
          ((and (string= property "DEADLINE") (not (string= property-value "0"))) (max 0 (+ (* org-enhanced-priorities-deadline-gradient (org-time-stamp-to-now property-value)) 3)))
          (t (string-to-number property-value)))))

(defun org-enhanced-priorities--get-marker (item)
  "Get the org-marker from text properties of ITEM."
  (or (get-text-property 0 'org-marker item)
      (get-text-property 0 'org-hd-marker item)))

(defun org-enhanced-priorities--calculate-priority-components (&optional pos)
  "Calculate the overall priority of an Org item either at POS or under cursor."
  (cl-loop for (property . weight) in org-enhanced-priorities-property-weights
           collect (cons property (* (org-enhanced-priorities--get-numerical-property property pos) weight))))

(defun org-enhanced-priorities--calculate-overall-priority (&optional pos)
  "Calculate the overall priority of an Org item either at POS or under cursor."
  (cl-loop for (property . value) in (org-enhanced-priorities--calculate-priority-components pos)
           sum value))
  
(defun org-enhanced-priorities--agenda-sort-overall-priority (a b)
  "Compare the overall priority values of items A and B."
  (let*((ma (org-enhanced-priorities--get-marker a))
        (mb (org-enhanced-priorities--get-marker b))
        
        (total-a (org-enhanced-priorities--calculate-overall-priority ma))
        (total-b (org-enhanced-priorities--calculate-overall-priority mb)))
    
    (cond ((> total-a total-b) +1)
          ((< total-a total-b) -1))))

;;;; Footer

(provide 'org-enhanced-priorities)

;;; org-enhanced-priorities.el ends here
