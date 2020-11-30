;;; org-tree-slide-pauses.el --- 

;; Copyright 2020 cnngimenez
;;
;; Author: cnngimenez
;; Version: $Id: org-tree-slide-pauses.el,v 0.0 2020/11/30 02:29:30  Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'org-tree-slide-pauses)

;;; Code:

(provide 'org-tree-slide-pauses)

;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################


(defconst ots-pauses-pause-regexp "^[[:space:]]*# pause[[:space:]]*$"
  "Regexp to find the pause declaration.") ;; defconst

(defvar ots-pauses-pause-text-list '()
  "List of overlays to hide the \"pause\" text position." )

(defvar ots-pauses-overlay-lists '()
  "List of pauses overlays.
This list is created with the `ots-pauses-search-pauses'.")

(defvar ots-pauses-current-pause 0)

(defun ots-pauses-clear-overlay-list ()
  "Clear the `ots-pauses-overlay-lists'."
  (dolist (the-overlay ots-pauses-overlay-lists)
    (delete-overlay the-overlay))
  (setq ots-pauses-overlay-lists '())

  (dolist (the-overlay ots-pauses-pause-text-list)
    (delete-overlay the-overlay))
  (setq ots-pauses-pause-text-list '())

  (setq ots-pauses-current-pause 0)
  ) ;; defun


(defun ots-pauses--search-elements ()
  "Search all items that needs pauses and return the org-element list."

  (delq
   nil
   (org-element-map (org-element-parse-buffer nil t)
       '(comment item keyword headline)
     (lambda (element)
       "If it is one of the pauses, return their positions"
       (cond

	((eq (org-element-type element) 'keyword)
	 (if (or (string-equal (org-element-property :key element) "PAUSE")
		 (and (string-equal (org-element-property :key element)
				    "BEAMER")
		      (string-equal (org-element-property :value element)
				    "\\pause")))
	     element  
	   nil))
	
	((eq (org-element-type element) 'comment)
	 (if (string-equal (org-element-property :value element) "pause")
	     element
	   nil))
	
	(t element)
	))))
  ) ;; defun

(defun ots-pauses--new-overlay-for-text ()
  "Return new overlays for all elements that needs to be hidden."

  (delq nil
	(mapcar (lambda (element)
		  (unless (member (org-element-type element)
				  '(item headline))
		    (make-overlay
		     (org-element-property :begin element)
		     (org-element-property :end element)))
		  )
		(ots-pauses--search-elements)))
  
  ) ;; defun

(defun ots-pauses--new-overlay-for-pair (element next-element)
  "Creates overlays for a consecutive pair of elements."
  (cond
   ((and (eq (org-element-type element) 'item)
	 (eq (org-element-type next-element) 'item))
    ;; both are items
    (list 
     (make-overlay (org-element-property :begin element)
		   (org-element-property :end element))))

   ((eq (org-element-type element) 'item)
    ;; the first one is an item, the second one is a pause/headline
    (list
     (make-overlay (org-element-property :begin element)
		   (org-element-property :end element))
     (make-overlay (org-element-property :end element)
		   (org-element-property :begin next-element))))

   ((eq (org-element-type next-element) 'item)
    ;; the first one is a pause/headline, the second one is an item
    (list
     (make-overlay (org-element-property :end element)
		   (org-element-property :begin next-element))
     (make-overlay (org-element-property :begin next-element)
		   (org-element-property :end next-element))))

   ((and (eq (org-element-type element) 'headline))
    ;; the first is a headline, ignore it.
    nil)

   (t
    ;; both of them are pauses
    (list
     (make-overlay (org-element-property :end element)
		   (org-element-property :begin next-element)))
    )
   ) ;; cond
  ) ;; defun

(defun ots-pauses--partition (lst-elements)
  "Partition of the LST-ELEMENTS into list of two elements."

  (let ((prev nil)
	(result '()))
    
    (dolist (element lst-elements)
      (add-to-list 'result (cons prev element) t)
      (setq prev element)
      )

    (cdr result))
  
  ) ;; defun


(defun ots-pauses--new-overlay-for-pauses ()
  "Return new overlays for all elements that needs to be paused."
  (delq
   nil
   (apply #'append
	  (mapcar (lambda (element)
		    (ots-pauses--new-overlay-for-pair (car element)
						     (cdr element))
		    )
		  (ots-pauses--partition (ots-pauses--search-elements))
		  )
	  )
   )
  ) ;; defun


(defun ots-pauses-search-pauses ()
  "Hide all pauses."
  (ots-pauses-clear-overlay-list)

  (setq ots-pauses-pause-text-list (ots-pauses--new-overlay-for-text))
  (setq ots-pauses-overlay-lists (ots-pauses--new-overlay-for-pauses))
  )

(defun ots-pauses-hide-pauses ()
  "Hide all pauses."
  (interactive)
  (dolist (the-overlay ots-pauses-pause-text-list)
    (overlay-put the-overlay 'invisible t))
  
  (dolist (the-overlay ots-pauses-overlay-lists)
    (overlay-put the-overlay 'face 'shadow))
  ) ;; defun

(defun ots-pauses-show-pauses ()
  "Show everything to edit the buffer easily.
This do not deletes the overlays that hides the pauses commands, it only make
them visibles."
  (interactive)
  (dolist (the-overlay ots-pauses-pause-text-list)
    (overlay-put the-overlay 'invisible nil))
  ) ;; defun


(defun ots-pauses-init ()
  "This function is intended to be added to the `org-tree-slide-mode-hook'
to start the pauses parsing."
  (ots-pauses-search-pauses)
  (ots-pauses-hide-pauses)
  ) ;; defun

(defun ots-pauses-end ()
  "Restore the buffer and delete overlays."
  (ots-pauses-clear-overlay-list)
  ) ;; defun


(defun ots-pauses-next-pause ()
  "Show next pause"
  
  (when (nth ots-pauses-current-pause ots-pauses-overlay-lists)
    (overlay-put (nth ots-pauses-current-pause ots-pauses-overlay-lists)
		 'face nil)
    (setq ots-pauses-current-pause (1+ ots-pauses-current-pause))
    )
  ) ;; defun


(defun ots-pauses-next-advice (ots-move-next-tree &rest args)
  " "
  (interactive)
  (if (>= ots-pauses-current-pause (length ots-pauses-overlay-lists))       
      (progn
	(apply ots-move-next-tree args)
	;; Parse the current slide, or just in case the user edited the buffer
	
	;; (ots-pauses-init)
	)
    (progn
      (ots-pauses-next-pause)
      (message (format "Pauses: %d/%d"
		       ots-pauses-current-pause
		       (length ots-pauses-overlay-lists)))
      )
    )
  ) ;; defun

(advice-add 'org-tree-slide-move-next-tree :around #'ots-pauses-next-advice)

(add-hook 'org-tree-slide-after-narrow-hook 'ots-pauses-init)

;;; org-tree-slide-pauses.el ends here
