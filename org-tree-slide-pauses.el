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


(defun ots-pauses-search-pauses ()
  "Hide all pauses."
  (ots-pauses-clear-overlay-list)

  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp ots-pauses-pause-regexp nil t)
      (let ((begin-text-1 (match-beginning 0))			
	    (begin-pause (match-end 0))
	    (end-pause (if (search-forward-regexp
			    ots-pauses-pause-regexp nil t)
			   (match-beginning 0)
			 nil))
	    (end-text-2 (match-end 0)))
	(when end-pause
	  (add-to-list 'ots-pauses-pause-text-list
		       (make-overlay begin-text-1 begin-pause))
	  (add-to-list 'ots-pauses-pause-text-list
		       (make-overlay end-pause end-text-2))
	  
	  (add-to-list 'ots-pauses-overlay-lists
		       (make-overlay (1+ begin-pause) (1- end-pause))))
	) ;; let
      ) ;; while
    ) ;; save-excursion

  )

(defun ots-pauses-hide-pauses ()
  "Hide all pauses."
  (dolist (the-overlay ots-pauses-pause-text-list)
    (overlay-put the-overlay 'invisible t))
  
  (dolist (the-overlay ots-pauses-overlay-lists)
    (overlay-put the-overlay 'face 'shadow))
  ) ;; defun

(defun ots-pauses-init ()
  "This function is intended to be added to the `org-tree-slide-mode-hook'
to start the pauses parsing."
  (message "ots-pauses-init")
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


(defun ots-pauses-next-advice ()
  " "
  (interactive)
  (if (>= ots-pauses-current-pause (length ots-pauses-overlay-lists))       
      (progn
	(org-tree-slide-move-next-tree)
	;; Parse the current slide, or just in case the user edited the buffer
	(ots-pauses-init)
	)
    (progn
      (ots-pauses-next-pause)
      (message (format "Pauses: %d/%d"
		       ots-pauses-current-pause
		       (length ots-pauses-overlay-lists)))
      )
    )
  ) ;; defun


(add-hook 'org-tree-slide-after-narrow-hook 'ots-pauses-init)

;;; org-tree-slide-pauses.el ends here
