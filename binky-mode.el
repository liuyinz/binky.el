;;; binky-mode.el --- Jump between points like a rabbit -*- lexical-binding: t -*-

;; Copyright (C) 2022 liuyinz

;; Author: liuyinz <liuyinz95@gmail.com>
;; Version: 0.9.0
;; Package-Requires: ((emacs "28"))
;; Keywords: convenience
;; Homepage: https://github.com/liuyinz/binky-mode

;; This file is not a part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides commands to jump between points in buffers and files.
;; Marked position, last jump position and recent buffers are all supported in
;; same mechanism like `point-to-register' and `register-to-point' but with an
;; enhanced experience.

;;; Code:

(require 'cl-lib)
(require 'cl-extra)
(require 'subr-x)

;;; Customize

(defgroup binky nil
  "Jump between points like a rabbit."
  :prefix "binky-"
  :group 'convenience
  :link '(url-link :tag "Repository" "https://github.com/liuyinz/binky-mode"))

(defcustom binky-mark-back ?,
  "Character used as mark to record last position before call `binky-jump'.
Any self-inserting character between ! (33) - ~ (126) is allowed to used as
mark.  Letters, digits, punctuation, etc.  If nil, disable the feature."
  :type '(choice character (const :tag "Disable back mark" nil))
  :group 'binky)

(defcustom binky-mark-quit ?q
  "Character used to quit the commad and preview if exists.
Any self-inserting character between ! (33) - ~ (126) is allowed to used as
mark.  Letters, digits, punctuation, etc.  If nil, disable the feature.

\\[keyboard-quit] and <escape> are enable by default."
  :type '(choice character (const :tag "Disable quit mark" nil))
  :group 'binky)

(defcustom binky-mark-auto
  '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0)
  "List of printable characters to record recent used buffers.
Any self-inserting character between ! (33) - ~ (126) is allowed to used as
marks.  Letters, digits, punctuation, etc.  If nil, disable the feature."
  :type '(choice (repeat (choice (character :tag "Printable character as mark")))
                 (const :tag "Disable auto marks" nil))
  :group 'binky)

(defcustom binky-mark-sort-by 'recency
  "Sorting strategy for used buffers."
  :type '(choice (const :tag "Sort by recency" recency)
                 (const :tag "Sort by frequency" frequency)
                 ;; TODO
                 ;; (cosnt :tag "Sort by frecency" frecency)
                 ;; (cosnt :tag "Sort by duration" duration)
                 )
  :group 'binky)

;; ;; TODO
;; (defcustom binky-mark-distance 200
;;   "Maxmium distance bwtween points for them to be considered equal."
;;   :type 'number
;;   :group 'binky)

(defcustom binky-mark-overwrite nil
  "If non-nil, overwrite record with existing mark when call `binky-add'."
  :type 'boolean
  :group 'binky)

(defcustom binky-include-regexps
  '("\\`\\*\\(scratch\\|info\\)\\*\\'")
  "List of regexps for buffer name included in `binky-auto-alist'.
For example, buffer *scratch* is always included by default."
  :type '(repeat string)
  :group 'binky)

(defcustom binky-exclude-regexps
  '("\\`\\(\\s-\\|\\*\\).*\\'")
  "List of regexps for buffer name excluded from `binky-auto-alist'.
When a buffer name matches any of the regexps, it would not be record
automatically unless it matchs `binky-include-regexps'.  By default, all buffer
names start with '*' or ' ' are excluded."
  :type '(repeat string)
  :group 'binky)

(defcustom binky-exclude-modes
  '(xwidget-webkit-mode)
  "List of major modes which buffers belong to excluded from `binky-auto-alist'."
  :type '(repeat symbol)
  :group 'binky)

(defcustom binky-exclude-functions
  (list #'minibufferp)
  "List of predicates which buffers satisfy exclude from `binky-auto-alist'.
A predicate is a function with no arguments to check the `current-buffer'
and that must return non-nil to exclude it."
  :type '(repeat function)
  :group 'binky)

(defcustom binky-preview-delay 0.5
  "If non-nil, time to wait in seconds before popping up a preview window.
If nil, disable preview, unless \\[help] is pressed."
  :type '(choice number (const :tag "No preview unless requested" nil))
  :group 'binky)

(defcustom binky-preview-side 'bottom
  "Which side to popup preview buffer."
  :type '(choice (const top)
                 (const bottom)
				 (cosnt left)
				 (cosnt right))
  :group 'binky)

(defcustom binky-preview-column
  '((mark    10   4)
    (name    28  15)
    (line    10  6)
    (mode    22  nil)
    (context 0   nil))
  "List of elements (COLUMN VERTICAL HORIZONTAL) to display preview.
COLUMN is one of five parameters of record, listed in `binky-alist'
and `binky-auto-alist'.

The `mark' is column to show mark.
The `name' is column to show buffer or file name.
The `line' is column to show line number.
The `mode' is column to show major mode.
The `context' is column to show line content.

VERTICAL and HORIZONTAL are width of the COLUMN depended on
`binky-preview-side'.  VERTICAL used for `top' and `bottom',
HORIZONTAL used for `left' and `right'.
If it's is nil, then COLUMN would not be displayed.
If it's 0, the COLUMN would not be truncated.
Usually, `context' column should be at the end and not truncated."
  ;; :type '(list symbol (choice number (const nil)) (choice number (const nil)))
  :type '(alist
          :key-type symbol
          :options '(mark name line mode context)
		  :value-type '(group (choice number (const nil))
							  (choice number (const nil))))
  :group 'binky)

(defcustom binky-preview-ellipsis ".."
  "String used to abbreviate text in preview."
  :type 'string
  :group 'binky)

(defcustom binky-preview-show-header t
  "If non-nil, showing header in preview."
  :type 'boolean
  :group 'binky)

(defcustom binky-preview-auto-first t
  "If non-nil, showing `binky-mark-auto' first in preview buffer."
  :type 'boolean
  :group 'binky)

(defcustom binky-highlight-duration 0.3
  "If non-nil, time in seconds to highlight the line operated on.
If nil, disable the highlight feature."
  :type '(choice number (const :tag "Disable highlight" nil))
  :group 'binky)

(defface binky-preview-header
  '((t :inherit font-lock-constant-face :underline t))
  "Face used to highlight the header in preview buffer."
  :group 'binky)

(defface binky-preview-column-mark-auto
  '((t :inherit font-lock-function-name-face :bold t))
  "Face used to highlight the auto mark of record in preview buffer."
  :group 'binky)

(defface binky-preview-column-mark-back
  '((t :inherit font-lock-type-face :bold t))
  "Face used to highlight the back mark of record in preview buffer."
  :group 'binky)

(defface binky-preview-column-mark
  '((t :inherit font-lock-variable-name-face :bold t))
  "Face used to highlight the mark of record in preview buffer."
  :group 'binky)

(defface binky-preview-column-name
  '((t :inherit default))
  "Face used to highlight the name of record in preview buffer."
  :group 'binky)

(defface binky-preview-column-line
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight the line number of record in preview buffer."
  :group 'binky)

(defface binky-preview-column-mode
  '((t :inherit font-lock-type-face))
  "Face used to highlight the major mode of record in preview buffer."
  :group 'binky)

(defface binky-preview-shadow
  '((t :inherit font-lock-comment-face))
  "Face used to highlight whole record of killed buffers in preview buffer."
  :group 'binky)

(defface binky-highlight-add
  '((t :inherit diff-refine-added))
  "Face used to highlight the line added to record."
  :group 'binky)

(defface binky-highlight-delete
  '((t :inherit diff-refine-removed))
  "Face used to highlight the line deleted from record."
  :group 'binky)

(defface binky-highlight-jump
  '((t :inherit highlight))
  "Face used to highlight the line jumped to."
  :group 'binky)

(defface binky-highlight-warn
  '((t :inherit diff-refine-changed))
  "Face used to highlight the line already record."
  :group 'binky)

;;; Variables

(defvar binky-alist nil
  "List of records (MARK . INFO) set and updated by mannual.
MARK is a downcase letter between a-z.  INFO is a marker or a list of form
 (filename line major-mode context position) use to stores point information.")

(defvar binky-auto-alist nil
  "Alist of records (MARK . MARKER), set and updated automatically.")

(defvar binky-back-record nil
  "Record of last position before `binky-jump', set and updated automatically.")

(defvar binky-frequency-timer nil
  "Timer used to automatically increase buffer frequency.")

(defvar binky-frequency-idle 3
  "Number of seconds of idle time to wait before increasing frequency.")

(defvar-local binky-frequency 0
  "Frequency of current buffer.")

(defvar binky-preview-buffer "*Binky Preview*"
  "Buffer used to preview records in binky alists.")

(defvar-local binky-overlay nil
  "Overlay used to highlight the line operated on.")

(defvar binky-debug-buffer "*Binky Debug*"
  "Buffer used to debug.")

(defvar binky-binky-keep-alive nil
  "If t, keep preview.")

;;; Functions

;; (defun binky--message (mark)
;;   "docstring"
;;   (error "")
;;   )

(defun binky--log (&rest args)
  "Print log into `binky-debug-buffer' about ARGS.
ARGS format is as same as `format' command."
  (with-current-buffer (get-buffer-create binky-debug-buffer t)
    (goto-char (point-max))
    (insert "\n")
    (insert (apply #'format args))))

(defun binky--regexp-match (lst)
  "Return non-nil if current buffer name match the LST."
  (and lst (string-match-p
            (mapconcat (lambda (x) (concat "\\(?:" x "\\)")) lst "\\|")
            (buffer-name))))

(defun binky--exclude-regexp-p ()
  "Return non-nil if current buffer name should be exclude."
  (and (not (binky--regexp-match binky-include-regexps))
       (binky--regexp-match binky-exclude-regexps)))

(defun binky--exclude-mode-p ()
  "Return non-nil if current buffer major mode should be exclude."
  (and binky-exclude-modes
       (memq mode-name binky-exclude-modes)))

(defun binky--frequency-increase ()
  "Frequency increases by 1 in after each idle."
  (cl-incf binky-frequency 1))

(defun binky--frequency-get (marker)
  "Return value of `binky-frequency' of buffer which MARKER points to."
  (or (buffer-local-value 'binky-frequency (marker-buffer marker)) 0))

(defun binky--record-get-info (record)
  "Return info list of elements (name line mode context position) from RECORD."
  (let* ((info (cdr record))
         (pos (marker-position info)))
    (with-current-buffer (marker-buffer info)
      (list (or buffer-file-name (buffer-name) "")
            (line-number-at-pos pos 'absolute)
            major-mode
            (save-excursion
              (goto-char info)
              (buffer-substring (pos-bol) (pos-eol)))
            pos))))

(defun binky-record-auto-update ()
  "Update `binky-auto-alist' and `binky-back-record' automatically."
  ;; delete back-record if buffer not exists
  (when (and binky-back-record
             (null (marker-buffer (cdr binky-back-record))))
    (setq binky-back-record nil))
  ;; update used buffers
  (let* ((marks (remove binky-mark-back binky-mark-auto))
         (len (length marks))
         (result (list))
         (filters (append binky-exclude-functions
                          '(binky--exclude-mode-p
                            binky--exclude-regexp-p))))
    (when (> len 0)
      ;; remove current-buffer
      (cl-dolist (buf (nthcdr 1 (buffer-list)))
        (with-current-buffer buf
          (unless (cl-some #'funcall filters)
            (push (point-marker) result))))
      ;; delete marker duplicated with `binky-alist'
      (setq result (cl-remove-if (lambda (m) (rassoc m binky-alist)) result))
      (cl-case binky-mark-sort-by
        (recency
         (setq result (reverse result)))
        (frequency
         (cl-sort result #'> :key #'binky--frequency-get))
        ;; TODO
        ;; (frecency ())
        ;; (duration ())
        (t nil))
      (setq binky-auto-alist (cl-mapcar (lambda (x y) (cons x y))
                                        marks
                                        result)))))

(defun binky-record-swap-out ()
  "Turn record from marker into list of infos when a buffer is killed."
  (dolist (record binky-alist)
    (let ((info (cdr record)))
	  (when (and (markerp info)
	             (eq (marker-buffer info) (current-buffer)))
        (if buffer-file-name
	        (setcdr record (binky--record-get-info record))
          (delete record binky-alist))))))

(defun binky-record-swap-in ()
  "Turn record from list of infos into marker when a buffer is reopened."
  (dolist (record binky-alist)
    (let ((info (cdr record)))
      (when (and (not (markerp info))
                 (equal (car info) buffer-file-name))
        (setcdr record (set-marker (make-marker) (car (last info))))))))

(defun binky--preview-horizontal-p ()
  "Return non-nil if binky preview buffer in horizontally."
  (not (null (memq binky-preview-side '(left right)))))

(defun binky--preview-column ()
  "Return alist of elements (COLUMN . WIDTH) to display preview."
  (cl-remove-if (lambda (x) (null (cdr x)))
                (mapcar (lambda (f)
  					      (cons (nth 0 f)
  						        (nth (if (binky--preview-horizontal-p) 2 1) f)))
  				        binky-preview-column)))

(defun binky--preview-extract (alist)
  "Return truncated string with selected columns according to ALIST."
  (format "%s%s\n"
          (if (binky--preview-horizontal-p) "" "  ")
		  (mapconcat
		   (lambda (x)
			 (let* ((item (car x))
					(limit (cdr x))
					(str (alist-get item alist))
					(len (length str)))
			   (if (zerop limit)
				   (substring str 0 nil)
				 (setf limit (max limit (length (symbol-name item))))
				 (and (> len limit)
                      (setf (substring str
                                       (- limit (length binky-preview-ellipsis))
                                       limit)
							binky-preview-ellipsis))
				 (setf (substring str len nil) (make-string limit 32))
				 (substring str 0 limit))))
		   (binky--preview-column) "  ")))

(defun binky--preview-propertize (record)
  "Return formated string for RECORD in preview."
  (let ((killed (not (markerp (cdr record)))))
    (or killed (setq record (cons (car record) (binky--record-get-info record))))
    (cl-mapcar
     (lambda (x y)
	   (let ((column-face (intern (concat "binky-preview-column-" (symbol-name x))))
             (cond-face (cond
                         (killed 'binky-preview-shadow)
                         ((and (eq x 'mark)
							   (memq (binky--mark-type (string-to-char (substring y -1))) '(auto back)))
						  (intern (concat "binky-preview-column-mark-"
										  (symbol-name (binky--mark-type
                                                        (string-to-char (substring y -1)))))))
                         (t nil))))
         (cons x (if (or killed (facep column-face))
                     (propertize y 'face (or cond-face column-face))
				   y))))
     '(mark name line mode context)
     (list (concat "  " (single-key-description (nth 0 record)))
		   (file-name-nondirectory (nth 1 record))
		   (number-to-string (nth 2 record))
		   (string-remove-suffix "-mode" (symbol-name (nth 3 record)))
		   (string-trim (nth 4 record))))))

(defun binky--preview-header ()
  "Return formated string of header for preview."
  (binky--preview-extract
   (mapcar (lambda (x)
             (cons (car x) (propertize (symbol-name (car x))
                                       'face
                                       'binky-preview-header)))
		   (binky--preview-column))))

(defun binky-preview (&optional force)
  "Preview records of marked positions in `binky-preview-buffer'.
When there is no window currently showing the buffer or FORCE is non-nil,
popup the window on the side `binky-preview-side'."
  (let ((total (remove nil
					   (cons binky-back-record
                             (if binky-preview-auto-first
                                 (append binky-auto-alist binky-alist)
							   (append binky-alist binky-auto-alist))))))
    (when (or force
			  (not (get-buffer-window binky-preview-buffer)))
	  (with-current-buffer-window
		  binky-preview-buffer
		  (cons 'display-buffer-in-side-window
				(append
				 `((dedicated . t)
				   (side      . ,binky-preview-side))
				 (if (binky--preview-horizontal-p)
					 '((window-width  . fit-window-to-buffer)
					   (preserve-size . (t . t)))
				   '((window-height . fit-window-to-buffer)
					 (preserve-size . (nil . t))))))
		  nil
		(progn
		  (setq cursor-in-non-selected-windows nil
				mode-line-format nil
				truncate-lines t)
		  (setq-local fit-window-to-buffer-horizontally t)
		  (let* ((final (mapcar #'binky--preview-propertize total))
				 (back (and binky-back-record
							(binky--preview-propertize binky-back-record)))
				 (dup (and back (rassoc (cdr back) (cdr final)))))
            (erase-buffer)
			;; insert header if non-nil
			(when (and (cl-some #'numberp (mapcar #'cdr (binky--preview-column)))
					   binky-preview-show-header)
			  (insert (binky--preview-header)))
			(when dup
			  (setf (cdar dup)
					(concat (substring (cdar back) -1)
							(substring (cdar dup) 1))))
			(mapc (lambda (record)
					(insert (binky--preview-extract record)))
				  (if dup (cdr final) final))))))))

(defun binky--highlight (cmd)
  "Highlight the line CMD operated on in `binky-highlight-duration' seconds."
  (when (and (numberp binky-highlight-duration)
		     (> binky-highlight-duration 0))
    (let ((beg (line-beginning-position))
          (end (line-beginning-position 2)))
      (if binky-overlay
          (move-overlay binky-overlay beg end)
	    (setq binky-overlay (make-overlay beg end)))
	  (overlay-put binky-overlay
                   'face
                   (intern (concat "binky-highlight-" (symbol-name cmd)))))
    (sit-for binky-highlight-duration)
    (delete-overlay binky-overlay)))

(defun binky--mark-type (&optional mark)
  "Return type of MARK or `last-input-event'.
The `quit' means to quit the command and preview.
The `help' means to preview records if not exist.
The `back' means to jump back last position.
The `auto' means to jump to used buffers.
The `mannual' means to operate on records mannually.
The `delete' means to delete existing mark by uppercase."
  (let ((char (or mark last-input-event)))
    (cond
     ((memq char (cons binky-mark-quit '(?\C-g ?\C-\[ escape))) 'quit)
     ((memq char (cons help-char help-event-list)) 'help)
     ((equal char binky-mark-back) 'back)
     ((memq char binky-mark-auto) 'auto)
     ((and (characterp char) (<= 97 char 122)) 'mannual)
     ((and (characterp char) (<= 65 char 90)) 'delete)
     (t nil))))

(defun binky--mark-exist (mark)
  "Return non-nil if MARK already exists in records."
  (or (alist-get mark (list binky-back-record))
	  (alist-get mark binky-alist)
	  (alist-get mark binky-auto-alist)))

(defun binky--mark-add (mark)
  "Add (MARK . MARKER) into records."
  (cond
   ((not (eq (binky--mark-type mark) 'mannual))
    (message "%s not allowed." mark))
   ((eq major-mode 'xwidget-webkit-mode)
    (message "%s not allowed" major-mode))
   ((and (binky--mark-exist mark) (not binky-mark-overwrite))
    (binky--highlight 'warn)
    (message "Mark %s exists." mark))
   ((rassoc (point-marker) binky-alist)
    (binky--highlight 'warn)
    (message "Record exists." ))
   (t
    (binky--highlight 'add)
    (setf (alist-get mark binky-alist) (point-marker)))))

(defun binky--mark-delete (mark)
  "Delete (MARK . INFO) from `binky-alist'."
  (if-let* ((mark (downcase mark))
            (info (binky--mark-exist mark))
            ((eq (binky--mark-type mark) 'mannual)))
      (progn
        (when (markerp info)
          (with-current-buffer (marker-buffer info)
            (save-excursion
              (goto-char info)
              (binky--highlight 'delete))))
	    (setq binky-alist (assoc-delete-all mark binky-alist)))
    (message "%s is not allowed." mark)))

(defun binky--mark-jump (mark)
  "Jump to point according to (MARK . INFO) in records."
  (if-let ((info (binky--mark-exist mark)))
	  (progn
        (if (characterp binky-mark-back)
            (setq binky-back-record (cons binky-mark-back (point-marker)))
		  (setq binky-back-record nil))
        (if (markerp info)
            (progn
			  (switch-to-buffer (marker-buffer info))
			  (goto-char info))
		  (find-file (car info))
		  (goto-char (car (last info))))
        (binky--highlight 'jump))
    (message "No marks %s" mark)))

(defun binky-mark-read (prompt &optional keep-alive)
  "Read and return a MARK possibly with preview.
Prompt with the string PROMPT and  may display a window listing exisiting
records after `binky-preview-delay' seconds.  When KEEP-ALIVE is non-nil,
preview buffer keep alive.

If `help-char' (or a member of `help-event-list') is pressed, display preview
window regardless.  Press \\[keyboard-quit] to quit."
  (and keep-alive (binky-preview 'force))
  (let ((timer (when (numberp binky-preview-delay)
		         (run-with-timer binky-preview-delay nil #'binky-preview))))
    (unwind-protect
        (progn
		  (while (eq (binky--mark-type
					  (read-key (propertize prompt 'face 'minibuffer-prompt)))
                     'help)
            (binky-preview))
		  (when (eq (binky--mark-type) 'quit)
            (keyboard-quit))
		  (if (memq (binky--mark-type) '(auto back mannual delete))
			  last-input-event
            (message "Non-character input-event")))
	  (and (timerp timer) (cancel-timer timer))
      (when (or (eq (binky--mark-type) 'quit)
                (null keep-alive))
	    (let* ((buf binky-preview-buffer)
               (win (get-buffer-window buf)))
          (and (window-live-p win) (delete-window win))
          (and (get-buffer buf) (kill-buffer buf)))))))

;;; Commands

;;;###autoload
(defun binky-add (mark)
  "Add the record in current point with MARK."
  (interactive (list (binky-mark-read "Mark add: ")))
  (binky--mark-add mark))

;;;###autoload
(defun binky-delete (mark)
  "Delete the record according to MARK."
  (interactive (list (binky-mark-read "Mark delete: ")))
  (binky--mark-delete mark))

;;;###autoload
(defun binky-jump (mark)
  "Jump to point according to record of MARK."
  (interactive (list (binky-mark-read "Mark jump: ")))
  (binky--mark-jump mark))

;;;###autoload
(defun binky-binky (mark &optional keep-alive)
  "Add, delete or jump records with MARK in one command.

If MARK exists, then call `binky-jump'.
If MARK doesn't exist, then call `binky-add'.
If MARK is uppercase, and the lowercase exists, then call `binky-delete'.

Interactively, KEEP-ALIVE is the prefix argument.  With no prefix argument,
it works as same as single command.  WIth a preifx argument, preview the
records with no delay and keep alive untill \\[keyboard-quit] pressed."
  (interactive (list (binky-mark-read "Mark: " current-prefix-arg)
                     current-prefix-arg))
  (if (binky--mark-exist mark)
	  (binky--mark-jump mark)
    (if (eq (binky--mark-type mark) 'mannual)
        (binky--mark-add mark)
      (binky--mark-delete mark)))
  (when keep-alive
    (binky-preview 'force)
    (call-interactively 'binky-binky)))

(define-minor-mode binky-mode
  "Toggle `binky-mode'.
This global minor mode allows you to jump easily between buffers
you used and marked position."
  :group 'binky
  :global t
  :require 'binky-mode
  (if binky-mode
	  (progn
        (add-hook 'buffer-list-update-hook 'binky-record-auto-update)
        (add-hook 'kill-buffer-hook 'binky-record-swap-out)
        (add-hook 'find-file-hook 'binky-record-swap-in)
        (setq binky-frequency-timer
			  (run-with-idle-timer binky-frequency-idle
								   t #'binky--frequency-increase)))
    (remove-hook 'buffer-list-update-hook 'binky-record-auto-update)
    (remove-hook 'kill-buffer-hook 'binky-record-swap-out)
    (remove-hook 'find-file-hook 'binky-record-swap-in)
    (cancel-timer binky-frequency-timer)
    (setq binky-frequency-timer nil)))

(provide 'binky-mode)
;;; binky-mode.el ends here
