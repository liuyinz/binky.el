;;; binky.el --- Jump between points like a rabbit -*- lexical-binding: t -*-

;; Copyright (C) 2022, 2023 liuyinz

;; Author: liuyinz <liuyinz95@gmail.com>
;; Version: 1.2.1
;; Package-Requires: ((emacs "26.3"))
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
(require 'seq)
(require 'subr-x)

;;; Customize

(define-obsolete-variable-alias 'binky-mark-auto 'binky-mark-recent "1.2.0")
(define-obsolete-variable-alias 'binky-record-sort-by 'binky-recent-sort-by "1.2.0")
(define-obsolete-variable-alias 'binky-record-distance 'binky-distance "1.2.0")
(define-obsolete-variable-alias 'binky-record-prune 'binky-prune "1.2.0")
(define-obsolete-variable-alias 'binky-mark-overwrite 'binky-overwrite "1.2.2")
(define-obsolete-variable-alias 'binky-preview-auto-first 'binky-preview-order "1.2.0")
(define-obsolete-variable-alias 'binky-margin-side 'binky-indicator-side "1.2.2")

(define-obsolete-face-alias 'binky-preview-column-mark-auto 'binky-preview-mark-recent "1.2.0")
(define-obsolete-face-alias 'binky-preview-column-mark-back 'binky-preview-mark-back "1.2.0")
(define-obsolete-face-alias 'binky-preview-column-mark-manual 'binky-preview-mark-manual "1.2.0")
(define-obsolete-face-alias 'binky-preview-column-name 'binky-preview-name "1.2.0")
(define-obsolete-face-alias 'binky-preview-column-name-same 'binky-preview-name-same "1.2.0")
(define-obsolete-face-alias 'binky-preview-column-line 'binky-preview-line "1.2.0")
(define-obsolete-face-alias 'binky-preview-column-mode 'binky-preview-mode "1.2.0")


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
  "Character used to quit the command and preview if exists.
Any self-inserting character between ! (33) - ~ (126) is allowed to used as
mark.  Letters, digits, punctuation, etc.  If nil, disable the feature.

\\[keyboard-quit] and <escape> are enable by default."
  :type '(choice character (const :tag "Disable quit mark" nil))
  :group 'binky)

(defcustom binky-mark-recent
  '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0)
  "List of printable characters to record recent used buffers.
Any self-inserting character between ! (33) - ~ (126) is allowed to used as
marks.  Letters, digits, punctuation, etc.  If nil, disable the feature."
  :type '(choice (repeat (choice (character :tag "Printable character as mark")))
                 (const :tag "Disable recent marks" nil))
  :group 'binky)

(defcustom binky-recent-sort-by 'recency
  "Sorting strategy for recent marked records."
  :type '(choice (const :tag "Sort by recency" recency)
                 (Const :tag "Sort by frequency" frequency))
  :package-version '(binky . "1.2.0")
  :group 'binky)

(defcustom binky-distance 5
  "Maximum distance in lines count between records to be considered same."
  :type 'integer
  :package-version '(binky . "1.2.0")
  :group 'binky)

(defcustom binky-overwrite nil
  "If non-nil, overwrite record with existing mark when call `binky-add'."
  :type 'boolean
  :package-version '(binky . "1.2.2")
  :group 'binky)

(defcustom binky-prune nil
  "If non-nil, delete related record when buffer was killed."
  :type 'boolean
  :group 'binky)

(defcustom binky-include-regexps
  '("\\`\\*\\(scratch\\|info\\)\\*\\'")
  "List of regexps for buffer name included in `binky-recent-alist'.
For example, buffer *scratch* is always included by default."
  :type '(repeat string)
  :package-version '(binky . "1.2.0")
  :group 'binky)

(defcustom binky-exclude-regexps
  '("\\`\\(\\s-\\|\\*\\).*\\'")
  "List of regexps for buffer name excluded from `binky-recent-alist'.
When a buffer name matches any of the regexps, it would not be record
automatically unless it matches `binky-include-regexps'.  By default, all buffer
names start with '*' or ' ' are excluded."
  :type '(repeat string)
  :group 'binky)

(defcustom binky-exclude-modes
  '(xwidget-webkit-mode)
  "List of major modes which buffers belong to excluded from `binky-recent-alist'."
  :type '(repeat symbol)
  :group 'binky)

(defcustom binky-exclude-functions
  (list #'minibufferp)
  "List of predicates which buffers satisfy exclude from `binky-recent-alist'.
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
  '((mark    0.04  4)
    (name    0.16  15)
    (line    0.05  6)
    (mode    0.12  nil)
    (context 0     nil))
  "List of elements (COLUMN VERTICAL HORIZONTAL) to display preview.
COLUMN is one of five parameters of record, listed in `binky-manual-alist'
and `binky-recent-alist'.

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
If it's float number between 0 and 1 rather than integer, then width is
relative based on current frame width.
Usually, `context' column should be at the end and not truncated."
  :type '(alist
          :key-type symbol
          :options '(mark name line mode context)
		  :value-type '(group (choice integer (const nil))
							  (choice integer (const nil))))
  :package-version '(binky . "1.2.0")
  :group 'binky)

(defcustom binky-preview-ellipsis ".."
  "String used to abbreviate text in preview."
  :type 'string
  :group 'binky)

(defcustom binky-preview-order '(back manual recent)
  "If non-nil, showing `binky-mark-recent' first in preview."
  :type '(repeat (choice (const back)
                         (const manual)
                         (const recent)))
  :package-version '(binky . "1.2.0")
  :group 'binky)

(defcustom binky-preview-in-groups nil
  "If non-nil, preview manual records in group by buffer."
  :type 'boolean
  :package-version '(binky . "1.2.1")
  :group 'binky)

(defcustom binky-preview-show-header t
  "If non-nil, showing header in preview."
  :type 'boolean
  :group 'binky)

(defcustom binky-highlight-duration 0.3
  "If non-nil, time in seconds to highlight the line operated on.
If nil, disable the highlight feature."
  :type '(choice number (const :tag "Disable highlight" nil))
  :group 'binky)

(defcustom binky-indicator-side 'left
  "Which side to show indicator on margin or fringe."
  :type '(choice (const left)
                 (const right))
  :package-version '(binky . "1.2.2")
  :group 'binky)

(defface binky-preview-header
  '((t :inherit font-lock-type-face :underline t))
  "Face used to highlight the header in preview buffer."
  :group 'binky)

(defface binky-preview-mark-recent
  '((t :inherit font-lock-constant-face
       :bold t
       :italic nil
       :inverse-video nil))
  "Face used to highlight the recent mark of record in preview."
  :group 'binky)

(defface binky-preview-mark-back
  '((t :inherit font-lock-type-face
       :bold t
       :italic nil
       :inverse-video nil))
  "Face used to highlight the back mark of record in preview."
  :group 'binky)

(defface binky-preview-mark-manual
  '((t :inherit font-lock-keyword-face
       :bold t
       :italic nil
       :inverse-video nil))
  "Face used to highlight the mark of record in preview."
  :group 'binky)

(defface binky-preview-name
  '((t :inherit default))
  "Face used to highlight the name of record in preview."
  :group 'binky)

(defface binky-preview-name-same
  '((t :inherit binky-preview-name :underline t))
  "Face used to highlight the name of record in same buffer in preview."
  :group 'binky)

(defface binky-preview-line
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight the line number of record in preview."
  :group 'binky)

(defface binky-preview-mode
  '((t :inherit font-lock-function-name-face))
  "Face used to highlight the major mode of record in preview."
  :group 'binky)

(defface binky-preview-killed
  '((t :inherit font-lock-comment-face))
  "Face used to highlight whole record of killed buffers in preview."
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
  '((t :inherit highlight :extend t))
  "Face used to highlight the line jumped to."
  :group 'binky)

(defface binky-highlight-view
  '((t :inherit diff-refine-changed :extend t))
  "Face used to highlight the line viewed."
  :group 'binky)

(defface binky-highlight-warn
  '((t :inherit warning :inverse-video t :extend t))
  "Face used to highlight the line already record."
  :group 'binky)

;;; Variables

(defvar binky-manual-alist nil
  "List of records (MARK . INFO) set and updated by manual.
MARK is a lowercase letter between a-z.  INFO is a marker or a list of form
 (filename line mode context position) use to stores point information.")

(defvar binky-recent-alist nil
  "List of records (MARK . MARKER), set and updated by recent buffers.")

(defvar binky-back-record nil
  "Record of last position before `binky-jump', set and updated automatically.")

(defvar binky-frequency-timer nil
  "Timer used to automatically increase buffer frequency.")

(defvar binky-frequency-idle 3
  "Number of seconds of idle time to wait before increasing frequency.")

(defvar-local binky-frequency 0
  "Frequency of current buffer.")

(defvar binky-preview-buffer "*binky-preview*"
  "Buffer used to preview records.")

(defvar binky-debug-buffer "*binky-debug-log*"
  "Buffer used to debug.")

(defvar binky-manual-alist-update-hook nil
  "Hook run when the variable `binky-manual-alist' changes.")

(defvar binky-recent-alist-update-hook nil
  "Hook run when the variable `binky-recent-alist' changes.")

(defvar binky-back-record-update-hook nil
  "Hook run when the variable `binky-back-record' changes.")

(defvar-local binky--highlight-overlay nil
  "Overlay used to highlight the line operated on.")

(defvar binky-current-buffer nil
  "Buffer where binky command called from.")

(defvar binky-current-type nil
  "Type of `last-input-event'.")

(defvar binky-mark-legal nil
  "List of legal marks used in all records.")

(defvar binky-mark-manual nil
  "List of legal marks used in manual records.")


;;; Functions

(defun binky--log (&rest args)
  "Print log into `binky-debug-buffer' about ARGS.
ARGS format is as same as `format' command."
  (with-current-buffer (get-buffer-create binky-debug-buffer)
    (goto-char (point-max))
    (insert "\n" (apply #'format args))))

(defun binky--message (mark status)
  "Echo information about MARK according to STATUS."
  (let ((message-map '((illegal   . "is illegal")
                       (overwrite . "is overwritten")
                       (exist     . "already exists")
                       (non-exist . "doesn't exist")
                       (toggle    .  "toggle groups view"))))
    (message "Mark %s %s."
             (propertize (single-key-description mark t)
                         'face
                         'binky-preview-mark-manual)
             (alist-get status message-map))
    (sit-for 0.8 t)))

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

(defun binky--distance (x y)
  "Return line distance between marker X and Y when in same buffer."
  (when-let* (((markerp x))
              ((markerp y))
              ((eq (marker-buffer x) (marker-buffer y)))
              (buf (marker-buffer x)))
    (with-current-buffer buf
      (abs (- (line-number-at-pos x 'absolute)
              (line-number-at-pos y 'absolute))))))

(defun binky--duplicated-p (marker &optional distance)
  "Return non-nil if MARKER equals with any record of `binky-manual-alist'.
When the line MARKER at has no larger distance with DISTANCE, return that
record."
  (seq-find (lambda (x)
              (and (markerp (cdr x))
                   (ignore-errors
                     (<= (binky--distance marker (cdr x))
                         (or distance binky-distance)))))
            binky-manual-alist))

(defun binky--normalize (record)
  "Return RECORD in normalized style (mark name file line mode context position)."
  (if-let* ((info (cdr record))
            ((markerp info))
            (pos (marker-position info)))
      (with-current-buffer (marker-buffer info)
        (list (car record)
              (or (buffer-name) "")
              (buffer-file-name)
              (line-number-at-pos pos 'absolute)
              major-mode
              (save-excursion
                (goto-char info)
                (buffer-substring (line-beginning-position) (line-end-position)))
              pos))
    record))

(defun binky--prop-get (record prop)
  "Return the property PROP of RECORD, or nil if none."
  (let ((record (binky--normalize record)))
    (cl-case prop
      (mark (nth 0 record))
      (name (nth 1 record))
      (file (nth 2 record))
      (line (nth 3 record))
      (mode (nth 4 record))
      (context (nth 5 record))
      (position (nth 6 record)))))

(defun binky--aggregate (style)
  "Return aggregated records according to STYLE."
  (delq
   nil
   (cl-case style
     (sum
      ;; orderless, non-uniq
      (cons binky-back-record (append binky-manual-alist binky-recent-alist)))
     (indicator
      ;; orderless, uniq
      (cons binky-back-record
            (seq-remove (lambda (x)
                          (or (not (markerp (cdr x)))
                              (eq (cdr x) (cdr binky-back-record))))
                        (append binky-manual-alist binky-recent-alist))))
     (preview
      ;; order, uniq
      (seq-reduce #'append
                  (mapcar
                   (lambda (x)
                     (alist-get x `((back   . ,(list binky-back-record))
                                    (recent . ,binky-recent-alist)
                                    (manual . ,(binky--manual-alist-preview)))))
                   binky-preview-order) nil)))))

(defun binky--auto-update ()
  "Update `binky-recent-alist' and `binky-back-record' automatically."
  ;; delete back-record if buffer not exists
  (when (and binky-back-record
             (null (marker-buffer (cdr binky-back-record))))
    (setq binky-back-record nil)
    (run-hooks 'binky-back-record-update-hook))
  ;; update recent marked records
  (let ((marks (remove binky-mark-back binky-mark-recent))
        (result (list)))
    (if (null marks)
        (setq binky-recent-alist nil)
      ;; remove current-buffer and last buffer(if current-buffer if minibuffer)
      (dolist (buf (nthcdr (if (minibufferp (current-buffer)) 2 1) (buffer-list)))
        (with-current-buffer buf
          (unless (seq-some #'funcall (append binky-exclude-functions
                                              '(binky--exclude-mode-p
                                                binky--exclude-regexp-p)))
            (push (point-marker) result))))
      ;; delete marker duplicated with `binky-manual-alist'
      (setq result (seq-remove (lambda (m) (binky--duplicated-p m 0)) result))
      (setq binky-recent-alist
            (seq-mapn (lambda (x y) (cons x y))
                      marks
                      (if (equal binky-recent-sort-by 'recency)
                          (reverse result)
                        (seq-sort-by #'binky--frequency-get #'> result)))))
    (run-hooks 'binky-recent-alist-update-hook)))

(defun binky--swap-out ()
  "Turn record from marker into list of properties when a buffer is killed."
  (let ((orig (copy-alist binky-manual-alist)))
    (dolist (record binky-manual-alist)
      (when-let* ((info (cdr record))
                  ((markerp info))
                  ((eq (marker-buffer info) (current-buffer))))
        (if (and (buffer-file-name)
                 (null binky-prune))
	        (setcdr record (cdr (binky--normalize record)))
          (delete record binky-manual-alist))))
    (unless (equal orig binky-manual-alist)
      (run-hooks 'binky-manual-alist-update-hook))))

(defun binky--swap-in ()
  "Turn record from list of infos into marker when a buffer is reopened."
  (let ((orig (copy-alist binky-manual-alist)))
    (dolist (record binky-manual-alist)
      (when-let* ((info (cdr record))
                  ((not (markerp info)))
                  ((equal (cadr info) buffer-file-name)))
        (setcdr record (set-marker (make-marker) (car (last info))))))
    (unless (equal orig binky-manual-alist)
      (run-hooks 'binky-manual-alist-update-hook))))

(defun binky--manual-alist-in-group (&optional name order)
  "Return alist of manual records which is in same buffer or file.
NAME is buffer or file name, if nil current buffer name is used.
ORDER is `<' or `>' to sort records by position, otherwise no sorting."
  (let ((filtered (seq-filter (lambda (x)
                                (equal (or name (buffer-name (current-buffer)))
                                       (binky--prop-get x 'name)))
                              binky-manual-alist)))
    (if (memq order '(< >))
        (seq-sort-by (lambda (x) (binky--prop-get x 'position)) order filtered)
      filtered)))

(defun binky--manual-alist-preview ()
  "Return manual alist for preview."
  (if binky-preview-in-groups
      (let (live killed same)
        (dolist (name (seq-uniq (mapcar (lambda (x) (binky--prop-get x 'name))
                                        binky-manual-alist)))
          (let ((group (binky--manual-alist-in-group name #'<)))
            (if (equal name (buffer-name binky-current-buffer))
                (setq same group)
              (if (get-buffer name)
                  (setq live (append live group))
                (setq killed (append killed group))))))
        (append same live killed))
    (reverse binky-manual-alist)))

(defun binky--preview-horizontal-p ()
  "Return non-nil if binky preview buffer in horizontally."
  (memq binky-preview-side '(left right)))

(defun binky--preview-column ()
  "Return alist of elements (COLUMN . WIDTH) to display preview."
  (seq-filter #'cdr
              (mapcar (lambda (f)
                        (let ((width (if (binky--preview-horizontal-p)
                                         (nth 2 f)
                                       (nth 1 f))))
  					      (cons (nth 0 f)
                                (ignore-errors
                                  (if (< 0 width 1)
                                      (ceiling (* width (frame-width)))
                                    width)))))
  				      binky-preview-column)))

(defun binky--preview-extract (alist)
  "Return truncated string with selected columns according to ALIST."
  (format "%s%s\n"
          (if (binky--preview-horizontal-p) "" "  ")
		  (mapconcat
		   (lambda (x)
			 (let* ((limit (cdr x))
					(str (alist-get (car x) alist))
                    (end (max limit (string-width (symbol-name (car x))))))
			   (if (zerop limit)
                   str
                 ;; FIXME align error if buffer name contain punctuation character "—"
                 ;; use such as `string-pixel-width'
                 (truncate-string-to-width str end nil ?\s binky-preview-ellipsis))))
		   (binky--preview-column) "  ")))

(defun binky--preview-propertize (record)
  "Return formatted string for RECORD in preview."
  (let ((killed (not (markerp (cdr record)))))
    (cons (cons 'mark (concat "  " (binky--mark-propertize
                                    (binky--prop-get record 'mark)
                                    nil killed)))
          (seq-mapn
           (lambda (x y)
	         (let ((column-face (intern (concat "binky-preview-"
                                                (symbol-name x))))
                   (cond-face (cond
                               (killed 'binky-preview-killed)
                               ((and (eq x 'name)
                                     (equal y (buffer-name binky-current-buffer)))
                                'binky-preview-name-same)
                               (t nil))))
               (cons x (if (or killed (facep column-face))
                           (propertize y 'face (or cond-face column-face)) y))))
           '(name line mode context)
           (list (binky--prop-get record 'name)
		         (number-to-string (binky--prop-get record 'line))
		         (string-remove-suffix "-mode"
                                       (symbol-name (binky--prop-get record 'mode)))
		         (string-trim (binky--prop-get record 'context)))))))

(defun binky--preview-header ()
  "Return formatted string of header for preview."
  (binky--preview-extract
   (mapcar (lambda (x)
             (cons (car x)
                   (propertize (symbol-name (car x)) 'face 'binky-preview-header)))
		   (binky--preview-column))))

(defun binky--preview (&optional action)
  "Toggle preview window on the side `binky-preview-side'.
If optional arg ACTION is `close', close preview, if it's `redisplay',
redisplay the preview.  If it's nil, toggle the preview."
  (if (or (eq action 'close)
          (and (null action)
               (get-buffer-window binky-preview-buffer)))
      (let* ((buf binky-preview-buffer)
             (win (get-buffer-window buf)))
        (and (window-live-p win) (delete-window win))
        (and (get-buffer buf) (kill-buffer buf)))
	(with-current-buffer-window
		binky-preview-buffer
		(cons 'display-buffer-in-side-window
			  `((side          . ,binky-preview-side)
                (window-height . fit-window-to-buffer)
                (window-width  . fit-window-to-buffer)))
        nil
      (let* ((total (mapcar #'binky--preview-propertize
                            (binky--aggregate 'preview)))
		     (back (and binky-back-record
					    (binky--preview-propertize binky-back-record)))
		     (dup (and back (rassoc (cdr back) (cdr total)))))
        (erase-buffer)
	    ;; insert header if non-nil
	    (when (and (seq-some #'integerp (mapcar #'cdr (binky--preview-column)))
			       binky-preview-show-header)
	      (insert (binky--preview-header)))
	    (when dup
	      (setf (cdar dup)
			    (concat (substring (cdar back) -1)
					    (substring (cdar dup) 1))))
        (dolist (record (if dup (cdr total) total))
          (insert (binky--preview-extract record))))
      (setq-local window-min-height 1)
      (setq-local fit-window-to-buffer-horizontally t)
      (setq-local cursor-in-non-selected-windows nil)
	  (setq-local mode-line-format nil)
	  (setq-local truncate-lines t)
      (setq-local buffer-read-only t))))

(defun binky--highlight (cmd)
  "Highlight the line CMD operated on in `binky-highlight-duration' seconds."
  (when (and (numberp binky-highlight-duration)
		     (> binky-highlight-duration 0))
    (let ((beg (line-beginning-position))
          (end (line-beginning-position 2)))
      (if binky--highlight-overlay
          (move-overlay binky--highlight-overlay beg end)
	    (setq binky--highlight-overlay (make-overlay beg end)))
	  (overlay-put binky--highlight-overlay
                   'face
                   (intern (concat "binky-highlight-" (symbol-name cmd)))))
    (sit-for binky-highlight-duration)
    (delete-overlay binky--highlight-overlay)))

(defun binky--mark-propertize (mark &optional replace-string killed)
  "Return propertized string of MARK character.
If REPLACE-STRING is non-nil, return it rather than MARK.  If KILLED is non-nil,
face `binky-preview-killed' is used instead."
  (propertize (or replace-string (single-key-description mark t)) 'face
              (if killed
                  'binky-preview-killed
                (intern (concat "binky-preview-mark-"
                                (symbol-name (binky--mark-type mark)))))))

(defun binky--mark-legal ()
  "Generate and return legal mark list for jumping."
  (or binky-mark-legal
      (setq binky-mark-legal
            (seq-remove
             (lambda (x) (memq x (list ?? ?\  binky-mark-quit nil)))
             (seq-uniq
              (cl-union (number-sequence ?a ?z)
                        (cons binky-mark-back binky-mark-recent)))))))

(defun binky--mark-manual ()
  "Generate and return legal mark list for manual."
  (or binky-mark-manual
      (setq binky-mark-manual
            (cl-set-difference (number-sequence ?a ?z)
                               (append (list binky-mark-quit binky-mark-back)
                                       binky-mark-recent)))))

(defun binky--mark-type (mark &optional refresh)
  "Return type of MARK and update `binky-current-type' if REFRESH is non-nil.
The `group' means to toggle whether records in groups and preview.
The `quit' means to quit the command and preview.
The `help' means to preview records if not exist.
The `back' means to jump back last position.
The `recent' means to jump to recent marked buffers.
The `manual' means to operate on records manually.
The `shift' means to delete existing mark.
The `crtl' means to view record without jumping."
  (let ((type (cond
               ((equal mark ?\ ) 'group)
               ((memq mark (cons binky-mark-quit '(?\C-\[ escape))) 'quit)
               ((memq mark help-event-list) 'help)
               ((equal mark binky-mark-back) 'back)
               ((memq mark binky-mark-recent) 'recent)
               ((memq mark (binky--mark-manual)) 'manual)
               ((memq (downcase mark) (binky--mark-manual)) 'shift)
               ((equal (binky--mark-prefix mark) "C") 'ctrl)
               ;; ((equal (binky--mark-prefix mark) "M") 'alt)
               (t 'illegal))))
    (and refresh (setq binky-current-type type))
    type))

(defun binky--mark-get (mark)
  "Return INFO if (MARK . INFO) found in records, or return nil."
  (alist-get mark (binky--aggregate 'sum)))

(defun binky--mark-prefix (mark)
  "Return prefix of MARK if exists.
Prefix would be \"C\" (ctrl) or \"M\" (alt)."
  (when-let* ((str (single-key-description mark t))
              ((string-match "\\(C\\|M\\)-\\(.\\)" str))
              ((memq (string-to-char (match-string 2 str)) (binky--mark-legal))))
    (match-string 1 str)))

(defun binky--mark-read (prompt &optional preview)
  "Read and return a MARK possibly with preview.
Prompt with the string PROMPT and  may display a window listing existing
records after `binky-preview-delay' seconds.  When PREVIEW is non-nil,
preview records at once.

If `help-char' (or a member of `help-event-list') is pressed, display preview
window regardless.  Press \\[keyboard-quit] to quit."
  (setq binky-current-buffer (current-buffer))
  (and preview (binky--preview 'redisplay))
  (let ((timer (when (and (numberp binky-preview-delay)
                          (null preview))
		         (run-with-timer binky-preview-delay nil
                                 (apply-partially #'binky--preview 'redisplay)))))
    (unwind-protect
        (progn
		  (while (memq (binky--mark-type (read-key prompt) 'refresh)
                       '(help group illegal))
            (cl-case binky-current-type
              (help (binky--preview))
              (group
               (progn
                 (setq binky-preview-in-groups (not binky-preview-in-groups))
                 (binky--preview 'redisplay)
                 (binky--message last-input-event 'toggle)))
              (illegal
               (progn
                 (binky--message last-input-event 'illegal)))))
		  (if (eq binky-current-type 'quit)
              (keyboard-quit)
            (downcase (string-to-char (nreverse (single-key-description
                                                 last-input-event t))))))
	  (and (timerp timer) (cancel-timer timer))
      (when (or (eq binky-current-type 'quit) (null preview))
        (binky--preview 'close)))))

(defun binky--mark-add (mark)
  "Add (MARK . MARKER) into records."
  (cond
   ((eq major-mode 'xwidget-webkit-mode)
    (message "%s is not allowed" major-mode))
   ((binky--duplicated-p (point-marker))
    (let ((record (binky--duplicated-p (point-marker))))
      (binky--highlight 'warn)
      (save-excursion
        (goto-char (cdr record))
        (binky--highlight 'warn))
      (binky--message (car record) 'exist)))
   ((not (eq (binky--mark-type mark) 'manual))
    (binky--message mark 'illegal))
   ((and (binky--mark-get mark) (not binky-overwrite))
    (binky--highlight 'warn)
    (binky--message mark 'exist))
   (t
    (binky--highlight 'add)
    (setf (alist-get mark binky-manual-alist) (point-marker))
    (run-hooks 'binky-manual-alist-update-hook)
    (and binky-overwrite
         (binky--message mark 'overwrite)))))

(defun binky--mark-delete (mark)
  "Delete (MARK . INFO) from `binky-manual-alist'."
  (let ((info (binky--mark-get mark)))
    (cond
     ((not (eq (binky--mark-type mark) 'manual))
      (binky--message mark 'illegal))
     ((null info)
      (binky--message mark 'non-exist))
     (t
      (when (markerp info)
        (save-excursion
          (with-current-buffer (marker-buffer info)
            (goto-char info)
            (binky--highlight 'delete))))
	  (setq binky-manual-alist (assoc-delete-all mark binky-manual-alist))
      (run-hooks 'binky-manual-alist-update-hook)))))

(defun binky--mark-jump (mark)
  "Jump to point according to (MARK . INFO) in records."
  (if-let ((info (binky--mark-get mark))
           (last (point-marker)))
	  (progn
        (if (markerp info)
            (progn
			  (switch-to-buffer (marker-buffer info))
			  (goto-char info))
		  (find-file (cadr info))
		  (goto-char (car (last info))))
        (binky--highlight 'jump)
        (when (and (characterp binky-mark-back)
                   (not (equal (binky--distance last (point-marker)) 0)))
          (setq binky-back-record (cons binky-mark-back last))
          (run-hooks 'binky-back-record-update-hook)))
    (binky--message mark 'non-exist)))

(defun binky--mark-view (mark)
  "View the point in other window according to MARK."
  (if-let ((info (binky--mark-get mark)))
      (progn
        (unless (markerp info) (find-file-noselect (cadr info)))
        (let* ((info (binky--mark-get mark))
               (buf (marker-buffer info))
               (pop-up-windows t))
          (save-selected-window
            (pop-to-buffer buf t 'norecord)
            (goto-char info)
            (binky--highlight 'view))))
    (binky--message mark 'non-exist)))

;;; Commands

;;;###autoload
(defun binky-add (mark)
  "Add the record in current point with MARK."
  (interactive (list (binky--mark-read "Add:")))
  (binky--mark-add mark))

;;;###autoload
(defun binky-delete (mark)
  "Delete the record MARK."
  (interactive (list (binky--mark-read "Delete:")))
  (binky--mark-delete mark))

;;;###autoload
(defun binky-jump (mark)
  "Jump to point of record MARK."
  (interactive (list (binky--mark-read "Jump:")))
  (binky--mark-jump mark))

;;;###autoload
(defun binky-view (mark)
  "View the point of record MARK in other window."
  (interactive (list (binky--mark-read "View:")))
  (binky--mark-view mark))

;;;###autoload
(defun binky-binky (mark &optional persist)
  "Add, delete or jump records with MARK in one command.

If MARK prefix is shift+, then call `binky-delete'.
If MARK prefix is ctrl+, then call `binky-view'.
If MARK prefix is nil and mark exists, then call `binky-jump'.
If MARK prefix is nil and mark doesn't exist, then call `binky-add'.

Interactively, PERSIST is the prefix argument.  With no prefix argument,
it works as same as single command.  With a prefix argument, repeating commands
until \\[keyboard-quit] pressed."
  (interactive (list (binky--mark-read
                      (propertize "Binky:" 'face
                                  (if current-prefix-arg
                                      'binky-preview-header
                                    'default))
                      current-prefix-arg)
                     current-prefix-arg))
  (cl-case binky-current-type
    (shift (binky--mark-delete mark))
    (ctrl (binky--mark-view mark))
    (t (if (binky--mark-get mark)
	       (binky--mark-jump mark)
         (if (eq binky-current-type 'manual)
             (binky--mark-add mark)
           (binky--message mark 'illegal)))))
  (when persist
    (binky--preview 'redisplay)
    (call-interactively #'binky-binky)))

;;;###autoload
(defun binky-recent-toggle ()
  "Toggle whether enable recent mark feature or not."
  (interactive)
  (if-let ((state (get 'binky-recent-toggle 'state)))
      (progn
        (setq binky-mark-recent state)
        (put 'binky-recent-toggle 'state nil))
    (put 'binky-recent-toggle 'state binky-mark-recent)
    (setq binky-mark-recent nil))
  (binky--auto-update))

;;;###autoload
(defun binky-next-in-buffer (&optional backward)
  "Jump to next manual record in current buffer if exists.
If BACKWARD is non-nil, jump to previous one."
  (interactive)
  (if-let* ((order (if backward #'> #'<))
            (sorted (binky--manual-alist-in-group nil order)))
      (if (and (equal (length sorted) 1)
               (equal (binky--distance (point-marker) (cdar sorted)) 0))
          (message "Point is on the only record in current buffer.")
        (binky--mark-jump
         (car (seq-find (lambda (x)
                          (funcall order (point) (binky--prop-get x 'position)))
                        sorted
                        (car sorted)))))
    (message "No records in current buffer.")))

;;;###autoload
(defun binky-previous-in-buffer ()
  "Jump to previous manual record in current buffer if exists."
  (interactive)
  (binky-next-in-buffer t))

;;;###autoload
(define-minor-mode binky-mode
  "Toggle rabbit-jumping style position changes.
This global minor mode allows you to jump easily between buffers
you used and marked position."
  :group 'binky
  :global t
  (let ((cmd (if binky-mode #'add-hook #'remove-hook)))
    (dolist (pair '((buffer-list-update-hook . binky--auto-update)
                    (kill-buffer-hook . binky--swap-out)
                    (find-file-hook . binky--swap-in)))
      (funcall cmd (car pair) (cdr pair))))
  (when (eq binky-recent-sort-by 'frequency)
    (if binky-mode
        (setq binky-frequency-timer
              (run-with-idle-timer binky-frequency-idle
			                       t #'binky--frequency-increase))
      (cancel-timer binky-frequency-timer)
      (setq binky-frequency-timer nil))))

(provide 'binky)
;;; binky.el ends here