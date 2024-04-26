;;; binky.el --- Jump between points like a rabbit -*- lexical-binding: t -*-

;; Copyright (C) 2022, 2023, 2024 liuyinz

;; Author: liuyinz <liuyinz95@gmail.com>
;; Version: 1.4.2
;; Package-Requires: ((emacs "29.1"))
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
(require 'pulse)

(declare-function ffip-project-root "find-file-in-project")
(declare-function project-root "project")
(declare-function projectile-project-root "projectile")

;;; Customize

(defgroup binky nil
  "Jump between points like a rabbit."
  :prefix "binky-"
  :group 'convenience
  :link '(url-link :tag "Repository" "https://github.com/liuyinz/binky-mode"))

(defcustom binky-debug nil
  "Whether or not to output debug messages while in operation.
Messages are written into the *binky-debug* buffer."
  :type 'boolean)

(defcustom binky-command-prefix "C-c b"
  "The prefix for all `binky' commands."
  :package-version '(binky . "1.4.2")
  :type 'key
  :group 'binky)

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
  :type '(choice (repeat (choice
                          (character :tag "Printable character as mark")))
                 (const :tag "Disable recent marks" nil))
  :group 'binky)

(defcustom binky-recent-sort-by 'recency
  "Sorting strategy for recent marked records."
  :type '(choice (const :tag "Sort by recency" recency)
                 (Const :tag "Sort by frequency" frequency))
  :package-version '(binky . "1.2.0")
  :group 'binky)

(defcustom binky-project-detection 'auto
  "How to detect the project root of binky records.
nil means to use `default-directory'.
`auto' means to detect the following options in order."
  :type '(choice (const :tag "Auto-detect" auto)
                 (const :tag "Find File in Project" ffip)
                 (const :tag "Projectile" projectile)
                 (const :tag "Built-in Project" project)
                 (const :tag "Disable" nil))
  :package-version '(binky . "1.3.0")
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
  "List of regexps for buffer name included in `binky-recent-records'.
For example, buffer *scratch* is always included by default."
  :type '(repeat regexp)
  :package-version '(binky . "1.2.0")
  :group 'binky)

(defcustom binky-exclude-regexps
  '("\\`\\(\\s-\\|\\*\\).*\\'")
  "List of regexps for buffer name excluded from `binky-recent-records'.
When a buffer name matches any of the regexps, it would not be record
automatically unless it matches `binky-include-regexps'.  By default, all buffer
names start with '*' or ' ' are excluded."
  :type '(repeat regexp)
  :group 'binky)

(defcustom binky-exclude-modes
  '(xwidget-webkit-mode)
  "List of major modes which excluded from `binky-recent-records'."
  :type '(repeat symbol)
  :group 'binky)

(defcustom binky-exclude-functions
  (list #'minibufferp)
  "List of predicates which buffers satisfy exclude from `binky-recent-records'.
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
  '((mark    0.03  4)
    (name    0.14  15)
    (line    0.04  6)
    (project 0.14  nil)
    (mode    0.10  nil)
    (context 0     nil))
  "List of elements (COLUMN VERTICAL HORIZONTAL) to display in preview.
COLUMN is one of five properties of record below:

The `mark' is column to show mark.
The `name' is column to show buffer name.
The `line' is column to show line number.
The `mode' is column to show major mode.
The `project' is column to show belonging project.
The `context' is column to show content of line which record located.

VERTICAL and HORIZONTAL are width of the COLUMN depended on
`binky-preview-side'.  VERTICAL used for `top' and `bottom',
HORIZONTAL used for `left' and `right'.
If it's is nil, then COLUMN would not be displayed.
If it's 0, the COLUMN would not be truncated.
If it's a integer, the COLUMN width would be truncated to the upper limit.
If it's float number between 0 and 1, then upper limit is calculated based on
current frame width.
Usually, `context' column should be placed at the end and not truncated."
  :type '(alist
          :key-type symbol
          :options '(mark name line project mode context)
		  :value-type (group (choice number (const nil))
							 (choice number (const nil))))
  :package-version '(binky . "1.3.0")
  :group 'binky)

(defcustom binky-preview-ellipsis ".."
  "String used to abbreviate text in preview."
  :type 'string
  :group 'binky)

(defcustom binky-preview-order '(back manual recent)
  "Order which is applied to preview records."
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

(defcustom binky-hl-use-pulse t
  "If non-nil, pulse-style highlight would be applied when it's available.
Otherwise, use blink-style instead."
  :type 'boolean
  :package-version '(binky . "1.3.2")
  :group 'binky)

(defcustom binky-hl-duration 0.4
  "If non-nil, used as time in seconds to highlight the line record located.
If nil, disable the highlight feature."
  :type '(choice number (const :tag "Disable highlight" nil))
  :package-version '(binky . "1.3.2")
  :group 'binky)

(defcustom binky-indicator-side 'left
  "Which side to show indicator in window."
  :type '(choice (const left)
                 (const right))
  :package-version '(binky . "1.2.2")
  :group 'binky)

(defcustom binky-cache-directory (locate-user-emacs-file "binky/cache/")
  "Cache directory to store records of `binky-manual-list'."
  :type 'directory
  :package-version '(binky . "1.4.1")
  :group 'binky)

;; Faces

(defgroup binky-faces nil
  "The faces of `binky'."
  :group 'binky
  :group 'faces
  :package-version '(binky . "1.3.2"))

(defface binky-preview
  '((t (:italic nil :underline nil :inverse-video nil)))
  "Default face."
  :package-version '(binky . "1.3.2")
  :group 'binky-faces)

(defface binky-preview-mark-recent
  '((t (:inherit (binky-preview bold)
        :foreground "#e27e8d")))
  "Face used to highlight the recent mark of record in preview."
  :package-version '(binky . "1.3.2")
  :group 'binky-faces)

(defface binky-preview-mark-back
  '((t (:inherit (binky-preview bold)
        :foreground "#ebbf83")))
  "Face used to highlight the back mark of record in preview."
  :package-version '(binky . "1.3.2")
  :group 'binky-faces)

(defface binky-preview-mark-manual
  '((t (:inherit (binky-preview bold)
        :foreground "#5ec4ff")))
  "Face used to highlight the mark of record in preview."
  :package-version '(binky . "1.3.2")
  :group 'binky-faces)

(defface binky-preview-header
  '((t (:inherit (binky-preview bold)
        :underline t
        :foreground "#8bd49c")))
  "Face used to highlight the header in preview buffer."
  :package-version '(binky . "1.3.2")
  :group 'binky-faces)

(defface binky-preview-name
  '((t (:inherit (binky-preview default))))
  "Face used to highlight the name of record in preview."
  :package-version '(binky . "1.3.2")
  :group 'binky-faces)

(defface binky-preview-name-same
  '((t (:inherit binky-preview-name
        :underline t)))
  "Face used to highlight the name of record in same buffer in preview."
  :package-version '(binky . "1.3.2")
  :group 'binky-faces)

(defface binky-preview-line
  '((t (:inherit binky-preview-mark-manual
        :bold nil)))
  "Face used to highlight the line number of record in preview."
  :package-version '(binky . "1.3.2")
  :group 'binky-faces)

(defface binky-preview-mode
  '((t (:inherit binky-preview-mark-recent
        :bold nil)))
  "Face used to highlight the major mode of record in preview."
  :package-version '(binky . "1.3.2")
  :group 'binky-faces)

(defface binky-preview-project
  '((t (:inherit binky-preview-mark-back
        :bold nil)))
  "Face used to highlight the project directory of record in preview."
  :package-version '(binky . "1.3.2")
  :group 'binky-faces)

(defface binky-preview-killed
  '((t (:inherit (binky-preview font-lock-comment-face))))
  "Face used to highlight whole record of killed buffers in preview."
  :package-version '(binky . "1.3.2")
  :group 'binky-faces)

(defface binky-hl
  `((t (:foreground ,(face-foreground 'default)
        :extend t)))
  "Face used to highlight the line added to record."
  :package-version '(binky . "1.3.2")
  :group 'binky-faces)

(defface binky-hl-add
  `((t (:inherit binky-hl
        :background ,(face-foreground 'binky-preview-header))))
  "Face used to highlight the line added to record."
  :package-version '(binky . "1.3.2")
  :group 'binky-faces)

(defface binky-hl-delete
  `((t (:inherit binky-hl
        :background ,(face-foreground 'binky-preview-mark-recent))))
  "Face used to highlight the line deleted from record."
  :package-version '(binky . "1.3.2")
  :group 'binky-faces)

(defface binky-hl-view
  `((t (:inherit binky-hl
        :background ,(face-foreground 'binky-preview-mark-back))))
  "Face used to highlight the line viewed."
  :package-version '(binky . "1.3.2")
  :group 'binky-faces)

(defface binky-hl-jump
  `((t (:inherit binky-hl
        :background ,(face-foreground 'binky-preview-mark-manual))))
  "Face used to highlight the line jumped to."
  :package-version '(binky . "1.3.2")
  :group 'binky-faces)

(defface binky-hl-warn
  `((t (:inherit binky-hl
        :background ,(face-foreground 'binky-preview-mark-back))))
  "Face used to highlight the line already record."
  :package-version '(binky . "1.3.2")
  :group 'binky-faces)


;;; Variables

(defvar binky-manual-records nil
  "List of records (MARK . INFO) set and updated by manual.
MARK is a lowercase letter between a-z.  INFO is a marker or a form in style of
\(marker buffer name line project mode context file position) to store
record properties.")

(defvar binky-recent-records nil
  "List of records (MARK . MARKER), set and updated by recent buffers.")

(defvar binky-back-record nil
  "Record of last position before `binky-jump', set and updated automatically.")

(defvar binky-frequency-timer nil
  "Timer used to automatically increase buffer frequency.")

(defvar binky-frequency-idle 3
  "Number of seconds of idle time to wait before increasing frequency.")

(defvar-local binky-frequency 0
  "Frequency of current buffer.")

(defvar-local binky-marked nil
  "If non-nil, the buffer was once recorded by binky.")

(defvar binky-preview-buffer "*binky-preview*"
  "Buffer used to preview records.")

(defvar binky-manual-records-update-hook nil
  "Hook run when the variable `binky-manual-records' changes.")

(defvar binky-recent-records-update-hook nil
  "Hook run when the variable `binky-recent-records' changes.")

(defvar binky-back-record-update-hook nil
  "Hook run when the variable `binky-back-record' changes.")

(defvar-local binky-hl-overlay nil
  "Overlay used to highlight the line operated on.")

(defvar binky-current-buffer nil
  "Buffer where binky command called from.")

(defvar binky-current-type nil
  "Type of `last-input-event'.")

(defvar binky-mark-legal nil
  "List of legal marks used in all records.")

(defvar binky-mark-manual nil
  "List of legal marks used in manual records.")

(defvar binky-message-alist
  '((illegal   . "is illegal")
    (overwrite . "is overwritten")
    (exist     . "already exists")
    (duplicate . "has already record current place")
    (non-exist . "doesn't exist")
    (toggle    . "toggle groups view"))
  "Alist of binky status and messages.")

(defvar-local binky-project-root nil
  "Project path of current buffer located.")

(defvar binky-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'binky-add)
    (define-key map "d" 'binky-delete)
    (define-key map "j" 'binky-jump)
    (define-key map "w" 'binky-jump-other-window)
    (define-key map "v" 'binky-view)
    (define-key map "b" 'binky-binky)
    (define-key map "s" 'binky-save)
    (define-key map "r" 'binky-restore)
    (define-key map "t" 'binky-recent-toggle)
    (define-key map "n" 'binky-next-in-buffer)
    (define-key map "p" 'binky-previous-in-buffer)
    map))
(fset 'binky-command-map binky-command-map)


;;; Functions

(defmacro binky--check (&rest body)
  "Eval BODY forms only when binky mode is enabled."
  `(if (bound-and-true-p binky-mode)
       (progn ,@body)
     (user-error "Binky mode is not enabled yet")))

(defun binky--debug (msg &rest args)
  "Print infomations into *binky-debug* if `binky-debug' is non-nil.
MSG and ARGS format is as same as `format' command."
  (when binky-debug
    (with-current-buffer "*binky-debug*"
      (unless (derived-mode-p 'special-mode)
        (special-mode))
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (apply #'format msg args))
        (newline)))))

(defun binky--message (mark status &optional duration)
  "Echo information about MARK according to STATUS.
Wait for DURATION seconds and then redisplay."
  (message "Mark %s %s." (propertize (single-key-description mark t)
                                     'face
                                     'binky-preview-mark-manual)
           (alist-get status binky-message-alist))
  (sit-for (or duration 0.8) t))

(defun binky--marker (&optional position)
  "Return a marker at point or POSITION and record the buffer by binky.
Optional arg POSITION could be a marker or number."
  (setq-local binky-marked t)
  (copy-marker (or position (point))))

(defun binky--marked-p (buffer)
  "Return t if BUFFER was once marked by binky."
  (or (buffer-local-value 'binky-marked buffer) nil))

(defun binky-project-root ()
  "Get the path to the project root.
Return nil if no project was found."
  (with-memoization binky-project-root
    (and (buffer-file-name)
         (cond
          ((and (memq binky-project-detection '(auto ffip))
                (fboundp 'ffip-project-root))
           (let ((inhibit-message t))
             (ffip-project-root)))
          ((and (memq binky-project-detection '(auto projectile))
                (bound-and-true-p projectile-mode))
           (projectile-project-root))
          ((and (memq binky-project-detection '(auto project))
                (fboundp 'project-current))
           (when-let ((project (project-current)))
             (expand-file-name
              (if (fboundp 'project-root)
                  (project-root project)
                (car (with-no-warnings
                       (project-roots project)))))))))))

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
  (when (and (markerp x) (markerp y)
             (eq (marker-buffer x) (marker-buffer y)))
    (with-current-buffer (marker-buffer x)
      (abs (- (line-number-at-pos x 'absolute)
              (line-number-at-pos y 'absolute))))))

(defun binky--duplicated-p (marker &optional distance)
  "Return non-nil if MARKER equals with any record of `binky-manual-records'.
When the line MARKER at has no larger distance with DISTANCE, return that
record."
  (seq-find (lambda (x)
              (when-let ((info (binky--prop x :marker)))
                (ignore-errors
                  (<= (binky--distance marker info)
                      (or distance binky-distance)))))
            binky-manual-records))

(defun binky--parse (record)
  "Parse RECORD and return list of properties.
With format (mark marker buffer name line project mode context file position)."
  (if-let* ((mark (car record))
            (marker (and (markerp (cdr record)) (cdr record)))
            (position (marker-position marker))
            (buffer (marker-buffer marker)))
      (with-current-buffer buffer
        (list mark
              (buffer-name)
              (buffer-file-name)
              (file-name-nondirectory
               (directory-file-name (or (binky-project-root)
                                        default-directory)))
              (symbol-name major-mode)
              (line-number-at-pos position 'absolute)
              position
              (save-excursion
                (goto-char marker)
                (buffer-substring (line-beginning-position)
                                  (line-end-position)))
              (cl-case mark
                (binky-mark-back 'back)
                (binky-mark-recent 'recent)
                (t 'manual))
              marker
              buffer))
    record))

(defun binky--prop (record prop)
  "Return the PROP of RECORD, or nil if none."
  (let ((record (binky--parse record)))
    (cl-case prop
      (:mark     (nth 0 record))
      (:name     (nth 1 record))
      (:file     (nth 2 record))
      (:project  (nth 3 record))
      (:mode     (nth 4 record))
      (:line     (nth 5 record))
      (:position (nth 6 record))
      (:context  (nth 7 record))
      (:type     (nth 8 record))
      (:marker   (nth 9 record))
      (:buffer   (nth 10 record)))))

(defun binky--records (style)
  "Return selected records according to STYLE."
  (delq
   nil
   (cl-case style
     (:union
      ;; orderless, non-uniq
      (cons binky-back-record (append binky-manual-records
                                      binky-recent-records)))
     (:indicator
      ;; orderless, uniq
      (cons binky-back-record
            (mapcar (lambda (x)
                      (when-let ((marker (binky--prop x :marker)))
                        (eq marker (cdr binky-back-record))))
                    (append binky-manual-records binky-recent-records))))
     (:preview
      ;; order, uniq
      (seq-reduce #'append
                  (mapcar
                   (lambda (x)
                     (alist-get x `((back   . ,(list binky-back-record))
                                    (recent . ,binky-recent-records)
                                    (manual . ,(binky--manual-preview)))))
                   binky-preview-order)
                  nil)))))

(defun binky--filter (prop pred &optional alist)
  "Return records in ALIST filtered by PROP for which PRED return non-nil.
ALIST must be a binky records list, if nil use `binky-manual-records' by
default.  PRED should be a Lisp objects to be compared or a function of
one argument."
  (seq-filter (lambda (record)
                (apply (or (and (functionp pred) pred)
                           (if (consp pred) #'member #'equal))
                       (if (functionp pred)
                           `(,(binky--prop record prop))
                         `(,(binky--prop record prop) ,pred))))
              (or alist binky-manual-records)))

(defun binky--auto-update ()
  "Update `binky-recent-records' and `binky-back-record' automatically."
  ;; delete back-record if buffer not exists
  (when (and binky-back-record
             (null (marker-buffer (cdr binky-back-record))))
    (setq binky-back-record nil)
    (run-hooks 'binky-back-record-update-hook))
  ;; update recent marked records
  (let ((orig (copy-alist binky-recent-records)))
    (if-let ((marks (remove binky-mark-back binky-mark-recent)))
        ;; remove current-buffer and last buffer if current-buffer if minibuffer
        (cl-loop for buf in (nthcdr (if (minibufferp (current-buffer)) 2 1)
                                    (buffer-list))
                 if (with-current-buffer buf
                      (and (not (seq-some #'funcall
                                          (append binky-exclude-functions
                                                  '(binky--exclude-mode-p
                                                    binky--exclude-regexp-p))))
                           (binky--marker)))
                 collect it into result
                 finally do
                 (progn
                   ;; delete recent marker on the same line
                   (let ((uniq (seq-remove (lambda (m)
                                             (binky--duplicated-p m 0))
                                           result)))
                     (setq binky-recent-records
                           (seq-mapn
                            (lambda (x y) (cons x y))
                            marks
                            (if (eq binky-recent-sort-by 'recency)
                                uniq
                              (seq-sort-by #'binky--frequency-get
                                           #'>
                                           uniq)))))))
      (setq binky-recent-records nil))
    (unless (equal orig binky-recent-records)
      (run-hooks 'binky-recent-records-update-hook))))

(defun binky--swap-out ()
  "Turn record from marker into list of properties when a buffer is killed."
  (when-let ((to-swap (binky--filter :buffer (current-buffer))))
    (dolist (record to-swap)
      (if (and (buffer-file-name) (null binky-prune))
          (setcdr record (seq-subseq (binky--parse record) 1 9))
        (delete record binky-manual-records)))
    (run-hooks 'binky-manual-records-update-hook)))

(defun binky--swap-in ()
  "Turn record from list of infos into marker when a buffer is reopened."
  (when-let ((to-swap (binky--filter :file (buffer-file-name))))
    (dolist (record to-swap)
      (setcdr record (binky--marker (binky--prop record :position))))
    (run-hooks 'binky-manual-records-update-hook)))

(defun binky--manual-group (&optional name order)
  "Return alist of manual records in same buffer or file.
NAME is a buffer name, if nil current buffer name is used.
ORDER is `<' or `>' to sort records by position, otherwise no sorting."
  (let ((filtered (binky--filter :name (or name (buffer-name)))))
    (if (memq order '(< >))
        (seq-sort-by (lambda (x) (binky--prop x :position)) order filtered)
      filtered)))

(defun binky--manual-preview ()
  "Return manual alist for preview."
  (if binky-preview-in-groups
      (cl-loop for name in (seq-uniq (mapcar (lambda (x) (binky--prop x :name))
                                             binky-manual-records))
               for group = (binky--manual-group name #'<)
               if (not (get-buffer name))
               append group into killed
               else if (equal name (buffer-name binky-current-buffer))
               append group into same
               else append group into live
               finally return (append same live killed))
    (reverse binky-manual-records)))

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
                 ;; FIXME align error if buffer name contain punctuation
                 ;; character "—", use such as `string-pixel-width'
                 (truncate-string-to-width str end nil ?\s
                                           binky-preview-ellipsis))))
		   (binky--preview-column) "  ")))

(defun binky--preview-propertize (record)
  "Return formatted string for RECORD in preview."
  (let ((killed (not (binky--prop record :marker))))
    (cons (cons 'mark (concat "  " (binky--mark-propertize
                                    (binky--prop record :mark)
                                    nil killed)))
          (seq-mapn
           (lambda (x y)
	         (let ((column-face (intern (concat "binky-preview-"
                                                (symbol-name x)))))
               (cons x (if (or killed (facep column-face))
                           (propertize
                            y 'face
                            (cond (killed 'binky-preview-killed)
                                  ((and (eq x 'name)
                                        (equal y (buffer-name
                                                  binky-current-buffer)))
                                   'binky-preview-name-same)
                                  (t column-face))) y))))
           '(name line project mode context)
           (list (binky--prop record :name)
		         (number-to-string (binky--prop record :line))
                 (binky--prop record :project)
		         (string-remove-suffix "-mode" (binky--prop record :mode))
		         (string-trim (binky--prop record :context)))))))

(defun binky--preview-header ()
  "Return formatted string of header for preview."
  (binky--preview-extract
   (mapcar (lambda (x)
             (cons (car x) (propertize (symbol-name (car x))
                                       'face 'binky-preview-header)))
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
                            (binky--records :preview)))
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

(defun binky--highlight (type)
  "Highlight the current line with TYPE related face."
  (when (and (numberp binky-hl-duration)
		     (> binky-hl-duration 0))
    (let ((beg (line-beginning-position))
          (end (line-beginning-position 2))
          (face (intern (concat "binky-hl-" (symbol-name type)))))
      (if (overlayp binky-hl-overlay)
          (move-overlay binky-hl-overlay beg end)
	    (setq binky-hl-overlay (make-overlay beg end)))
      ;; NOTE only highlight line in selected window
	  (overlay-put binky-hl-overlay 'window (selected-window))
      (if (and binky-hl-use-pulse (pulse-available-p))
          (let* ((pulse-flag t)
                 (pulse-iterations 20)
                 (pulse-delay (/ binky-hl-duration pulse-iterations)))
            (overlay-put binky-hl-overlay 'pulse-delete t)
            ;; NOTE must set :background attribute directly in face, don't
            ;; use :inverse-video etc which don't take effect in pulse
            (pulse-momentary-highlight-overlay binky-hl-overlay face))
        (overlay-put binky-hl-overlay 'face face)
        (sit-for binky-hl-duration)
        (delete-overlay binky-hl-overlay)))))

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
  (with-memoization binky-mark-legal
    (seq-remove
     (lambda (x) (memq x (list ?? ?\  binky-mark-quit nil)))
     (seq-uniq
      (cl-union (number-sequence ?a ?z)
                (cons binky-mark-back binky-mark-recent))))))

(defun binky--mark-manual ()
  "Generate and return legal mark list for manual."
  (with-memoization binky-mark-manual
    (cl-set-difference (number-sequence ?a ?z)
                       (append (list binky-mark-quit binky-mark-back)
                               binky-mark-recent))))

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
               ((and (char-or-string-p mark)
                     (memq (downcase mark) (binky--mark-manual))) 'shift)
               ((equal (binky--mark-prefix mark) "C") 'ctrl)
               ((equal (binky--mark-prefix mark) "M") 'alt)
               (t 'illegal))))
    (and refresh (setq binky-current-type type))
    type))

(defun binky--mark-get (mark)
  "Return (MARK . INFO) if found in records, or return nil."
  (assoc mark (binky--records :union)))

(defun binky--mark-prefix (mark)
  "Return prefix of MARK if exists.
Prefix would be \"C\" (ctrl) or \"M\" (alt)."
  (when-let* ((str (single-key-description mark t))
              ((string-match "\\(C\\|M\\)-\\(.\\)" str))
              ((memq (string-to-char (match-string 2 str))
                     (binky--mark-legal))))
    (match-string 1 str)))

(defun binky--mark-read (prompt &optional preview)
  "Read and return a MARK possibly with preview.
Prompt with the string PROMPT and  may display a window listing existing
records after `binky-preview-delay' seconds.  When PREVIEW is non-nil,
preview records at once.

If `help-char' (or a member of `help-event-list') is pressed, display preview
window regardless.  Press \\[keyboard-quit] to quit."
  (binky--check
   (setq binky-current-buffer (current-buffer))
   (and preview (binky--preview 'redisplay))
   (let ((timer (when (and (numberp binky-preview-delay)
                           (null preview))
		          (run-with-timer binky-preview-delay nil
                                  (apply-partially #'binky--preview
                                                   'redisplay)))))
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
         (binky--preview 'close))))))

(defun binky--mark-add (mark)
  "Add (MARK . MARKER) into records."
  (cond
   ((eq major-mode 'xwidget-webkit-mode)
    (message "%s is not allowed" major-mode))
   ((binky--duplicated-p (point-marker))
    (cl-destructuring-bind (dup-mark . dup-pos)
        (binky--duplicated-p (point-marker))
      (save-excursion
        (goto-char dup-pos)
        (binky--highlight 'warn))
      (binky--message dup-mark 'duplicate)))
   ((not (eq (binky--mark-type mark) 'manual))
    (binky--message mark 'illegal))
   ((and (binky--mark-get mark) (not binky-overwrite))
    (save-excursion
      (goto-char (cdr (binky--mark-get mark)))
      (binky--highlight 'warn))
    (binky--message mark 'exist))
   (t
    (binky--highlight 'add)
    (when-let (orig (binky--mark-get mark))
      ;; BUG pulse could not flash twice in one command,
      ;; only show highlihgt overwritten line when it
      ;; do not use pulse style
      (unless binky-hl-use-pulse
        (save-excursion
          (goto-char (cdr orig))
          (binky--highlight 'delete)))
      (binky--message mark 'overwrite))
    (setf (alist-get mark binky-manual-records) (binky--marker))
    (run-hooks 'binky-manual-records-update-hook))))

(defun binky--mark-delete (mark)
  "Delete (MARK . INFO) from `binky-manual-records'."
  (unless (eq (binky--mark-type mark) 'manual)
    (binky--message mark 'illegal))
  (if-let ((record (binky--mark-get mark)))
      (progn
        (when (binky--prop record :marker)
          (save-excursion
            (with-current-buffer (binky--prop record :name)
              (goto-char (binky--prop record :position))
              (binky--highlight 'delete))))
	    (setq binky-manual-records (delq record binky-manual-records))
        (run-hooks 'binky-manual-records-update-hook))
    (binky--message mark 'non-exist)))

(defun binky--mark-jump (mark &optional other)
  "Jump to point according to (MARK . INFO) in records.
If optional arg OTHER is non-nil, jump to other window."
  (if-let ((record (binky--mark-get mark))
           (last (point-marker)))
      (progn
        (and other (switch-to-buffer-other-window (current-buffer)))
        (if (binky--prop record :marker)
            (switch-to-buffer (binky--prop record :name))
          (find-file (binky--prop record :file)))
        (goto-char (binky--prop record :position))
        (binky--highlight 'jump)
        (when (and (characterp binky-mark-back)
                   (not (equal (binky--distance last (point-marker)) 0)))
          (setq binky-back-record (cons binky-mark-back (binky--marker last)))
          (run-hooks 'binky-back-record-update-hook)))
    (binky--message mark 'non-exist)))

(defun binky--mark-view (mark)
  "View the point in other window according to MARK."
  (if-let* ((record (binky--mark-get mark)))
      (progn
        (unless (binky--prop record :marker)
          (find-file-noselect (binky--prop record :file)))
        (let ((pop-up-windows t))
          (save-selected-window
            (pop-to-buffer (binky--prop record :name) t 'norecord)
            (goto-char (binky--prop record :position))
            (binky--highlight 'view))))
    (binky--message mark 'non-exist)))


;;; Commands

;;;###autoload
(defun binky-add (mark)
  "Add the record in current point with MARK."
  (interactive (list (binky--mark-read "Binky add:")))
  (binky--mark-add mark))

;;;###autoload
(defun binky-delete (mark)
  "Delete the record MARK."
  (interactive (list (binky--mark-read "Binky delete:")))
  (binky--mark-delete mark))

;;;###autoload
(defun binky-jump (mark)
  "Jump to point of record MARK."
  (interactive (list (binky--mark-read "Binky jump:")))
  (binky--mark-jump mark))

;;;###autoload
(defun binky-jump-other-window (mark)
  "Jump to point of record MARK in other window."
  (interactive (list (binky--mark-read "Binky jump in other window:")))
  (binky--mark-jump mark t))

;;;###autoload
(defun binky-view (mark)
  "View the point of record MARK in other window."
  (interactive (list (binky--mark-read "Binky view:")))
  (binky--mark-view mark))

;;;###autoload
(defun binky-binky (mark &optional persist)
  "Add, delete or jump records with MARK in one command.

If MARK prefix is shift+, then call `binky-delete'.
If MARK prefix is ctrl+, then call `binky-view'.
If MARK prefix is alt+, then call `binky-jump-other-window'.
If MARK prefix is nil and mark exists, then call `binky-jump'.
If MARK prefix is nil and mark doesn't exist, then call `binky-add'.

Interactively, PERSIST is the prefix argument.  With no prefix argument,
it works as same as single command.  With a prefix argument, repeating commands
until \\[keyboard-quit] pressed."
  (interactive (list (binky--mark-read
                      (propertize "Binky binky:" 'face
                                  (if current-prefix-arg
                                      'binky-preview-header
                                    'default))
                      current-prefix-arg)
                     current-prefix-arg))
  (cl-case binky-current-type
    (shift (binky--mark-delete mark))
    (alt (binky--mark-jump mark t))
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
            (sorted (binky--manual-group nil order)))
      (if (and (equal (length sorted) 1)
               (equal (binky--distance (point-marker) (cdar sorted)) 0))
          (message "Point is on the only record in current buffer.")
        (binky--mark-jump
         (car (seq-find (lambda (x)
                          (funcall order (point) (binky--prop x :position)))
                        sorted
                        (car sorted)))))
    (message "No records in current buffer.")))

;;;###autoload
(defun binky-previous-in-buffer ()
  "Jump to previous manual record in current buffer if exists."
  (interactive)
  (binky-next-in-buffer t))

(defun binky-select-cache (file prompt &optional mustmatch)
  "Return binky cache FILE.
Prompting with PROMPT amd MUSTMATCH if called interactively, otherwise return
FILE or default cache."
  (if current-prefix-arg
      (read-file-name prompt binky-cache-directory nil mustmatch)
    (expand-file-name (or file "default.eld") binky-cache-directory)))

;;;###autoload
(defun binky-save (&optional file)
  "Save manual records informations to FILE.
If optional argument FILE is nil, choose default file instead."
  (interactive)
  (when-let ((output (binky-select-cache file "[Binky] save records to: ")))
    (make-directory binky-cache-directory t)
    (with-temp-file output
      (let ((print-level nil)
            (print-length nil))
        (pp (mapcar (lambda (record)
                      (cons (car record)
                            (seq-subseq (binky--parse record) 1 9)))
                    (binky--filter :file #'stringp))
            (current-buffer))))))

;;;###autoload
(defun binky-restore (&optional file)
  "Restore manual records informations from FILE.
This command will overwrite `binky-manual-records' by force."
  (interactive)
  (when-let* ((input (binky-select-cache file "[Binky] read records from: " t))
              ((file-exists-p input)))
    (with-temp-buffer
      (insert-file-contents input)
      (cl-loop for record in (read (current-buffer))
               collect
               (if-let ((buf (get-file-buffer (binky--prop record :file))))
                   (with-current-buffer buf
                     (cons (car record)
                           (binky--marker (binky--prop record :position))))
                 record)
               into result
               finally do
               (setq binky-manual-records result)))))


;;; Minor mode

;;;###autoload
(define-minor-mode binky-mode
  "Toggle rabbit-jumping style position changes.
This global minor mode allows you to jump easily between buffers
you used and marked position."
  :group 'binky
  :global t
  :keymap `((,(kbd binky-command-prefix) . binky-command-map))
  (let ((cmd (if binky-mode #'add-hook #'remove-hook)))
    (cl-loop for (hook . func) in
               '((buffer-list-update-hook . binky--auto-update)
                 (kill-buffer-hook . binky--swap-out)
                 (find-file-hook . binky--swap-in))
               do (funcall cmd hook func)))
  (when (eq binky-recent-sort-by 'frequency)
    (if binky-mode
        (setq binky-frequency-timer
              (run-with-idle-timer binky-frequency-idle
			                       t #'binky--frequency-increase))
      (cancel-timer binky-frequency-timer)
      (setq binky-frequency-timer nil))))

(provide 'binky)
;;; binky.el ends here
