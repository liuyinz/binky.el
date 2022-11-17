;;; binky-mode.el --- Jump between points like a rabbit -*- lexical-binding: t -*-

;; Copyright (C) 2022 liuyinz

;; Author: liuyinz <liuyinz95@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27"))
;; Keywords: convenience
;; Homepage: https://github.com/liuyinz/binky-mode

;; This file is not a part of GNU Emacsl.

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

;; This package provides commands to jump between positions recorded in buffers and files.

;;; Code:

(require 'cl-extra)

;;; Customize

(defgroup binky nil
  "Jump between points like a rabbit."
  :group 'convenience)

(defcustom binky-auto-sort-by 'recent
  "Sorting strategy for automaticaaly jump."
  :type '(choice (const :tag "Sort by recent" recent)
                 (cosnt :tag "Sort by frequent" frequent)
                 (const :tag "Sort by frecent" frecent)
                 (const :tag "Sort by duration" duration))
  :group 'binky)

(defcustom binky-auto-marks
  ;; '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0)
  '(?' ?1 ?2 ?3 ?4 ?5)
  "Default marks for jumping between recent visited buffers.
Any mark is either a character representing a self-inserting
key (letters, digits, punctuation, etc.) or a symbol denoting a
non-printing key like an arrow key (left, right, up, down)."
  :type '(repeat :tag "Marks" (choice (character :tag "char")))
  :group 'binky)

(defcustom binky-auto-exclude-functions
  '()
  "Don't mark buffers automatically for which any of these funcions return non-nil."
  :type '(repeat function)
  :group 'binky)

(defcustom binky-auto-exclude-modes
  '()
  "Don't mark in these modes."
  :type '(repeat symbol)
  :group 'binky)

(defcustom binky-auto-exclude-regexps
  '("\\` \\*.*\\'"
    "\\`\\*\\(Messages\\|Backtrace\\|Completions\\|Pp Eval Output\\|Buffer List\\)\\*\\'"
    "\\`\\*\\(Async-native-compile-log\\|lsp-bridge.*\\|helpful.*\\)\\*\\'"
    "\\`\\(magit.*\\)\\'"
    )
  "docstring"
  :type '(repeat string)
  :group 'binky)

(defcustom binky-ellipsis (if (char-displayable-p ?…) "…" "...")
  "Ellipsis string used to truncate strings."
  :type 'string
  :group 'binky)

(defcustom binky-preview-delay 1
  "If non-nil, time to wait in seconds before popping up a preview window.
If nil, do not show previews, unless `help-char' is pressed."
  :type '(choice number (const :tag "No preview unless requested" nil))
  :group 'binky)

(defcustom binky-jump-flash-duration 0.3
  "Highlight the line of marker jumped to with duration of time in seconds.
If it is 0, then disable the highlight feature."
  :type 'number
  :group 'binky)

(defcustom binky-overwrite-marks nil
  "If non-nil, overwrite record when set the exsiting key."
  :type 'boolean
  :group 'binky)

(defface binky-jump-flash
  '((t :inherit highlight :extend t))
  "Default face for highlighting the current line when jump to selected mark."
  :group 'binky)

;;; Variables

(defvar binky-alist nil
  "Alist of points vars.")

(defvar binky-auto-alist nil
  "Alist of buffers automatically generated.")

(defvar binky--auto-freq-count-timer nil
  "Timer used for count buffer used frequency.")

(defvar-local binky--auto-freq nil
  "Frequency of current buffer.")

(defvar-local binky--jump-overlay nil
  "The overlay used for highlighting the line jumped to.")

;;; Functions

;; binky auto marks related

(defun binky--auto-exclude-mode-p ()
  "docstring"
  (and binky-auto-exclude-modes
       (memq mode-name binky-auto-exclude-modes)))

(defun binky--auto-exclude-regexp-p ()
  "docstring"
  (let ((filter (mapconcat (lambda (x) (concat "\\(?:" x "\\)"))
                           binky-auto-exclude-regexps "\\|")))
    (and binky-auto-exclude-regexps
         (string-match-p filter (buffer-name)))))

(defun binky--auto-sort ()
  )

(defun binky-auto-update ()
  "docstring"
  (let ((len (length binky-auto-marks))
        (result (list))
        (filters (append binky-auto-exclude-functions
                         '(binky--auto-exclude-mode-p
                           binky--auto-exclude-regexp-p
                           minibufferp))))
    (unless (zerop len)
      (cl-dolist (buf (nthcdr 1 (buffer-list)))
        (and (equal len (length result)) (cl-return))
        (with-current-buffer buf
          (unless (cl-some #'funcall filters)
            (push (point-marker) result))))
      (nreverse result)
      ;; (cl-case binky-auto-sort-by
      ;;   (recent
      ;;    ())
      ;;   (frequent
      ;;    ())
      ;;   (frecent
      ;;    ())
      ;;   (duration
      ;;    ())
      ;;   (t nil))
      (setq binky-auto-alist
            (cl-mapcar (lambda (x y) (cons x y))
                       binky-auto-marks
                       result)))))

(defun binky-alist-swap-out ()
  "Turn markers into file references when a buffer is killed."
  (dolist (elem binky-alist)
    (let ((info (cdr elem)))
	  (when (and (markerp info)
	             (eq (marker-buffer info) (current-buffer)))
        (if buffer-file-name
	        (setcdr elem
		            (list buffer-file-name
                          (marker-position info)
                          major-mode
                          (string-trim (save-excursion
                                         (goto-char info)
                                         (buffer-substring (pos-bol) (pos-eol))))))
          (delete elem binky-alist))))))

(defun binky-alist-swap-in ()
  "docstring"
  (dolist (elem binky-alist)
    (let ((info (cdr elem)))
      (when (and (not (markerp info))
                 (equal (car info) buffer-file-name))
        (setcdr elem (set-marker (make-marker) (cadr info)))))))

(defun binky--jump-flash (beg end)
  "docstring"
  (if binky--jump-overlay
      (move-overlay binky--jump-overlay beg end)
    (setq binky--jump-overlay (make-overlay beg end))
    ;; (overlay-put binky--jump-overlay 'priority 1000)
    (overlay-put binky--jump-overlay 'face 'binky-jump-flash))
  (sit-for binky-jump-flash-duration)
  (delete-overlay binky--jump-overlay))

(defun binky--mark-type (&optional event)
  "Return type of EVENT."
  (let ((mark (or event last-input-event)))
    (cond
     ((memq mark '(?\C-g ?\C-\[ escape)) 'quit)
     ((memq mark (cons help-char help-event-list)) 'help)
     ((memq mark binky-auto-marks) 'auto)
     ((<= 33 mark 127) 'mannual)
     (t nil))))

(defun binky--mark-exist (mark)
  "docstring"
  (or (alist-get mark binky-auto-alist)
      (alist-get mark binky-alist)))

(defun binky--mark-read (prompt)
  "docstring"
  (unwind-protect
      (progn
        (while (eq (binky--mark-type
                    (read-key (propertize prompt 'face 'minibuffer-prompt)))
                   'help)
          (message "%s is not allowd." last-input-event))
        (when (eq  (binky--mark-type) 'quit)
          (keyboard-quit))
        (if (memq (binky--mark-type) '(auto mannual))
            last-input-event
          (error "Non-character input-event")))))

(defun binky--mark-get (mark)
  "docstring"
  (assoc mark binky-alist))

(defun binky--mark-set (mark)
  "docstring"
  (if (eq (binky--mark-type mark) 'mannual)
      (push (cons mark (point-marker)) binky-alist)
    (error "%s is not allowed.")))

(defun binky--mark-unset (mark)
  "docstring"
  (if (and (binky--mark-exist mark)
           (eq (binky--mark-type mark) 'mannual))
      (setq binky-alist (assoc-delete-all mark binky-alist))
    (error "%s is not allowed.")))

(defun binky--mark-jump (mark)
  "docstring"
  (if-let ((info (binky--mark-exist mark)))
      (progn
        (if (markerp info)
            (progn
              (switch-to-buffer (marker-buffer info))
              (goto-char info))
          (find-file (car info)))
        (when (> binky-jump-flash-duration 0)
          (binky--jump-flash (line-beginning-position) (line-beginning-position 2))))
    (error "no marks %s" mark)))

;; (defun binky--preview-format (elem)
;;   "docstring"
;;   )

;; (defun binky--preview (buffer)
;;   "docstring"
;;   (when (consp binky-alist)
;;     (with-current-buffer-window
;;         buffer
;;         (cons 'display-buffer-below-selected
;;               '((window-height . fit-window-to-buffer)
;;                 (preserve-size . (nil .t))))
;;         nil
;;       (with-current-buffer standard-output
;;         (setq cursor-in-non-selected-windows nil)
;;         (mapc (lambda (elem)
;;                 (when (alist-get (car elem) binky-alist)
;;                   (insert (binky--preview-format elem))))
;;               binky-alist)))))
;;
;; (defun binky--read-with-preview (prompt)
;;   "Read and return a register name, possibly showing existing registers.
;; Prompt with the string PROMPT.  If `register-alist' and
;; `register-preview-delay' are both non-nil, display a window
;; listing existing registers after `register-preview-delay' seconds.
;; If `help-char' (or a member of `help-event-list') is pressed,
;; display such a window regardless."
;;   (let* ((buffer "Binky Preview*")
;; 	     (timer (when (numberp binky-preview-delay)
;; 		          (run-with-timer binky-preview-delay nil
;; 				                  (lambda ()
;; 				                    (unless (get-buffer-window buffer)
;; 				                      (binky--preview buffer)))))))
;;     (unwind-protect
;; 	    (progn
;; 	      (while (binky--help-p (read-key (propertize prompt 'face 'minibuffer-prompt)))
;; 	        (unless (get-buffer-window buffer)
;; 	          (binky--preview buffer)))
;;           (when (or (eq ?\C-g last-input-event)
;;                     (eq 'escape last-input-event)
;;                     (eq ?\C-\[ last-input-event))
;;             (keyboard-quit))
;; 	      (if (characterp last-input-event) last-input-event
;; 	        (error "Non-character input-event")))
;;       (and (timerp timer) (cancel-timer timer))
;;       (let ((w (get-buffer-window buffer)))
;;         (and (window-live-p w) (delete-window w)))
;;       (and (get-buffer buffer) (kill-buffer buffer)))))

;;; Commands

;;;###autoload
(defun binky-mark (mark)
  "docstring"
  (interactive (list (binky--mark-read "Set mark: ")))
  (binky--mark-set mark))

;;;###autoload
(defun binky-unmark (mark)
  "docstring"
  (interactive (list (binky--mark-read "Unset mark: ")))
  (binky--mark-unset mark))

;;;###autoload
(defun binky-jump (mark)
  "docstring"
  (interactive (list (binky--mark-read "Jump to mark: ")))
  (binky--mark-jump mark))

;; (defun binky-binky (mark)
;;   "docstring"
;;   (interactive)
;;   )

(define-minor-mode binky-mode
  "Toggle `binky-mode'.
This global minor mode allows you to easily jump between buffers
you used ever."
  :group 'binky
  :global t
  :require 'binky-mode
  (if binky-mode
      (progn
        (add-hook 'buffer-list-update-hook 'binky-auto-update)
        (add-hook 'kill-buffer-hook 'binky-alist-swap-out)
        (add-hook 'find-file-hook 'binky-alist-swap-in))
    (remove-hook 'buffer-list-update-hook 'binky-auto-update)
    (remove-hook 'kill-buffer-hook 'binky-alist-swap-out)
    (remove-hook 'find-file-hook 'binky-alist-swap-in)))


(provide 'binky-mode)
;;; binky-mode.el ends here
