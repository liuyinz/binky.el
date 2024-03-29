;;; binky-margin.el --- Highlight binky records on margins -*- lexical-binding: t -*-

;; Copyright (C) 2023 liuyinz

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

;; This is a global mode, it enables `binky-mode' to use margin as highlight records,
;; type `M-x binky-margin-mode'.

;;; Code:

(require 'binky)

(defvar-local binky-margin-width-orig nil
  "Default margin width of user setting.")

(defgroup binky-margin nil
  "Highlight binky records on margins."
  :group 'binky)

(defcustom binky-margin-string "\x2691"
  "Which string to show as margin indicator.
If nil, mark character would be used instead.  Recommendation as follow:
\\x2590 => ▐, \\x2665 => ♥, \\x2630 => ☰, \\x2691 => ⚑, \\x221a => √, \\x229b => ⊛."
  :type '(choice string (const :tag "Use mark character" nil))
  :group 'binky-margin)

(defun binky-margin--spec (mark)
  "Return margin display string according to MARK if provided."
  (propertize " " 'display
              `((margin ,(intern (format "%s-margin" binky-indicator-side)))
                ,(binky--mark-propertize mark binky-margin-string))))

(defun binky-margin--local-update (&optional update)
  "Remove margin indicators in current buffer and update if UPDATE is non-nil."
  ;; delete all overlays in buffer
  (save-restriction
    (widen)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'binky) (delete-overlay ov))))
  (when update
    (dolist (record (binky--filter :buffer (current-buffer)
                                   (binky--records :indicator)))
      (let* ((pos (binky--prop record :position))
             (ov (make-overlay pos pos)))
        (overlay-put ov 'binky t)
        (overlay-put ov 'before-string
                     (binky-margin--spec (binky--prop record :mark)))))))

(defun binky-margin--update ()
  "Remove and update margin indicators in all marked buffers if needed."
  (dolist (buf (seq-filter #'binky--marked-p (buffer-list)))
    (with-current-buffer buf
      ;; delete overlays
      (binky-margin--local-update
       (and (bound-and-true-p binky-mode)
            (bound-and-true-p binky-margin-mode)
            (bound-and-true-p binky-margin-local-mode))))))

;;;###autoload
(defun binky-margin-turn-on-maybe ()
  "Turn on `binky-margin-local-mode' if available."
  (when (and (bound-and-true-p binky-mode)
             (bound-and-true-p binky-margin-mode))
    (binky-margin-local-mode 1)))

;;;###autoload
(define-minor-mode binky-margin-local-mode
  "Toggle displaying indicators on the margin locally.
You probably shouldn't use this function directly."
  :group 'binky-margin
  :lighter ""
  (let ((width-var (intern (format "%s-margin-width" binky-indicator-side))))
    (if binky-margin-local-mode
        (progn
          (setq-local binky-margin-width-orig width-var)
          (setq width-var 1))
      (setq width-var binky-margin-width-orig)
      (setq binky-margin-width-orig nil)))
  (dolist (win (get-buffer-window-list))
    (set-window-buffer win (current-buffer)))
  (binky-margin--local-update binky-margin-local-mode))

;;;###autoload
(define-global-minor-mode binky-margin-mode
  binky-margin-local-mode binky-margin-turn-on-maybe
  :group 'binky-margin
  (let ((cmd (if binky-margin-mode #'add-hook #'remove-hook)))
    (dolist (hook '(binky-mode-hook
                    binky-manual-records-update-hook
                    binky-recent-records-update-hook
                    binky-back-record-update-hook))
      (funcall cmd hook #'binky-margin--update))))

(provide 'binky-margin)
;;; binky-margin.el ends here
