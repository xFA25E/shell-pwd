;;; shell-pwd.el --- Dispaly shell working directory in buffer name -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author: Valeriy Litkovskyy <vlr.ltkvsk@protonmail.com>
;; Version: 0.2
;; Keywords: processes, terminals, unix
;; URL: https://github.com/xFA25E/shell-pwd
;; Package-Requires: ((emacs "26.1"))

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

;; This package can display shell working directory in a mode-line.  To enable
;; just add shell-pwd-enable to shell-mode-hook: (add-hook 'shell-mode-hook
;; 'shell-pwd-enable)

;; There are additional commands: shell-pwd-change-directory,
;; shell-pwd-list-buffers

;;; Code:

(defun shell-pwd-shorten-directory (directory)
  "Shorten DIRECTORY."
  (replace-regexp-in-string
   (rx (group (? ".") (not "/")) (* (not "/")) "/") "\\1/"
   (directory-file-name (abbreviate-file-name directory))))

(defun shell-pwd (dir)
  "Return propertized and shortened string based on DIR."
  (let* ((remote (file-remote-p dir))
         (localname (or (file-remote-p dir 'localname) dir))
         (shortened (shell-pwd-shorten-directory localname)))
    (propertize (concat " " remote shortened " ") 'face 'font-lock-doc-face)))

;;;###autoload
(defun shell-pwd-enable ()
  "Put this into the `shell-mode-hook'."
  (add-to-list 'mode-line-buffer-identification
               '(:eval (shell-pwd default-directory))))

(declare-function comint-show-maximum-output "comint")
(declare-function comint-delete-input "comint")
(declare-function comint-send-input "comint")

;;;###autoload
(defun shell-pwd-change-directory ()
  "Change directory in a shell, interactively."
  (interactive)
  (comint-show-maximum-output)
  (comint-delete-input)
  (let* ((read-dir (read-directory-name "Change directory: "))
         (dir (or (file-remote-p read-dir 'localname) read-dir)))
    (insert (concat "cd " (shell-quote-argument (expand-file-name dir))))
    (comint-send-input)
    (cd dir)))

(defun shell-pwd-quit-window ()
  "Equivalent to (quit-window t)."
  (interactive)
  (quit-window t))

(defun shell-pwd-ibuffer-visit-buffer ()
  "Kill ibuffer buffer after visiting a buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (call-interactively 'ibuffer-visit-buffer)
    (kill-buffer buffer)))

(declare-function ibuffer-update "ibuffer")
(declare-function ibuffer-auto-mode "ibuf-ext")

;;;###autoload
(defun shell-pwd-list-buffers (&optional other-window-p)
  "Open shell buffers in ibuffer.
`OTHER-WINDOW-P' is like in `ibuffer'."
  (interactive "P")
  (let ((buffer-name "*Shell buffers*"))
    (ibuffer other-window-p buffer-name `((mode . shell-mode)) nil nil
             '(("Shells" (name . "\\`\\*sh "))
               ("Async shell commands" (name . "\\`\\*Async Shell Command\\*")))
             '((mark " " (name 40 50 :left :elide) " " filename-and-process)))
    (with-current-buffer buffer-name
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map (current-local-map))
        (define-key map [remap quit-window] 'shell-pwd-quit-window)
        (define-key map [remap ibuffer-visit-buffer] 'shell-pwd-ibuffer-visit-buffer)
        (use-local-map map))

      (setq-local ibuffer-use-header-line nil)
      (ibuffer-auto-mode)
      (ibuffer-update nil t)
      (hl-line-mode t))))

(provide 'shell-pwd)

;;; shell-pwd.el ends here
