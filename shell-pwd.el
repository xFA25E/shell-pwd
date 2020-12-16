;;; shell-pwd.el --- Dispaly shell working directory in buffer name.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author: xFA25E
;; Keywords: terminals, extensions

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

;; This package can display shell working directory in a buffer name.
;; To enable just add shell-pwd-enable to shell-mode-hook:
;; (add-hook 'shell-mode-hook 'shell-pwd-enable)

;; You can customize how new buffer name is chosen through
;; shell-pwd-generate-buffer-name-function.  Set to it your custom function.
;; This funciton should take a string as an argumnt and return a string.  The
;; resulting string will be used as new buffer name.

;; If you like the default name generating function, but you don't
;; want directory path shorten, set shell-pwd-shorten-directory to nil.

;; Use shell-pwd-switch-to-buffer to manage shell buffers.
;;; Code:

(require 'subr-x)
(require 'files)
(require 'tramp)

(defgroup shell-pwd nil
  "Display shell working directory in buffer name."
  :group 'extensions)

(defcustom shell-pwd-generate-buffer-name-function
  #'shell-pwd-generate-buffer-name
  "Function called when generating new buffer name.
Should accept one string argument and return a string.  Returned
string will be used as buffer name."
  :type 'function
  :group 'shell-pwd)

(defcustom shell-pwd-shorten-directory t
  "Whether default name generating function should shorten directory or not."
  :type 'boolean
  :group 'shell-pwd)

(defvar shell-pwd--previous-directory ""
  "Variable used to check for directory change.")
(make-variable-buffer-local 'shell-pwd--previous-directory)

(defun shell-pwd--cut-file-name (file-name)
  "Cut `FILE-NAME' to a length of 1 or 2.
.name -> .n
 name -> n"
  (substring file-name 0 (min (if (string-prefix-p "." file-name) 2 1)
                              (length file-name))))

(defun shell-pwd-shorten-directory (directory)
  "Shortens `DIRECTORY'.
/home/user/some/dir/ -> /h/u/s/dir/

If `DIRECTORY' does not end with a slash, it will be considered
as a file.
/home/user/some/file -> /h/u/some/file"
  (save-match-data
    (if (string-match (rx bos
                          (group (+? anything) "/")
                          (group (+ (not "/")) "/" (* (not "/")))
                          eos)
                      directory)
        (let ((to-shorten (match-string 1 directory))
              (unchanged (match-string 2 directory)))
          (thread-first (mapcar #'shell-pwd--cut-file-name
                                (split-string to-shorten "/"))
            (string-join "/")
            (concat unchanged)))
      directory)))

(defun shell-pwd-generate-buffer-name (dir)
  "Generate new shell buffer name based on `DIR'.
If `DIR' is a remote directory, add tramp host and method to generated name"
  (let ((name (or (file-remote-p dir 'localname) dir))
        (host (if-let ((host (file-remote-p dir 'host))) (concat host " ") ""))
        (user (if-let ((user (file-remote-p dir 'user))) (concat user "@") "")))
    (format "*sh %s%s%s*" user host
            (if shell-pwd-shorten-directory
                (shell-pwd-shorten-directory (abbreviate-file-name name))
              name))))

;;;###autoload
(defun shell-pwd-directory-tracker (&rest _args)
  "The main function that tracks directory change.
Put this in `comint-input-filter-functions' after
`shell-directory-tracker'."
  (if (not (string-equal default-directory
                         shell-pwd--previous-directory))
      (let ((name (funcall shell-pwd-generate-buffer-name-function
                           default-directory)))
        (rename-buffer name t))
    (setq shell-pwd--previous-directory default-directory)))

;;;###autoload
(defun shell-pwd-enable ()
  "Put this into the `shell-mode-hook'."
  (add-hook 'comint-input-filter-functions
            #'shell-pwd-directory-tracker
            t t))

;;;###autoload
(cl-defun shell-pwd-switch-to-buffer (&optional (directory default-directory))
  "Show a list of shell buffers and suggest to create a new one in `DIRECTORY'."
  (interactive
   (list (if current-prefix-arg
             (expand-file-name (read-directory-name "Default directory: "))
           default-directory)))
  (cl-flet ((shell-buffer-p (b) (eq (buffer-local-value 'major-mode b) 'shell-mode))
            (pwd-buffer () (shell-pwd-generate-buffer-name directory)))
    (let* ((buffer-name (generate-new-buffer-name (pwd-buffer)))
           (shell-buffers (mapcar #'buffer-name (cl-delete-if-not #'shell-buffer-p (buffer-list))))
           (name (completing-read "Shell buffer: " (cons buffer-name shell-buffers))))
      (if-let ((buffer (get-buffer name))) (pop-to-buffer buffer) (shell name))))
  (shell-pwd-enable))

(provide 'shell-pwd)

;;; shell-pwd.el ends here
