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
;; M-x shell-pwd-shell

;;; Code:

(require 'subr-x)
(require 'tramp)
(require 'files)

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
/home/user/some/file -> /h/u/some/file

It is abbreviated with `ABBREVIATE-FILE-NAME' first, so user
directory may change to tilde."
  (let ((directory (abbreviate-file-name directory)))
    (save-match-data
      (if (string-match (rx bos
                            (group (+? anything) "/")
                            (group (+ (not "/")) "/" (* (not "/")))
                            eos)
                        directory)
          (let ((to-shorten (match-string 1 directory))
                (unchanged  (match-string 2 directory)))
            (thread-first (mapcar #'shell-pwd--cut-file-name
                                  (split-string to-shorten "/"))
              (string-join "/")
              (concat unchanged)))
        directory))))

(defun shell-pwd-generate-buffer-name (dir)
  "Generate new shell buffer name based on `DIR'.
If `DIR' is a remote directory, add tramp host and method to generated name"
  (let ((name (or (file-remote-p dir 'localname) dir))
        (host (if-let ((host (file-remote-p dir 'host))) (concat host " ") ""))
        (user (if-let ((user (file-remote-p dir 'user))) (concat user "@") "")))
    (format "*sh %s%s%s*" user host (shell-pwd-shorten-directory name))))

;;;###autoload
(defun shell-pwd-directory-tracker (&rest _args)
  "The main function that tracks directory change.
Put this in `comint-input-filter-functions' after
`shell-directory-tracker'."
  (unless (string= default-directory shell-pwd--previous-directory)
    (rename-buffer (shell-pwd-generate-buffer-name default-directory) t)
    (setq shell-pwd--previous-directory default-directory)))

;;;###autoload
(defun shell-pwd-enable ()
  "Put this into the `shell-mode-hook'."
  (add-hook 'comint-input-filter-functions #'shell-pwd-directory-tracker t t))

;;;###autoload
(defun shell-pwd-shell (&optional directory)
  (interactive
   (list (if current-prefix-arg
             (expand-file-name (read-directory-name "Default directory: "))
           default-directory)))
  (shell (generate-new-buffer-name (shell-pwd-generate-buffer-name
                                    (or directory default-directory))))
  (shell-pwd-enable))

(provide 'shell-pwd)

;;; shell-pwd.el ends here
