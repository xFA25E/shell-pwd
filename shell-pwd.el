;;; shell-pwd.el --- Dispaly shell working directory in buffer name.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author: Valeriy Litkovskyy <vlr.ltkvsk@protonmail.com>
;; Version: 0.1.0
;; Keywords: processes, terminals, unix
;; URL: https://github.com/xFA25E/shell-pwd
;; Package-Requires: ((emacs "25.1"))

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

;; You can customize how new buffer name is chosen by creating an :override
;; advice for shell-pwd-generate-buffer-name function.  This function should
;; take a directory string as an argumnt and return a string.  The resulting
;; string will be used as new buffer name.

;; There is also a shell-pwd-shell command, that calls M-x shell with shell-pwd
;; already set.  The purpose of this command is to replace M-x shell, because
;; maybe you don't want to enable shell-pwd in a project shell.

;;; Code:

(require 'subr-x)
(require 'tramp)
(require 'files)

(defvar-local shell-pwd--previous-directory ""
  "Variable used to check for directory change.")

(defun shell-pwd--first-letter (file-name)
  "Return `FILE-NAME's first letter.
If `FILE-NAME' starts with a point, return point and first letter.

.name -> .n | name -> n"
  (let ((limit (if (string-prefix-p "." file-name) 2 1)))
    (substring file-name 0 (min limit (length file-name)))))

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
            (thread-first (mapcar #'shell-pwd--first-letter
                                  (split-string to-shorten "/"))
              (string-join "/")
              (concat unchanged)))
        directory))))

(defun shell-pwd-generate-buffer-name (dir)
  "Generate new shell buffer name based on `DIR'.
If `DIR' is a remote directory, add tramp host and method to
generated name"
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
  (add-hook 'comint-input-filter-functions
            #'shell-pwd-directory-tracker
            t t))

;;;###autoload
(defun shell-pwd-shell (&optional directory)
  "`shell' replacement with shell-pwd set.
You this if you want to preserve `project-shell's behaviour.
With prefix argument start it in a `DIRECTORY'."
  (interactive
   (list (if current-prefix-arg
             (expand-file-name (read-directory-name "Default directory: "))
           default-directory)))

  (let* ((default-directory (or directory default-directory))
         (buffer-name (shell-pwd-generate-buffer-name default-directory))
         (unique-buffer-name (generate-new-buffer-name buffer-name)))
    (shell unique-buffer-name)
    (shell-pwd-enable)))

(provide 'shell-pwd)

;;; shell-pwd.el ends here
