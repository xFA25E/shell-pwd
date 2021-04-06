;;; shell-pwd-test.el --- Tests for shell-pwd       -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy <vlr.ltkvsk@protonmail.com>
;; Keywords:

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

;;; Code:

(require 'ert)
(require 'shell-pwd)

(ert-deftest shell-pwd-test-first-letter ()
  "Test first-letter function."
  (should (string= (shell-pwd--first-letter "hello world") "h"))
  (should (string= (shell-pwd--first-letter ".hello world") ".h"))
  (should (string= (shell-pwd--first-letter "") "")))

(ert-deftest shell-pwd-test-shorten-directory ()
  "Test shorten directory function"
  (should (string= (shell-pwd-shorten-directory "/hello/world/my/lady/")
                        "/h/w/m/lady/"))
  (should (string= (shell-pwd-shorten-directory "/hello/world/my/boi")
                        "/h/w/my/boi"))
  (should (string= (shell-pwd-shorten-directory (expand-file-name "some/dir/" (getenv "HOME")))
                        "~/s/dir/"))
  (should (string= (shell-pwd-shorten-directory "/dir/.hidden/.other/hidden/")
                        "/d/.h/.o/hidden/")))

(ert-deftest shell-pwd-test-generate-buffer-name ()
  "Test buffer name generation"
  (should (string= (shell-pwd-generate-buffer-name "/some/dir/very/deep/")
                        "*sh /s/d/v/deep/*"))
  (should (string= (shell-pwd-generate-buffer-name "/ssh:user@host:/another/deep/dir/")
                        "*sh user@host /a/d/dir/*"))
  (should (string= (shell-pwd-generate-buffer-name "/ssh:user@host:relative/deep/dir/")
                        "*sh user@host r/d/dir/*"))
  (should (string= (shell-pwd-generate-buffer-name "/ssh:host:/dir/my/what/")
                        "*sh host /d/m/what/*")))

(provide 'shell-pwd-test)
;;; shell-pwd-test.el ends here
