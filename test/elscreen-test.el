;;; elscreen-tests.el --- Tests for ElScreen         -*- lexical-binding: t; -*-

;; Copyright (C) 2014  papaeye

;; Author: papaeye <papaeye@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'elscreen)

(defadvice elscreen-message (before elscreen-no-sit activate)
  (ad-set-arg 1 0))

(defmacro elscreen-test-with-frame-conf (&rest body)
  (declare (indent 0) (debug t))
  `(let ((frame (selected-frame))
         frame-conf original-frame-conf)
     (elscreen-make-frame-confs frame)
     (setq frame-conf (cdr (assq frame elscreen-frame-confs)))
     (setq original-frame-conf (copy-tree frame-conf))
     (cl-labels ((getter (&rest keys)
                         (let ((value frame-conf))
                           (dolist (key keys)
                             (setq value (cdr (assq key value))))
                           value))
                 ,@(mapcar (lambda (key)
                             `(,key (&rest args)
                                    (apply #'getter (quote ,key) args)))
                           '(screen-property screen-history modified-inquirer
                                             screen-to-name-alist-cache)))
       (unwind-protect
           (progn ,@body)
         (setcdr (assq frame elscreen-frame-confs) original-frame-conf)))))

(ert-deftest elscreen-test-make-and-delete-frame-confs ()
  (setq elscreen-frame-confs nil)
  (let ((frame (selected-frame))
        frame-conf)
    (elscreen-make-frame-confs frame)
    (should (= (length elscreen-frame-confs) 1))
    (should (setq frame-conf (cdr (assq frame elscreen-frame-confs))))
    (should (assq 'screen-property frame-conf))
    (should (assq 'screen-history frame-conf))
    (should (assq 'modified-inquirer frame-conf))
    (should (assq 'screen-to-name-alist-cache frame-conf))
    (elscreen-delete-frame-confs frame)
    (should-not (assq frame elscreen-frame-confs))))

(ert-deftest elscreen-test-set-conf-list ()
  (elscreen-test-with-frame-conf
    (should (equal (screen-history) '(0)))
    (elscreen-set-conf-list 'screen-history '(1 2 3))
    (should (equal (screen-history) '(1 2 3)))
    ;; Note
    (should-not (assq 'foo frame-conf))
    (elscreen-set-conf-list 'foo '(1 2 3))
    (should-not (assq 'foo frame-conf))))

(ert-deftest elscreen-test-set-screen-property ()
  (elscreen-test-with-frame-conf
    (elscreen-set-screen-property 0 '(foo))
    (should (equal (screen-property 0) '(foo)))
    (elscreen-set-screen-property 9 '(bar))
    (should (equal (screen-property 0) '(foo)))
    (should (equal (screen-property 9) '(bar)))))

(ert-deftest elscreen-test-set-window-configuration ()
  (elscreen-test-with-frame-conf
    (elscreen-set-window-configuration 0 '(foo))
    (should (equal (screen-property 0 'window-configuration) '(foo)))
    (elscreen-set-window-configuration 9 '(bar))
    (should (equal (screen-property 0 'window-configuration) '(foo)))
    (should (equal (screen-property 9 'window-configuration) '(bar)))))

(ert-deftest elscreen-test-set-screen-nickname ()
  (elscreen-test-with-frame-conf
    (elscreen-set-screen-nickname 0 "foo")
    (should (equal (screen-property 0 'nickname) "foo"))
    (elscreen-set-screen-nickname 9 "bar")
    (should (equal (screen-property 0 'nickname) "foo"))
    (should (equal (screen-property 9 'nickname) "bar"))))

(ert-deftest elscreen-test-get-screen-to-name-alist ()
  (elscreen-test-with-frame-conf
    (elscreen-set-conf-list 'modified-inquirer nil)
    (should (elscreen-screen-modified-p 'elscreen-get-screen-to-name-alist))
    (should (equal (elscreen-get-screen-to-name-alist) '((0 . "*scratch*"))))
    (should (equal (screen-to-name-alist-cache) '((0 . "*scratch*"))))))

(ert-deftest elscreen-test-tab-create-keymap ()
  (cl-flet ((elscreen-test-def1 () ())
            (elscreen-test-def2 () ()))
    (let ((keymap (elscreen-tab-create-keymap
                   'mouse-1 #'elscreen-test-def1
                   'M-mouse-1 #'elscreen-test-def2)))
      (should (eq (lookup-key keymap [header-line mouse-1])
                  #'elscreen-test-def1))
      (should (eq (lookup-key keymap [header-line M-mouse-1])
                  #'elscreen-test-def2))
      (dolist (key '(mouse-2 mouse-3
                             down-mouse-1 down-mouse-2 down-mouse-3
                             drag-mouse-1 drag-mouse-2 drag-mouse-3))
        (should (eq (lookup-key keymap (vector 'header-line key)) 'ignore))))))

(ert-deftest elscreen-test-delete-screen-property ()
  (elscreen-test-with-frame-conf
    (elscreen-set-screen-property 9 '(bar))
    (elscreen-delete-screen-property 0)
    (should-not (screen-property 0))
    (should (equal (screen-property 9) '(bar)))))

(ert-deftest elscreen-test-delete-screen-nickname ()
  (elscreen-test-with-frame-conf
    (elscreen-set-screen-nickname 0 "foo")
    (elscreen-set-screen-nickname 9 "bar")
    (elscreen-delete-screen-nickname 0)
    (should-not (screen-property 0 'nickname))
    (should (equal (screen-property 9 'nickname) "bar"))))

(ert-deftest elscreen-test-screen-modified-p ()
  (elscreen-test-with-frame-conf
    (elscreen-set-conf-list 'modified-inquirer nil)
    (should (elscreen-screen-modified-p 'foo))
    (should (memq 'foo (modified-inquirer)))
    (should-not (elscreen-screen-modified-p 'foo))
    (should (memq 'foo (modified-inquirer)))))

(provide 'elscreen-test)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; elscreen-test.el ends here
