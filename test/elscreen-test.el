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

(defmacro elscreen-test-with-mode (&rest body)
  (declare (indent 0) (debug t))
  `(let ((frame (selected-frame))
         frame-conf)
     (elscreen-mode 1)
     (setq frame-conf (cdr (assq frame elscreen-frame-confs)))
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
         (elscreen-mode -1)))))

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
  (elscreen-test-with-mode
    (should (equal (screen-history) '(0)))
    (elscreen-set-conf-list 'screen-history '(1 2 3))
    (should (equal (screen-history) '(1 2 3)))
    ;; Note
    (should-not (assq 'foo frame-conf))
    (elscreen-set-conf-list 'foo '(1 2 3))
    (should-not (assq 'foo frame-conf))))

(ert-deftest elscreen-test-set-screen-property ()
  (elscreen-test-with-mode
    (elscreen-set-screen-property 0 '(foo))
    (should (equal (screen-property 0) '(foo)))
    (elscreen-set-screen-property 9 '(bar))
    (should (equal (screen-property 0) '(foo)))
    (should (equal (screen-property 9) '(bar)))))

(ert-deftest elscreen-test-set-window-configuration ()
  (elscreen-test-with-mode
    (elscreen-set-window-configuration 0 '(foo))
    (should (equal (screen-property 0 'window-configuration) '(foo)))
    (elscreen-set-window-configuration 9 '(bar))
    (should (equal (screen-property 0 'window-configuration) '(foo)))
    (should (equal (screen-property 9 'window-configuration) '(bar)))))

(ert-deftest elscreen-test-set-screen-nickname ()
  (elscreen-test-with-mode
    (elscreen-set-screen-nickname 0 "foo")
    (should (equal (screen-property 0 'nickname) "foo"))
    (elscreen-set-screen-nickname 9 "bar")
    (should (equal (screen-property 0 'nickname) "foo"))
    (should (equal (screen-property 9 'nickname) "bar"))))

(when noninteractive
  (ert-deftest elscreen-test-get-screen-to-name-alist ()
    (elscreen-test-with-mode
     (elscreen-set-conf-list 'modified-inquirer nil)
     (should (elscreen-screen-modified-p 'elscreen-get-screen-to-name-alist))
     (should (equal (elscreen-get-screen-to-name-alist) '((0 . "*scratch*"))))
     (should (equal (screen-to-name-alist-cache) '((0 . "*scratch*")))))))

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
  (elscreen-test-with-mode
    (elscreen-set-screen-property 9 '(bar))
    (elscreen-delete-screen-property 0)
    (should-not (screen-property 0))
    (should (equal (screen-property 9) '(bar)))))

(ert-deftest elscreen-test-delete-screen-nickname ()
  (elscreen-test-with-mode
    (elscreen-set-screen-nickname 0 "foo")
    (elscreen-set-screen-nickname 9 "bar")
    (elscreen-delete-screen-nickname 0)
    (should-not (screen-property 0 'nickname))
    (should (equal (screen-property 9 'nickname) "bar"))))

(ert-deftest elscreen-test-screen-modified-p ()
  (elscreen-test-with-mode
    (elscreen-set-conf-list 'modified-inquirer nil)
    (should (elscreen-screen-modified-p 'foo))
    (should (memq 'foo (modified-inquirer)))
    (should-not (elscreen-screen-modified-p 'foo))
    (should (memq 'foo (modified-inquirer)))))

(ert-deftest elscreen-test-hooks-setup-and-teardown ()
  (elscreen-setup)
  (should (memq 'elscreen-make-frame-confs after-make-frame-functions))
  (should (memq 'elscreen-delete-frame-confs delete-frame-functions))
  (should (memq 'elscreen-mode-line-update elscreen-screen-update-hook))
  (should (memq 'elscreen-menu-bar-update elscreen-screen-update-hook))
  (should (memq 'elscreen-tab-update elscreen-screen-update-hook))
  (should (memq 'elscreen-mode-map-disable minibuffer-setup-hook))
  (should (memq 'elscreen-mode-map-enable minibuffer-exit-hook))
  (elscreen-teardown)
  (should-not (memq 'elscreen-make-frame-confs after-make-frame-functions))
  (should-not (memq 'elscreen-delete-frame-confs delete-frame-functions))
  (should-not (memq 'elscreen-mode-line-update elscreen-screen-update-hook))
  (should-not (memq 'elscreen-menu-bar-update elscreen-screen-update-hook))
  (should-not (memq 'elscreen-tab-update elscreen-screen-update-hook))
  (should-not (memq 'elscreen-mode-map-disable minibuffer-setup-hook))
  (should-not (memq 'elscreen-mode-map-enable minibuffer-exit-hook)))

(defun elscreen-test-screen-modified-setup-p (hook-or-function &optional mode)
  (let ((func (if (eq mode 'force)
                  'elscreen-notify-screen-modification-force
                'elscreen-notify-screen-modification)))
    (cond
     ((string-match "-\\(hooks?\\|functions\\)$"
                    (symbol-name hook-or-function))
      (memq func (symbol-value hook-or-function)))
     (t
      (string-match-p
       (symbol-name func)
       (format "%s" (ad-find-advice hook-or-function
                                    'around
                                    'elscreen-screen-modified-advice)))))))

(ert-deftest elscreen-test-screen-modified-hook-setup-and-teardown ()
  (cl-flet ((setupp (hook-or-function &optional mode)
                    (elscreen-test-screen-modified-setup-p
                     hook-or-function mode)))
    (elscreen-setup)
    (should (setupp 'recenter 'force))
    (should (setupp 'change-major-mode-hook 'force))
    (should (setupp 'other-window))
    (should (setupp 'window-configuration-change-hook))
    (should (setupp 'window-size-change-functions))
    (should (setupp 'handle-switch-frame 'force))
    (should (setupp 'delete-frame 'force))
    (should (setupp 'Info-find-node-2 'force))
    (elscreen-teardown)
    (should-not (setupp 'recenter 'force))
    (should-not (setupp 'change-major-mode-hook 'force))
    (should-not (setupp 'other-window))
    (should-not (setupp 'window-configuration-change-hook))
    (should-not (setupp 'window-size-change-functions))
    (should-not (setupp 'handle-switch-frame 'force))
    (should-not (setupp 'delete-frame 'force))
    (should-not (setupp 'Info-find-node-2 'force))))

(ert-deftest elscreen-test-menu-bar-setup-and-teardown ()
  (elscreen-setup)
  (should
   (lookup-key global-map [menu-bar elscreen]))
  (should
   (eq (lookup-key minibuffer-local-map [menu-bar elscreen])
       'undefined))
  (elscreen-teardown)
  (should-not
   (lookup-key global-map [menu-bar elscreen]))
  (should-not
   (lookup-key minibuffer-local-map [menu-bar elscreen])))

(ert-deftest elscreen-test-help-setup-and-teardown ()
  (elscreen-setup)
  (should (equal elscreen-help-symbol-list '(elscreen-help)))
  (elscreen-teardown)
  (should-not elscreen-help-symbol-list))

(ert-deftest elscreen-test-set-prefix-key ()
  (elscreen-test-with-mode
    (should (equal elscreen-prefix-key (kbd "C-z")))
    (should (eq (lookup-key elscreen-mode-map (kbd "C-z C-c"))
                'elscreen-create))
    (elscreen-set-prefix-key (kbd "C-!"))
    (should (equal elscreen-prefix-key (kbd "C-!")))
    (should (= (cl-loop for map in (current-minor-mode-maps)
                        count (eq (lookup-key map (kbd "C-! C-c"))
                                  'elscreen-create))
               1))
    (should (= (cl-loop for map in (current-minor-mode-maps)
                        count (eq (lookup-key map (kbd "C-z C-c"))
                                  'elscreen-create))
               0))))

(unless noninteractive
  (ert-deftest elscreen-test-mode-line-in-multi-frames ()
    (elscreen-mode 1)
    (let ((old-frame (selected-frame))
          (new-frame (make-frame)))
      (with-selected-frame new-frame
        (elscreen-create)
        (should (string-match-p "[1]" (format-mode-line mode-line-format))))
      (with-selected-frame old-frame
        (should (string-match-p "[0]" (format-mode-line mode-line-format))))
      (delete-frame new-frame))))

(unless noninteractive
  (ert-deftest elscreen-test-mode-line-in-multi-frames ()
    (elscreen-mode 1)
    (let ((old-frame (selected-frame))
          (new-frame (make-frame)))
      (with-selected-frame new-frame
        (elscreen-create)
        (should (string-match-p "[1]" (format-mode-line mode-line-format))))
      (with-selected-frame old-frame
        (should (string-match-p "[0]" (format-mode-line mode-line-format))))
      (delete-frame new-frame))))

(provide 'elscreen-test)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; elscreen-test.el ends here
