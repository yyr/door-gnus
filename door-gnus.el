;;; door-gnus.el -- Hack to enter, bury, unbury gnus.

;; Copyright (C) 2013 Yagnesh Raghava Yakkala <http://yagnesh.org>

;; Author: Yagnesh Raghava Yakkala <hi@yagnesh.org>
;; URL: https://github.com/yyr/door-gnus
;; Maintainer: Yagnesh Raghava Yakkala <hi@yagnesh.org>
;; Version: 0.1
;; Created: Friday, February 15 2013
;; Keywords: gnus, hack

;; This file is NOT part of GNU Emacs.

;; door-gnus is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; door-gnus.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Enhanced version from http://www.emacswiki.org/emacs/SwitchToGnus

;;; Code:
(require 'regexp-opt)

(defvar door-gnus-buffer-list
  '("*Group*" "*BBDB*" "*Summary" "*mail" "*wide" "*Article" "*reply")
  "List of possible gnus buffers (desired ones at least)")

(defvar door-gnus-buffer-list-re (regexp-opt door-gnus-buffer-list))

(defvar door-gnus-bury-window-configuration nil
  "Window configuration which will be restored when burying gnus")

(defvar door-gnus-unbury-window-configuration nil
  "Window configuration which will be restored when unburying gnus")

(defun door-gnus ()
  "Switch between gnus and non-gnus buffers, preserving window configurations."
  (interactive)
  (let ((bufname (buffer-name)))
    (if (string-match door-gnus-buffer-list-re bufname)
        (door-gnus-bury)
      (if (get-buffer "*Group*")
          (door-gnus-unbury)
        (progn
          (setq door-gnus-bury-window-configuration
                (current-window-configuration))
          (delete-other-windows)
          (gnus))))))

(defun door-gnus-unbury ()
  "Bring gnus on top, restore if there is saved window
configuration `door-gnus-unbury-window-configuration' for gnus."
  (interactive)
  (setq door-gnus-bury-window-configuration (current-window-configuration))
  (if door-gnus-unbury-window-configuration
      (progn
        (set-window-configuration door-gnus-unbury-window-configuration)
        (setq door-gnus-unbury-window-configuration nil))
    (delete-other-windows)
    (switch-to-buffer "*Group*" nil t)))

(defun door-gnus-bury ()
  "Bring gnus on top, restore if there is saved window
configuration `door-gnus-bury-window-configuration' for gnus."
  (interactive)
  (setq door-gnus-unbury-window-configuration (current-window-configuration))
  (if door-gnus-unbury-window-configuration
      (progn
        (set-window-configuration door-gnus-unbury-window-configuration)
        (setq door-gnus-unbury-window-configuration nil))
    (delete-other-windows)))

(global-set-key (kbd "<f9> g") 'door-gnus)

;;; init-door-gnus.el ends here
