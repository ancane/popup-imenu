;;; popup-imenu.el --- imenu index popup

;; Author: Igor Shymko <igor.shimko@gmail.com>
;; Version: 0.1
;; Package-Requires: ((dash "2.9.0") (popup "0.5.3") (flx-ido "0.5"))
;; Keywords: popup, imenu
;; URL: https://github.com/ancane/popup-imenu.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Displays imenu index in a popup window. Fuzzy matching supported.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl))

(require 'dash)
(require 'popup)
(require 'imenu)
(require 'artist)
(require 'flx-ido)

(defvar popup-imenu-use-flx t
  "Turns on flx matching")

(defun popup-imenu--filter ()
  (if popup-imenu-use-flx
      'popup-imenu--flx-match
    'popup-isearch-filter-list))

(defun popup-imenu--flx-match (query items)
  (let ((flex-result (flx-flex-match query items)))
    (let* ((matches (cl-loop for item in flex-result
                             for string = (ido-name item)
                             for score = (flx-score string query flx-file-cache)
                             if score
                             collect (cons item score)
                             into matches
                             finally return matches)))
      (popup-imenu--flx-decorate
       (delete-consecutive-dups
        (sort matches
              (lambda (x y) (> (cadr x) (cadr y))))
        t)))))

(defun popup-imenu--flx-decorate (things)
  (if flx-ido-use-faces
      (let ((decorate-count (min ido-max-prospects
                                 (length things))))
        (nconc
         (cl-loop for thing in things
                  for i from 0 below decorate-count
                  collect (popup-imenu--propertize thing))
         (mapcar 'car (nthcdr decorate-count things))))
    (mapcar 'car things)))

(defun popup-imenu--propertize (thing)
  (let* ((item-value (popup-item-value (car thing)))
         (flx-propertized (flx-propertize (car thing) (cdr thing))))
    (popup-item-propertize flx-propertized 'value item-value)))

(defun popup-imenu--flatten-index (menu-index)
  (-mapcat
   (lambda (x)
     (if (imenu--subalist-p x)
         (mapcar (lambda (y) (cons (concat (car x) ":" (car y)) (cdr y)))
                 (popup-imenu--flatten-index (cdr x)))
       (list x)))
   menu-index))

;;;###autoload
(defun popup-imenu ()
  (interactive)
  (let* ((popup-list (popup-imenu--flatten-index (imenu--make-index-alist)))
         (menu-height (min 15 (length popup-list) (- (window-height) 4)))
         (popup-items popup-list)
         (line-number (save-excursion
                        (goto-char (window-start))
                        (line-number-at-pos)))
         (x (+ (/ (- fill-column
                     (apply 'max (mapcar (lambda (x) (length (car x))) popup-list)))
                  2)
               (window-hscroll)))
         (y (+ (- line-number 2)
               (/ (- (window-height) menu-height) 2)))
         (menu-pos (save-excursion
                     (artist-move-to-xy x y)
                     (point)))
         (selected (popup-menu*
                    (mapcar
                     (lambda (x) (popup-make-item (car x) :value x))
                     popup-list)
                    :point menu-pos
                    :height menu-height
                    :isearch t
                    :isearch-filter (popup-imenu--filter)
                    :scroll-bar nil
                    :margin-left 1
                    :margin-right 1
                    )))
    (goto-char (cdr selected))))

(provide 'popup-imenu)

;;; popup-imenu.el ends here
