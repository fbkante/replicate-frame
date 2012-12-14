;; Author: Koji Mitsuda <fbkante2u atmark gmail.com>
;; Keywords: convenience
;; Version: 1.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(eval-when-compile (require 'cl))

;;継承するframe-parameterの属性シンボルリスト
(defconst replicate-parameters '(name width height left top))
;;windowを複製した時に、新規ウィンドウと複写元ウィンドウを引数にして、呼び出される。
(defvar replicate-window-hook nil)

;;ウィンドウツリーノードからウィンドウサイズを取り出す
(defun window-treenode-edges (node)
  (if (windowp node)
      (window-edges node)
    (destructuring-bind (dir edges . windows) node edges)))

;;windowをnodeそっくりに複製する。popwin.el参照。
(defun replicate-window (new-window node)
  (if (windowp node)
      (progn
        (set-window-buffer new-window (window-buffer node))
        (set-window-point new-window (window-point node))
        (dolist (pair (window-parameters node))
          (set-window-parameter new-window (car pair) (cdr pair)))
        (set-window-prev-buffers new-window (window-prev-buffers node))
        (set-window-next-buffers new-window (window-next-buffers node))
        (run-hook-with-args 'replicate-window-hook new-window node))
    (destructuring-bind (dir edges . windows) node
      (loop while windows
            for sub-node = (pop windows)
            for win = new-window then next-win
            for size = (multiple-value-bind (left top right bottom) (window-treenode-edges sub-node)
                         (if dir (- bottom top) (- right left)))
            for next-win = (and windows (split-window win size (not dir)))
            do (replicate-window win sub-node)))))

;;frameを複製する
(defun replicate-frame ()
  (interactive)
  (let ((tree (window-tree))
        (fp (frame-parameters (selected-frame)))
        (param nil)
        (fr))
    (dolist (a replicate-parameters)
    	(let ((b (assq a fp)))
    		(when b (push (cons a (cdr b)) param))))
    (setq fr (make-frame param))
    (multiple-value-bind (root mini) tree (replicate-window (frame-selected-window fr) root))))

(provide 'replicate-frame)
