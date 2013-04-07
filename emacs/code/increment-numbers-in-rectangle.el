(require 'gosu-assert "~/configuration/emacs/code/assert.el")
(require 'gosu-increment-numbers-in-region "~/configuration/emacs/code/increment-numbers-in-region.el")

(defun gosu/position-on-line (pos)
  (save-excursion
    (goto-char pos)
    (- pos (line-beginning-position))))

(defun gosu/increment-numbers-in-rectangle (start end arg)
  (interactive "r\np")
  (save-excursion
    (goto-char start)
    (let ((previous-line-number -1)
          (current-line-number (line-number-at-pos start))
          (end-line-number (line-number-at-pos end))
          (start-pos-on-line (gosu/position-on-line start))
          (end-pos-on-line (gosu/position-on-line end)))
      
      ;; Go on until we either move past the end line, or until we reach the last line of the
      ;; buffer, which will result in that we're still on the same line after callign forward-line 
      (while (and (<= current-line-number end-line-number) (< previous-line-number current-line-number))
        (gosu/increment-numbers-in-region (+ (line-beginning-position) start-pos-on-line)
                                          (+ (line-beginning-position) end-pos-on-line)
                                          arg)
        (setq previous-line-number current-line-number)
        (forward-line 1)
        (setq current-line-number (line-number-at-pos))))))

;; position-on-line should return number of chars from beginning of line
;; and should not touch the current position
(with-test-buffer
 "line 1
line 2"
 (goto-char 3)
 (assert-equal 2 (gosu/position-on-line 10)
 (assert-equal 3 (point))))

;; If no increment argument is given, 1 is used
(with-test-buffer 
 "var f1 = new Foo(11, 12);
var f2 = new Foo(13, 14);
var f3 = new Foo(15, 16);
var f4 = new Foo(17, 18);"
 (gosu/increment-numbers-in-rectangle 9 72 nil)
 (assert-equal 
  "var f1 = new Foo(12, 12);
var f2 = new Foo(14, 14);
var f3 = new Foo(16, 16);
var f4 = new Foo(17, 18);" (buffer-string)))

;; End condition works when end of region is end of buffer
(with-test-buffer 
 "line 1
line 2"
 (gosu/increment-numbers-in-rectangle 3 14 nil)
 (assert-equal 
  "line 2
line 3" (buffer-string)))

;; Increment can be specified through argument
(with-test-buffer 
 "line 1
line 2"
 (gosu/increment-numbers-in-rectangle 3 14 2)
 (assert-equal 
  "line 3
line 4" (buffer-string)))

(with-test-buffer 
 "1 2 3
4 5 6"
 (gosu/increment-numbers-in-rectangle 1 10 100)
 (assert-equal 
  "101 102 3
104 105 6" (buffer-string)))

