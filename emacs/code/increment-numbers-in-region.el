(defun increment-numbers-in-region (start end arg)
  (interactive "r\np")
  (goto-char start)
  (let ((increment (or arg 1)))
    (while 
        (re-search-forward "[0-9]+" end t)
      (let ((incremented-string (int-to-string (+ increment (string-to-int (match-string 0))))))
        ;; If incremented string and original match differ in length, update end point
        (setq end (+ end (- (length incremented-string) (length (match-string 0)))))
        (replace-match incremented-string)))))

;; If no increment argument is given, 1 is used
(with-test-buffer 
 "var f1 = new Foo(11, 12);
var f2 = new Foo(13, 14);
var f3 = new Foo(15, 16);
var f4 = new Foo(17, 18);"
 (increment-numbers-in-region 21 76 nil)
 (assert-equal 
  "var f1 = new Foo(11, 13);
var f3 = new Foo(14, 15);
var f4 = new Foo(16, 17);
var f4 = new Foo(17, 18);" (buffer-string)))

;; The increment can be specified explicitly using a prefix argument (c-u)
(with-test-buffer 
 "var f1 = new Foo(11, 12);
var f2 = new Foo(13, 14);
var f3 = new Foo(15, 16);
var f4 = new Foo(17, 18);"
 (increment-numbers-in-region 21 76 2)
 (assert-equal 
  "var f1 = new Foo(11, 14);
var f4 = new Foo(15, 16);
var f5 = new Foo(17, 18);
var f4 = new Foo(17, 18);" (buffer-string)))

;; Negative increments can be used
(with-test-buffer 
 "var f1 = new Foo(11, 12);
var f2 = new Foo(13, 14);
var f3 = new Foo(15, 16);
var f4 = new Foo(17, 18);"
 (increment-numbers-in-region 21 76 -1)
 (assert-equal 
  "var f1 = new Foo(11, 11);
var f1 = new Foo(12, 13);
var f2 = new Foo(14, 15);
var f4 = new Foo(17, 18);" (buffer-string)))

;; Make sure all numbers in the original region are incremented, even though
;; the length of the text in the region is increased by the incrementation
(with-test-buffer 
 "var f1 = new Foo(11, 12);
var f2 = new Foo(13, 14);
var f3 = new Foo(15, 16);
var f4 = new Foo(17, 18);"
 (increment-numbers-in-region 21 76 100)
 (assert-equal 
  "var f1 = new Foo(11, 112);
var f102 = new Foo(113, 114);
var f103 = new Foo(115, 116);
var f4 = new Foo(17, 18);" (buffer-string)))
