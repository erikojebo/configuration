(defun increment-numbers-in-region (start end arg)
  (interactive "r\np")
  (goto-char start)
  (let ((increment (or arg 1)))
    (while 
        (re-search-forward "[0-9]+" end t)
      (replace-match (int-to-string (+ increment (string-to-int (match-string 0))))))))

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
