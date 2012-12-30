(defun increment-numbers-in-region (start end)
  (interactive "r")
  (goto-char start)
  (while 
      (re-search-forward "[0-9]+" end t)
    (replace-match (int-to-string (1+ (string-to-int (match-string 0)))))))


(with-test-buffer 
 "var f1 = new Foo(11, 12);
var f2 = new Foo(13, 14);
var f3 = new Foo(15, 16);
var f4 = new Foo(17, 18);"
                  (increment-numbers-in-region 21 76)
                  (assert-equal 
                   "var f1 = new Foo(11, 13);
var f3 = new Foo(14, 15);
var f4 = new Foo(16, 17);
var f4 = new Foo(17, 18);" (buffer-string)))
