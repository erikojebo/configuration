(provide 'gosu-assert)

(defmacro assert-equal (expected expression &optional msg)
  "Prints message if the given expression does not evaluate to the expected value"
  (eval-when-compile
    (let ((actual expression))
      `(let ((passed (equal ,expected ,actual)))
         (if passed
             (message ".")
           (progn
             (message (format "FAIL: %sExpected (equal %S %S). Actual value: %S"
                              (if ,msg
                                  (format "%s\n" ,msg)
                                "")
                              ,expected 
                              (quote ,expression)
                              ,actual))))
         passed))))

(defmacro with-test-buffer (content &rest body)
  "Creates a temporary buffer with the given contents, then executes 
the body and closes the buffer."
  `(let ((original-buffer (current-buffer))
        (buffer (generate-new-buffer "gosu-test-buffer")))
     (set-buffer buffer)
     (insert ,content)
     ,@body
     (set-buffer original-buffer)
     (kill-buffer buffer)
))
