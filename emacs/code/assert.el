(provide 'gosu-assert)

(defmacro assert-equal (expected expression)
  "Prints message if the given expression does not evaluate to the expected value"
  (eval-when-compile
    (let ((actual expression))
      `(unless (equal ,expected ,actual)
         (message (format "FAIL: Expected (equal %S %S). Actual value: %S"
                          ,expected 
                          (quote ,expression)
                          ,actual))))))
