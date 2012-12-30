(require 'gosu-assert "~/configuration/code/assert.el")
(require 'gosu-text-helpers "~/configuration/code/text.el")
(require 'cl)

;;;
;;; Nisse-Tisse Kalle
;;; Requires looking-back-from-current defined in functions.el
;;; Kalle är en grisbulle, sån tycker jaj.
;; var a = function (a b c) {
;;     console.log("hej");
;; }

;; Del av ord hej
;; hel symbol (whitespace till whitespace) || inom matchande braces
;; hel rad || inom matchande braces
;; hel paragraf (blankrad/start av buffer till blankrad eller slut av buffer)
;; hela buffern


(defun gosu-expand-region (start end)
  (interactive "r")
  (let* ((region-start (if (and start mark-active) start (point)))
         (region-end (if (and end mark-active) end (point))))
    (cond 
     ((and (looking-back-from-point-at-p region-start "[\'\(\)]")
           (looking-from-point-at-p region-end "[\'\(\)]"))
;;      (message "found pairs")
      (set-mark (1- region-start))
      (goto-char (1+ region-end)))
     (t
;;      (message "Expanding to word bounds")
      (goto-char region-start)
      (backward-word)
      (set-mark (point))
      (goto-char region-end)
      (forward-word)))))

(defun foo ()
  (interactive)
  (message "Point: %s"(point))
  (message "%s" (char-to-string (char-after (point))))
  (message "%s" (looking-back-from-point-at-p (point) "\'")))


(assert-expanded-region "" :original (1 1) :expanded (1 1) :scenario "empty buffer")
(assert-expanded-region "abcd" :original (1 4) :expanded (1 5) :scenario "First and last char of word")
(assert-expanded-region "abcd" :original (1 1) :expanded (1 5) :scenario "Beginning of word")
(assert-expanded-region "abcd" :original (2 3) :expanded (1 5) :scenario "Middle of word")
(assert-expanded-region "ab cd" :original (1 1) :expanded (1 3) :scenario "First char of first word in sentence")
(assert-expanded-region "ab cd" :original (4 5) :expanded (4 6) :scenario "Inside second word in sentence")
(assert-expanded-region "åäöÅÄÖ" :original (2 3) :expanded (1 7) :scenario "Inside word with swedish chars")
(assert-expanded-region "abc def" :original (2 6) :expanded (1 8) :scenario "Inside words in sentence")
(assert-expanded-region "console.log('message')" :original (15 18) :expanded (14 21) :scenario "Inside word within single quotes")
(assert-expanded-region "console.log('message')" :original (14 21) :expanded (13 22) :scenario "Inside single quotes")
(assert-expanded-region "console.log('message')" :original (13 22) :expanded (12 23) :scenario "Inside parens")


(defmacro* assert-expanded-region (content &key (original '(1 1)) (expanded '(1 1)) (scenario nil))
  (let ((original-point (cadr original))
        (original-mark (car original))
        (expanded-point (cadr expanded))
        (expanded-mark (car expanded)))
    `(with-test-buffer ,content
                       (set-mark ,original-mark)
                       (goto-char ,original-point)
                       (gosu-expand-region ,original-mark ,original-point)
                       (assert-equal ,expanded-mark (mark) ,scenario)
                       (assert-equal ,expanded-point (point) ,scenario))))



;; (defun subset-of-line-p (start end)
;;   (let ((selection (buffer-substring start end)))
;;     (not (string-match-p "\n" selection))))

;; (defun re-search-backward-to-after-match (regexp)
;;   (re-search-backward regexp nil t) ;; suppress errors
;;   (goto-char (match-end 0)))

;; (defun gosu-expand-selection (&optional start end)
;;   (interactive "r")
;;   (let* ((whitespace-pattern "[ |	|
;; ]")
;;          (non-whitespace-pattern "[a-zA-ZåäöÅÄÖ\\-]")
;;          (word-char-pattern "[a-zA-ZåäöÅÄÖ]")
;;          (non-word-char-pattern "[\-]")

;;          (selection-start (if (and start mark-active) start (point)))
;;          (selection-end (if (and end mark-active) end (point)))
;;          (selected-text (buffer-substring selection-start selection-end))

;;          (whitespace-before-p (looking-back-from-point-at-p selection-start whitespace-pattern))
;;          (whitespace-after-p (looking-from-point-at-p selection-end whitespace-pattern))

;;          (word-char-before-p (looking-back-from-point-at-p selection-start word-char-pattern))
;;          (word-char-after-p (looking-from-point-at-p selection-end word-char-pattern))
;;          (non-word-char-before-p (looking-back-from-point-at-p selection-start non-word-char-pattern))
;;          (non-word-char-after-p (looking-from-point-at-p selection-end non-word-char-pattern))

;;          forward-skip-pattern
;;          backward-skip-pattern
;;          forward-search-pattern
;;          backward-search-pattern

;;          new-selection-start
;;          new-selection-end)

;;     (progn
;;       (message "selection-start: %d, selection-end: %d" selection-start selection-end)
;;       (message "non-word-char before: %s" non-word-char-before-p)
;;       (message "non-word-char after: %s" non-word-char-after-p)
;;       (message "word-char before: %s" word-char-before-p)
;;       (message "word-char after: %s" word-char-after-p)
;;       (message "whitespace before: %s" whitespace-before-p)
;;       (message "whitespace after: %s" whitespace-after-p)
;;       (setq new-selection-start selection-start)
;;       (setq new-selection-end selection-end)
;;       (cond
;;        ((and word-char-before-p word-char-after-p)
;;         (setq backward-skip-pattern word-char-pattern)
;;         (setq forward-skip-pattern word-char-pattern))
       
;;        ((and word-char-before-p non-word-char-after-p)
;;         (setq backward-skip-pattern word-char-pattern))

;;        ((and non-word-char-before-p word-char-after-p)
;;         (setq forward-skip-pattern word-char-pattern))
       
;;        ((and whitespace-before-p (not whitespace-after-p))
;;         (setq forward-skip-pattern non-whitespace-pattern))

;;        ((and (not whitespace-before-p) whitespace-after-p)
;;         (setq backward-search-pattern whitespace-pattern))

;;        ((and whitespace-before-p whitespace-after-p
;;              (subset-of-line-p selection-start selection-end))
;;         (setq backward-search-pattern "\n")
;;         (setq forward-search-pattern "\n"))
       

;;        )

;;       (when backward-search-pattern
;;         (goto-char selection-start)
;;         (message "Searching backwards with pattern %s" backward-search-pattern)
;;         (re-search-backward-to-after-match backward-search-pattern)
;;         (setq new-selection-start (point)))
;;       (when forward-search-pattern
;;         (goto-char selection-end)
;;         (message "Searching forward with pattern %s" forward-search-pattern)
;;         (re-search-forward forward-search-pattern nil t) ;; suppress errors
;;         (setq new-selection-end (- (point) 1)))
;;       (when backward-skip-pattern
;;         (goto-char selection-start)
;;         (message "Skipping backwards with pattern %s" backward-skip-pattern)
;;         (skip-chars-backward backward-skip-pattern)
;;         (setq new-selection-start (point)))
;;       (when forward-skip-pattern
;;         (goto-char selection-end)
;;         (message "Skipping forwards with pattern %s" forward-skip-pattern)
;;         (skip-chars-forward forward-skip-pattern)
;;         (setq new-selection-end (point)))
;;       (goto-char new-selection-end)
;;       (set-mark new-selection-start))))

;; Nisse-Kalle-Pelle kalle

;; K#alle => [Kalle]
;; Kalle-Svenson => [Kalle]-Svenson
;; [Kalle]-Svenson => [Kalle-Svenson]

;; <>(){}[]''""

;; Om man står intill en brace/quote så expandera selection till matchande brace/quote
;; Om man har selection och har stött på blank space åt ena hållet men inte andra
;;    expandera en enhet åt andra hållet

;; Hi, this is a sample sentence. And here is another.
;; function foo(hej, sven) { console.log("bulle är en kulle"); }
;; private void FooBar(int kalle, int bulle) { return kalle + bulle; }
;; (defun hello-world () (message "hello world"))
;; (+ (+ 1 2) (+ 3 4))
;; Selection-enheter:
;; {}, () etc
;; Meningar
;; Ord
;; Paragrafer

;; Hela buffern-
