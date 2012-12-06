;;;
;;; Nisse-Tisse Kalle
;;; Requires looking-back-from-current defined in functions.el
;;; Kalle är en grisbulle, sån tycker jaj.


(defun subset-of-line-p (start end)
  (let ((selection (buffer-substring start end)))
    (not (string-match-p "\n" selection))))

(defun re-search-backward-to-after-match (regexp)
  (re-search-backward regexp nil t) ;; suppress errors
  (goto-char (match-end 0)))

(defun looking-back-from-point-at-p (point regex)
  (save-excursion
    (goto-char point)
    (looking-back regex)))

(defun looking-from-point-at-p (point regex)
  (save-excursion
    (goto-char point)
    (looking-at-p regex)))

(defun gosu-expand-selection (&optional start end)
  (interactive "r")
  (let* ((whitespace-pattern "[ |	|
]")
         (non-whitespace-pattern "[a-zA-ZåäöÅÄÖ\\-]")
         (word-char-pattern "[a-zA-ZåäöÅÄÖ]")
         (non-word-char-pattern "[\-]")

         (selection-start (if (and start mark-active) start (point)))
         (selection-end (if (and end mark-active) end (point)))
         (selected-text (buffer-substring selection-start selection-end))

         (whitespace-before-p (looking-back-from-point-at-p selection-start whitespace-pattern))
         (whitespace-after-p (looking-from-point-at-p selection-end whitespace-pattern))

         (word-char-before-p (looking-back-from-point-at-p selection-start word-char-pattern))
         (word-char-after-p (looking-from-point-at-p selection-end word-char-pattern))
         (non-word-char-before-p (looking-back-from-point-at-p selection-start non-word-char-pattern))
         (non-word-char-after-p (looking-from-point-at-p selection-end non-word-char-pattern))

         forward-skip-pattern
         backward-skip-pattern
         forward-search-pattern
         backward-search-pattern

         new-selection-start
         new-selection-end)

    (progn
      (message "selection-start: %d, selection-end: %d" selection-start selection-end)
      (message "non-word-char before: %s" non-word-char-before-p)
      (message "non-word-char after: %s" non-word-char-after-p)
      (message "word-char before: %s" word-char-before-p)
      (message "word-char after: %s" word-char-after-p)
      (message "whitespace before: %s" whitespace-before-p)
      (message "whitespace after: %s" whitespace-after-p)
      (setq new-selection-start selection-start)
      (setq new-selection-end selection-end)
      (cond
       ((and word-char-before-p word-char-after-p)
        (setq backward-skip-pattern word-char-pattern)
        (setq forward-skip-pattern word-char-pattern))
       
       ((and word-char-before-p non-word-char-after-p)
        (setq backward-skip-pattern word-char-pattern))

       ((and non-word-char-before-p word-char-after-p)
        (setq forward-skip-pattern word-char-pattern))
       
       ((and whitespace-before-p (not whitespace-after-p))
        (setq forward-skip-pattern non-whitespace-pattern))

       ((and (not whitespace-before-p) whitespace-after-p)
        (setq backward-search-pattern whitespace-pattern))

       ((and whitespace-before-p whitespace-after-p
             (subset-of-line-p selection-start selection-end))
        (setq backward-search-pattern "\n")
        (setq forward-search-pattern "\n"))
       

       )

      (when backward-search-pattern
        (goto-char selection-start)
        (message "Searching backwards with pattern %s" backward-search-pattern)
        (re-search-backward-to-after-match backward-search-pattern)
        (setq new-selection-start (point)))
      (when forward-search-pattern
        (goto-char selection-end)
        (message "Searching forward with pattern %s" forward-search-pattern)
        (re-search-forward forward-search-pattern nil t) ;; suppress errors
        (setq new-selection-end (- (point) 1)))
      (when backward-skip-pattern
        (goto-char selection-start)
        (message "Skipping backwards with pattern %s" backward-skip-pattern)
        (skip-chars-backward backward-skip-pattern)
        (setq new-selection-start (point)))
      (when forward-skip-pattern
        (goto-char selection-end)
        (message "Skipping forwards with pattern %s" forward-skip-pattern)
        (skip-chars-forward forward-skip-pattern)
        (setq new-selection-end (point)))
      (goto-char new-selection-end)
      (set-mark new-selection-start))))

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
