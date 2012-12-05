;;; Requires looking-back-from-current defined in functions.el

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
  (let* ((whitespace-pattern "\\s-")
         (word-char-pattern "[a-zA-Z|å|ä|ö|Å|Ä|Ö]")
         (non-word-char-pattern "[-]")

         (selection-start (if (and start mark-active) start (point)))
         (selection-end (if (and end mark-active) end (point)))
         (selected-text (buffer-substring selection-start selection-end))

         (whitespace-before-p (looking-back-from-point-at-p selection-start whitespace-pattern))
         (whitespace-after-p (looking-from-point-at-p selection-end whitespace-pattern))
         (word-char-before-p (looking-back-from-point-at-p selection-start word-char-pattern))
         (word-char-after-p (looking-from-point-at-p selection-start word-char-pattern))
         (non-word-char-before-p (looking-back-from-point-at-p selection-start non-word-char-pattern))
         (non-word-char-after-p (looking-from-point-at-p selection-start non-word-char-pattern))

         new-selection-start
         new-selection-end)

    (progn
      (setq new-selection-start selection-start)
      (setq new-selection-end selection-end)
      (cond
       ((and word-char-before-p word-char-after-p)
        (goto-char selection-start)
        (skip-chars-backward "a-zA-Z|å|ä|ö|Å|Ä|Ö")
        (setq new-selection-start (point))
        (goto-char selection-end)
        (skip-chars-forward "a-zA-Z|å|ä|ö|Å|Ä|Ö")
        (setq new-selection-end (point)))
       
       ((and word-char-before-p non-word-char-after-p)
        (goto-char selection-start)
        (skip-chars-backward "a-zA-Z|å|ä|ö|Å|Ä|Ö")
        (setq new-selection-start (point)))

       ((and non-word-char-before-p word-char-after-p)
        (goto-char selection-end)
        (skip-chars-forward "a-zA-Z|å|ä|ö|Å|Ä|Ö")
        (setq new-selection-end (point))))
      (goto-char new-selection-end)
      (set-mark new-selection-start))))

;; Nisse-Kalle-Pelle

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

;; Hela buffern
