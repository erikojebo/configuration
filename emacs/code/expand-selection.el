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
  (let* ((selection-start (if start start (mark)))
        (selection-end (if end end (point)))
        (selected-text (buffer-substring selection-start selection-end)))
  (when (or (looking-from-point-at-p selection-end "-")
            (looking-back-from-point-at-p selection-start "-"))
      ;; (string-match-p "[a-z]*" selected-text)
    (goto-char selection-start)
    (skip-chars-backward "a-zA-Z|å|ä|ö|Å|Ä|Ö")
    (setq selection-start (point))
    (goto-char selection-end)
    (skip-chars-forward "a-zA-Z|å|ä|ö|Å|Ä|Ö")
    (set-mark selection-start))))

;; Kalle-

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
