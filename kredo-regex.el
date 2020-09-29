(setq kredo-re "\\([0-9]+\\).\\([0-9]+\\).\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\) \\(KREDOBANK\\)? ?\\(INTERNET CODE\\|KUPIVLIA\\|PEREKAZ\\|ZARAKHUVANIA\\|ZNYATIA HOTIVKY\\|SPYSANIA\\|AKTUVYJTE KARTY\\) ?\\(\\([[:graph:][:space:]]*?\\), \\(UA\\|EE\\|GB\\|US\\|NL\\)\\)? \\(\\([[:digit:].]+\\)\\([[:upper:]]+\\) \\(CARD\\|NA KARTY \\|Z  KARTY \\)\\*\\*\\([[:digit:]]\\{4\\}\\)\\)?\\( ZALISHOK \\([[:digit:].-]+\\) UAH \\(OVER\\|OVERDRAFT\\)? ?[[:digit:].]+ ? \\(UAH\\)? DOSTUPNO [[:digit:].-]+UAH\\)?\\(AKTUVYVATU [[:graph:] ]*\\)?")

(defun kredo-replace-func (&optional test count)
  "TEST test argument. COUNT test."
  (let ((day (match-string 1))
        (month (match-string 2))
        (year (match-string 3))
        (hour (match-string 4))
        (minute (match-string 5))
        (type (match-string 7))
        (payee (match-string 9))
        (amount (match-string 12))
        (currency (match-string 13))
        (fromto (match-string 14)) ;; NA KARTY | Z  KARTY
        (card (match-string 15))
        (left-amount (match-string 17)))
    (let ((payee (cond ((not payee) "\\?")
                       (t payee)))
          (isdebit (cond ((and fromto (string-match "NA KARTY" fromto)) "-")
                         (t "")))
          (account-to (cond ((not payee) "\\?")
                            ((string-match "UKLON\\|UBER" payee) "Taxi")
                            (t "\\?"))) ;; edit category by yourself
          (account-from (cond ((not card) "\\?")
                              ((string-match "7257" card) "Kredo")
                              ((string-match "7154" card) "FOP")
                              (t "\\?")))) ;; edit by yourself
      (let ((posting (format "%s/%s %s\n\t%s  %s%s %s\n\t%s\n\t; type=%s, left=%s\n" day month payee account-to isdebit amount currency account-from type left-amount)))
        (message (format "Replacing #%d:\n%s" count posting))
        posting))))

(defun kredo-replace ()
  (interactive)
  (perform-replace kredo-re ; from-string
                 '(kredo-replace-func . nil) ; replacement function
                 t ; query-flag
                 t ; regexp-flag
                 nil ; delimited-flag
                 nil ; repeat-count
                 query-replace-map ; map
                 nil ; start
                 nil ; end
                 nil ; backward
                 nil ; noncontiguous
                 ))
