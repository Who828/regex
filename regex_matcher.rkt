(define (match regex string)
  (cond ((equal? (cars regex) "^")
         (matchhere (cdrs regex) string))
        ((empty? string) #f)
        ((matchhere regex string) #t)
        (else (match regex (cdrs string)))))

(define (matchhere regex string)
  (cond ((empty? (cars regex)) #t)
        ((equal? (cadrs regex) "*") 
         (matchstar (cars regex) (cdrs (cdrs string)) string))
        ((and (equal? (cars regex) "$") (empty? (cadrs regex)))
         (empty? string))
        ((and (not (empty? string)) (or (equal? (cars regex) ".") (equal? (cars regex) (cars string))))
         (matchhere (cdrs regex) (cdrs string)))
        (else #f)))

(define (matchstar char regex string)
  (cond ((matchhere regex string) #t)
        ((not (and (not (empty? string)) (or (equal? (cars string) c) (equal? c "."))))
         #f)
        (else (matchstar char regex (cdrs string)))))

(define (empty? string) (equal? string ""))

(define (cars string)
  (if (equal? string "") ""
      (substring string 0 1)))

(define (cdrs string)
  (if (equal? string "") ""
      (substring string 1)))

(define (cadrs string)
  (cars (cdrs string)))
