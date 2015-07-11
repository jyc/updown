(use posix srfi-69 srfi-13 irregex)

(define (cursor-next-line #!optional (n 1))
  (format "\x1b[~AE" n))

(define (cursor-prev-line #!optional (n 1))
  (format "\x1b[~AF" n))

(define (cursor-backward #!optional (n 1))
  (format "\x1b[~AD" n))

(define (cursor-forward #!optional (n 1))
  (format "\x1b[~AC" n))

(define (erase-in-line #!optional (mode #:to-end))
  (let ((n (case mode
             ((#:to-end) 0)
             ((#:to-beginning) 1)
             ((#:line) 2)
             (else (abort "erase-in-line: Unrecognized mode."))))) 
    (format "\x1b[~AK" n)))

(define push-line #f)
(define update-line #f)

(let ((n 0))
  (set! push-line
    (lambda (text)
    (set! n (+ n 1))
    (print text)
    (- n 1)))

  (set! update-line
    (lambda (i text)
      (let loop ((j n))
        (unless (= j i)
          (display (cursor-prev-line))
          (loop (- j 1))))
      (display text)
      (display (erase-in-line))
      (let loop ((j i))
        (unless (= j n)
          (display (cursor-next-line))
          (loop (+ j 1)))))))

(if (null? (command-line-arguments))
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "usage: " (car (argv)) " <command> [arg] ...")
      (exit 1))))

(define path (car (command-line-arguments)))
(define args (cdr (command-line-arguments)))

(define prefixes (make-hash-table))
(define default-prefix "*")
(define message-rgx (irregex "\\[(.+)\\](.+)"))

(define (format-line prefix rest)
  (format "\x1b[33m[~A]\x1b[0m~A" prefix rest))

(set-buffering-mode! (current-output-port) #:none)

(receive (in out pid)
  (process path args)
  (let loop ()
    (define line (read-line in))
    (if (eof-object? line)
      (exit 0))
    (let* ((m (irregex-match message-rgx line)) 
           (prefix (if m
                     (irregex-match-substring m 1)
                     default-prefix))
           (rest (if m
                   (irregex-match-substring m 2)
                   (string-append " " line))))
      (if (hash-table-exists? prefixes prefix)
        (update-line (hash-table-ref prefixes prefix)
                     (format-line prefix rest))
        (let ((m (push-line (format-line prefix rest))))
          (hash-table-set! prefixes prefix m))))
    (loop)))
