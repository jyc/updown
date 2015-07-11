(use posix srfi-69 srfi-13 irregex)
(use args pty)

(define (cursor-next-line #!optional (n 1))
  (format "\033[~AB" n))

(define (cursor-prev-line #!optional (n 1))
  (format "\033[~AA" n))

(define (cursor-backward #!optional (n 1))
  (format "\033[~AD" n))

(define (cursor-forward #!optional (n 1))
  (format "\033[~AC" n))

(define (erase-in-line #!optional (mode #:to-end))
  (let ((n (case mode
             ((#:to-end) 0)
             ((#:to-beginning) 1)
             ((#:line) 2)
             (else (abort "erase-in-line: Unrecognized mode."))))) 
    (format "\033[~AK" n)))

(define push-line #f)
(define update-line #f)
(define forget-lines #f)
(define count-lines #f)

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
      (display (cursor-backward (string-length text)))
      (let loop ((j i))
        (unless (= j n)
          (display (cursor-next-line))
          (loop (+ j 1))))))

  (set! forget-lines
    (lambda ()
      (set! n 0)))

  (set! count-lines
    (lambda ()
      n)))

(define opts
  (list (args:make-option (x exclude)         #:optional
                          "Exclude a prefix from collection (output it raw).")
        (args:make-option (r reset-length)    #:optional
                          "The number of lines at which to reset collection.")
        (args:make-option (i input)           #!optional
                          (string-append
                            "The regex for parsing input lines. Uses CHICKEN's "
                            "irregex to parse. The first match group should be "
                            "the prefix, and the second should be the message. "))
        (args:make-option (f format)          #:optional
                          (string-append
                            "The output line format, in CHICKEN's printf syntax "
                            "(~A for substitution). Provided two arguments: the "
                            "extracted prefix and the extracted message. ANSI "
                            "presentation information from directly before and "
                            "after the prefix is stripped by the default input "
                            "regex."))
        (args:make-option (d default-prefix)  #:optional
                          (string-append
                            "The default prefix to use for lines that don't match "
                            "the input regex. By default such lines are output raw."))
        (args:make-option (h help)	      #:none
                          "Display this message."
                          (usage))))

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " (car (argv)) " [updown-args...] -- <command> [command-args...] ")
      (newline)
      (print (args:usage opts))
      (print "Sources at https://bitbucket.org/jyc/updown.")
      (exit 1))))

(args:width 30)

(receive (options operands)
  (args:parse (command-line-arguments) opts)

  (if (null? operands)
    (usage))

  (define command (car operands))
  (define arguments (cdr operands))
  (define excludes (make-hash-table))
  (for-each (lambda (l)
              (if (and (cdr l) (eq? (car l) 'exclude))
                (hash-table-set! excludes (cdr l) #t)))
            options) 
  (define reset-n (string->number (or (assq 'reset-length options) "25")))
  (define input-rgx (irregex (or (assq 'input options) "^(?:\x1b\\[[0-9;]+m)?\\[(.+)\\](?:\x1b\\[[0-9;]+m)?(.+)$")))
  (define output-fmt (assq 'format options))
  (define default-prefix (assq 'default-prefix options))

  (unless (and reset-n input-rgx)
    (usage))

  (define prefixes (make-hash-table))

  (define (format-line prefix rest raw)
    (cond
      ((and output-fmt prefix) (format output-fmt prefix rest))
      ((and output-fmt default-prefix) (format output-fmt default-prefix rest))
      (else raw)))

  (set-buffering-mode! (current-output-port) #:full)

  (call-with-pty-process-io (cons command arguments)
    (lambda (in out pid)
      ; Propogate signal to child (seems like this isn't done by default?)
      (set-signal-handler! signal/int
        (lambda (sig)
          (process-signal pid signal/int)))

      (let loop ()
        (flush-output)
        (define line (read-line in))
        (flush-output)
        (if (eof-object? line)
          (exit 0))
        (let* ((m (irregex-match input-rgx line)) 
               (prefix (and m (irregex-match-substring m 1)))
               (rest (and m (irregex-match-substring m 2))))
          (when (= (count-lines) reset-n)
            (set! prefixes (make-hash-table))
            (forget-lines))
          (if (and prefix (not (hash-table-exists? excludes prefix)))
            (if (hash-table-exists? prefixes prefix)
              (update-line (hash-table-ref prefixes prefix)
                           (format-line prefix rest line))
              (let ((m (push-line (format-line prefix rest line))))
                (hash-table-set! prefixes prefix m))) 
            (push-line (format-line prefix rest line))))
        (flush-output)
        (loop))))

  (flush-output))

