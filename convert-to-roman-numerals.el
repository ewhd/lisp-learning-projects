#!emacs --script

(defun to-roman (AN)
  "Translate from arabic number AN to roman number.
   For example, (to-roman 1999) returns 'MCMXCIX' as a string.

   The function works by leveraging the way the pattern of Roman numerals is the
   same for any multiple of 10, just the digits change. I.e. the pattern always 
   goes a, aa, aaa, ab, b, ba, baa, baaa, ac, c, regardless of whether (a, b, c)
   are (I, V, X) for (1, 5, 10) or (C, D, M) for (100, 500, 1000).
   The function uses a loop counter (magnitude) corresponding to double the
   multiple of 10 of the arabic digit being converted, which provides the 
   relative index of the correct Roman numeric character within a string 
   (romans).
   This way, I only needed to encode the pattern of Roman numeric characters
   across a multiple of 10 once, and then repeat that pattern, shifting the
   magnitude to adjust which characters are used.
   Admittedly, Roman numerals don't got much past 3000, so such a generalized 
   solution probably isn't much tighter than one which relies on more direct 
   encoding, but it was much more satisfying."
  
  (setq output-list '())
  (setq magnitude 0)
  (setq romans "IVXLCDM")
  
  (defun add-rom-num (which-num list)
    (cons (string (elt romans which-num)) list))

  (setq AN (cl-map 'list (lambda (c)(or (cl-digit-char-p c) '-)) (cl-prin1-to-string AN)))
  (setq AN (reverse AN))
  (dolist (item AN)
    (cond
     ((= item 9)
      (setq output-list (add-rom-num magnitude (add-rom-num (+ 2 magnitude) output-list))))
     ((= item 8)
      (setq output-list (add-rom-num (+ 1 magnitude) (add-rom-num magnitude (add-rom-num magnitude (add-rom-num magnitude output-list))))))
     ((= item 7)
      (setq output-list (add-rom-num (+ 1 magnitude) (add-rom-num magnitude (add-rom-num magnitude output-list)))))
     ((= item 6)
      (setq output-list (add-rom-num (+ 1 magnitude) (add-rom-num magnitude output-list))))
     ((= item 5)
      (setq output-list (add-rom-num (+ 1 magnitude) output-list)))
     ((= item 4)
      (setq output-list (add-rom-num magnitude (add-rom-num (+ 1 magnitude) output-list))))
     ((= item 3)
      (setq output-list (add-rom-num magnitude (add-rom-num magnitude (add-rom-num magnitude output-list)))))
     ((= item 2)
      (setq output-list (add-rom-num magnitude (add-rom-num magnitude output-list))))
     ((= item 1)
      (setq output-list (add-rom-num magnitude output-list)))
     )
    (setq magnitude (+ 2 magnitude))
    )
  (mapconcat 'identity output-list "")
  )
