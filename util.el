(progn



(defvar mysnip-mark-char ?$)

(defvar mysnip-dynamic-snippets-map nil)  
(setq mysnip-dynamic-snippets-map (make-hash-table :size 50))  

(defun mysnip-is-whitespace(char)
  ""
  ;(interactive "")

  (let*
      (
        (space-char ?\ )
        (tab-char ?\t)
        
      )
    
    (or
      (char-equal char  space-char)
      (char-equal char  tab-char)
    )      
  )  
)



(defun mysnip-is-digit(char)
  ""
  ;(interactive "")
  
  (and
    (>= char ?0)
    (<= char ?9)
  )  
)



(defun mysnip-is-az(char)
  ""
  ;(interactive "")

  (and
    (>= char ?a)
    (<= char ?z)
  )
)



(defun mysnip-is-AZ(char)
  ""
  ;(interactive "")

  (and
    (>= char ?A)
    (<= char ?Z)
  )
)



(defun mysnip-is-letter(char)
  ""
  ;(interactive "")
  
  (or
    (mysnip-is-az char)
    (mysnip-is-AZ char)
  )  
)



(defun mysnip-is-ident(char)
  ""
  ;(interactive "")
  
  (or
    (mysnip-is-letter char)
    (mysnip-is-digit char)
  )  
)



(defun mysnip-util-wrap(shell)
  ""
  (interactive "sShell: ")
  
  (let*
      (
	(region-text (buffer-substring-no-properties (region-beginning) (region-end)))
        (space-char ?\ )
	(next-line-char ?\n)	
	(tab-char ?\t)
	(output-text (concat shell region-text shell))
      )

    (delete-region (region-beginning) (region-end))
    (princ output-text (current-buffer))
  )
)



(defun mysnip-read-dynamic-snippet(snippet-name)
  ""
  (interactive "sName: ")
  
  (let*
      (
        (general-state)
	(arg-state)
	(state 'general-state)
	(region-text (buffer-substring-no-properties (region-beginning) (region-end)))
	(space-char ?\ )
	(nextline-char ?\n)	
	(tab-char ?\t)
	(snippet-lines)
	(snippet-line)
	(current-line-fragment)
	(fragment-begin 0)
	(fragment-end 0)
	(mark-char mysnip-mark-char)
	(last-char)        
      )

    (mapc
      (lambda (char)
        (setq last-char char) 
	(cond
	  ((equal state 'general-state)
	    (cond
	      ((char-equal char nextline-char)
		(setq fragment-end (1+ fragment-end))
		(setq current-line-fragment (substring region-text fragment-begin fragment-end))
		(setq snippet-line (append snippet-line (list current-line-fragment)))
		(setq snippet-lines (append snippet-lines (list snippet-line)))
		(setq snippet-line nil)
		(setq fragment-begin fragment-end)
	      )
	      ((char-equal char mark-char)	       
		(setq current-line-fragment (substring region-text fragment-begin fragment-end))
		(setq snippet-line (append snippet-line (list current-line-fragment)))
		(setq fragment-end (+ fragment-end 1))
		(setq fragment-begin fragment-end)
		(setq state 'arg-state)
	      )
	      (t
		(setq fragment-end (1+ fragment-end))
	      )
	    )
	  )
	  ((equal state 'arg-state)
	    (cond
	      ((not (char-equal char mark-char))
		(setq fragment-end (1+ fragment-end))
	      )
	      (t
		(setq current-line-fragment
		  (cons t (intern (substring region-text fragment-begin fragment-end)))
		)
		(setq snippet-line (append snippet-line (list current-line-fragment)))
		(setq fragment-end (+ fragment-end 1))
		(setq fragment-begin fragment-end)		 
		(setq state 'general-state)
	      )
	    )
	  )
	)
      )
      region-text
    )      
    (cond
      ((equal state 'general-state)
	(when (not (char-equal last-char nextline-char))      
	  (setq current-line-fragment (substring region-text fragment-begin fragment-end))
	  (setq snippet-line (append snippet-line (list current-line-fragment)))
	  (setq snippet-lines (append snippet-lines (list snippet-line)))          
	)            
      )
      ((equal state 'arg-state)
	(when (not (equal fragment-begin fragment-end))      
	  (setq current-line-fragment
	    (cons t (substring region-text fragment-begin fragment-end))
	  )
	  (setq snippet-line (append snippet-line (list current-line-fragment)))
	  (setq snippet-lines (append snippet-lines (list snippet-line)))          
	)            
      )
    )
    (puthash (intern snippet-name) snippet-lines mysnip-dynamic-snippets-map)
    (message "snippet has been created")
  )
)



(defun mysnip-get-dynamic-snippet-lines(snippet-name)
  ""
  ;(interactive "")
  
  (let*
      (
        (snippet-pack)
        (mark-string (make-string 1 mysnip-mark-char))
      )
    
    (setq snippet-pack
      (gethash
        (intern snippet-name)
        mysnip-dynamic-snippets-map
	(list (list (concat mark-string snippet-name  mark-string)))
      )
    )
    (mapcar 
      (lambda (snippet-line)
        (mapconcat 
          (lambda (fragment)
            (if (stringp fragment)
              fragment
              (concat mark-string (symbol-name (cdr fragment)) mark-string)
            )
          )
	  snippet-line
	  ""
	)
      )
      snippet-pack
    )
  )
)


(defun mysnip-print-dynamic-snippet(snippet-name)
  ""
  (interactive "sName: ")
  
  (let*
      (
        (mark-string (make-string 1 mysnip-mark-char))
	(snippet-lines
	  (gethash
	    (intern snippet-name)
	    mysnip-dynamic-snippets-map
	    (list (list (concat mark-string snippet-name mark-string)))
	  )
	)
	(output-snippet-lines)
	(output-fragment "")
	(base-offset (current-column))
        (inner-offset 0)
        (fragment-lines)
	(space-char ?\ )
      )
    
    (mapc
      (lambda (snippet-line)
        (mapc 
          (lambda (fragment)
            (if (stringp fragment)
              (progn                 
		(if (stringp output-fragment)
		  (progn 
                    (setq output-fragment (concat output-fragment fragment))
		    (setq inner-offset (length output-fragment))
		  )
		  (progn 
                    (setcar output-fragment (concat (car output-fragment) fragment))
		    (setq inner-offset (length (car output-fragment))
                  )		  
		)		  	       		
              )	      
              (progn 
                (setq fragment-lines (mysnip-get-dynamic-snippet-lines (symbol-name (cdr fragment))))		
		(setq output-fragment (concat output-fragment (car fragment-lines)))
		(setq output-snippet-lines (append output-snippet-lines (list output-fragment)))
		(setq fragment-lines (mysnip-add-offset-to-strings inner-offset (cdr fragment-lines)));!
		(setq output-snippet-lines (append output-snippet-lines fragment-lines))
		(setq output-fragment (last fragment-lines))
		(setq inner-offset (length output-fragment))
              )
            )
          )
	  snippet-line
	)
	(if (stringp output-fragment)
          (setq output-snippet-lines (append output-snippet-lines (list output-fragment)))
          ()
        )	
	(setq output-fragment "")
	(setq inner-offset 0)
      )
      snippet-lines
    )
    (progn 
      (princ "\n" (current-buffer))
      (print output-snippet-lines (current-buffer))
    )
  )
)


)


there is $arg1$ on first line
$arg2$  on second line
and finally on line 3 here is $arg3$

mother
father
sister
brother



