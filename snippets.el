(progn
(defvar mysnip-offset-power 2)



(defun mysnip-add-offset-to-strings(offset strings)
  (setq strings
    (cons (car strings)  
      (mapcar
	(lambda (str)
	  (concat offset str) 
	) 
	(cdr strings)
      ) 
    )
  )  
)



(defun mysnip-make-snippet-string-from-strings(strings)
  (mapconcat
    (lambda (str)
      str
    )
    strings
    ""
  )
)



(defun mysnip-print-snippet-and-move-point(snippet-string point-start-pos end)
  (princ snippet-string (current-buffer))
  (goto-char point-start-pos)
  (end-of-line end)    
)



(defun mysnip-let()
  ""
  (interactive)
  (let*
      (
        (snippet-string "")
        (strings nil)
        (space ?\ )
	(base-offset (make-string (current-column) space))		
	(offset "")
	(point-start-pos (point))
      )
    
    (setq strings
      (list
        "(let*\n"
        (concat (make-string (* mysnip-offset-power 2) space) "(\n")
	(concat (make-string (* mysnip-offset-power 3) space) "\n")
	(concat (make-string (* mysnip-offset-power 2) space) ")\n")
	(concat (make-string mysnip-offset-power space) "\n")
	(concat (make-string mysnip-offset-power space) "\n")
	")"
      )
    )

    (setq strings (mysnip-add-offset-to-strings base-offset strings))
    (setq snippet-string (mysnip-make-snippet-string-from-strings strings))
    (mysnip-print-snippet-and-move-point snippet-string point-start-pos 3)
  )
)



(defun mysnip-lvar(name)
  ""
  (interactive "sName: ")

  (let*
      (
        (snippet-string "")
        (strings nil)
        (space ?\ )
	(base-offset (make-string (current-column) space))		
	(offset "")
	(point-start-pos (point))        
      )

    (if (stringp name)
	()
        (setq name "var")
    )
    (setq strings
      (list
        (concat "(" name " )\n")
        ""
      )
    )
    
    (setq strings (mysnip-add-offset-to-strings base-offset strings))
    (setq snippet-string (mysnip-make-snippet-string-from-strings strings))
    (mysnip-print-snippet-and-move-point snippet-string point-start-pos 1)
    (backward-char 1)
  )
)



(defun mysnip-lambda()
  ""
  (interactive)

  (let*
      (
        (snippet-string "")
        (strings nil)
        (space ?\ )
	(base-offset (make-string (current-column) space))		
	(offset "")
	(point-start-pos (point))        
      )

    (setq strings
      (list
        "(lambda ()\n"
        (concat (make-string mysnip-offset-power space) "\n")
	")"
      )      
    )
    
    (setq strings (mysnip-add-offset-to-strings base-offset strings))
    (setq snippet-string (mysnip-make-snippet-string-from-strings strings))
    (mysnip-print-snippet-and-move-point snippet-string point-start-pos 1)
    (backward-char 1)
  )
)



(defun mysnip-block(name)
  ""
  (interactive "sName: ")

  (let*
      (
        (snippet-string "")
        (strings nil)
        (space ?\ )
	(base-offset (make-string (current-column) space))		
	(offset "")
	(point-start-pos (point))        
      )

    (if (stringp name)
	()
        (setq name "progn")
    )
    (setq strings
      (list
        (concat "(" name  " \n")
	(concat (make-string mysnip-offset-power space) "\n")
	")"
      )
    )
    
    (setq strings (mysnip-add-offset-to-strings base-offset strings))
    (setq snippet-string (mysnip-make-snippet-string-from-strings strings))
    (mysnip-print-snippet-and-move-point snippet-string point-start-pos 2)
  )
)



(defun mysnip-defun(name)
  ""
  (interactive "sName: ")

  (let*
      (
        (snippet-string "")
        (strings nil)
        (space ?\ )
	(base-offset (make-string (current-column) space))		
	(offset "")
	(point-start-pos (point))        
      )

    (if (stringp name)
	()
        (setq name "function")
    )
    (setq strings
      (list
        (concat "(defun " name  "()\n")
        (concat (make-string mysnip-offset-power space) "\"\"\n")
	(concat (make-string mysnip-offset-power space) ";(interactive \"\")\n")
	(concat (make-string mysnip-offset-power space) "\n")
	(concat (make-string mysnip-offset-power space) "\n")
	")"
      )
    )
    
    (setq strings (mysnip-add-offset-to-strings base-offset strings))
    (setq snippet-string (mysnip-make-snippet-string-from-strings strings))
    (mysnip-print-snippet-and-move-point snippet-string point-start-pos 1)
    (backward-char 1)
  )
)



(defun mysnip-if()
  ""
  (interactive)

  (let*
      (
        (snippet-string "")
        (strings nil)
        (space ?\ )
	(base-offset (make-string (current-column) space))		
	(offset "")
	(point-start-pos (point))        
      )

    (setq strings
      (list
        "(if ()\n"
        (concat (make-string mysnip-offset-power space) "() ; then form\n")
	(concat (make-string mysnip-offset-power space) "() ; else forms\n")
	")"
      )
    )
    
    (setq strings (mysnip-add-offset-to-strings base-offset strings))
    (setq snippet-string (mysnip-make-snippet-string-from-strings strings))
    (mysnip-print-snippet-and-move-point snippet-string point-start-pos 1)
    (backward-char 1)
  )
)



(defun mysnip-when()
  ""
  (interactive)

  (let*
      (
        (snippet-string "")
        (strings nil)
        (space ?\ )
	(base-offset (make-string (current-column) space))		
	(offset "")
	(point-start-pos (point))        
      )

    (setq strings
      (list
        "(when ()\n"
        (concat (make-string mysnip-offset-power space) "() ; then forms\n")
	")"
      )
    )
    
    (setq strings (mysnip-add-offset-to-strings base-offset strings))
    (setq snippet-string (mysnip-make-snippet-string-from-strings strings))
    (mysnip-print-snippet-and-move-point snippet-string point-start-pos 1)
    (backward-char 1)
  )
)



(defun mysnip-unless()
  ""
  (interactive)

  (let*
      (
        (snippet-string "")
        (strings nil)
        (space ?\ )
	(base-offset (make-string (current-column) space))		
	(offset "")
	(point-start-pos (point))        
      )

    (setq strings
      (list
        "(unless ()\n"
        (concat (make-string mysnip-offset-power space) "() ; else forms\n")
	")"
      )
    )
    
    (setq strings (mysnip-add-offset-to-strings base-offset strings))
    (setq snippet-string (mysnip-make-snippet-string-from-strings strings))
    (mysnip-print-snippet-and-move-point snippet-string point-start-pos 1)
    (backward-char 1)
  )
)



(defun mysnip-cond(conditions-count)
  ""
  (interactive "sConditions count: ")

  (let*
      (
        (snippet-string "")
        (strings nil)
        (space ?\ )
	(base-offset (make-string (current-column) space))		
	(offset "")
	(point-start-pos (point))        
	(condition-strings nil)
      )

    (setq conditions-count (string-to-number conditions-count))
    (if (not (equal conditions-count 0))
      nil
      (setq conditions-count 1)
    )
    
    (setq condition-strings
      (lambda ()
        (list 
	  (concat (make-string mysnip-offset-power space) "(()\n")
	  (concat (make-string (* mysnip-offset-power 2) space) "\n")
	  (concat (make-string mysnip-offset-power space) ")\n")
        )  
      )
    )

    (setq condition-strings 
      (let*
          (
            (i 0)
            (strings nil)
	  )

	(while (< i conditions-count)
	  (setq strings (append strings (funcall condition-strings)))
	  (setq i (1+ i))
	)
	strings
      )
    )
    
    (setq strings
      (append
        (list "(cond\n")
        condition-strings
	(list ")")
      )
    )
    
    (setq strings (mysnip-add-offset-to-strings base-offset strings))
    (setq snippet-string (mysnip-make-snippet-string-from-strings strings))
    (mysnip-print-snippet-and-move-point snippet-string point-start-pos 2)
    (backward-char 1)
  )
)



(defun mysnip-while()
  ""
  (interactive)

  (let*
      (
        (snippet-string "")
        (strings nil)
        (space ?\ )
	(base-offset (make-string (current-column) space))		
	(offset "")
	(point-start-pos (point))        
      )

    (setq strings
      (list
        "(while ()\n"
        (concat (make-string mysnip-offset-power space) "()\n")
	")"
      )
    )
    
    (setq strings (mysnip-add-offset-to-strings base-offset strings))
    (setq snippet-string (mysnip-make-snippet-string-from-strings strings))
    (mysnip-print-snippet-and-move-point snippet-string point-start-pos 1)
    (backward-char 1)
  )
)





)

                         
