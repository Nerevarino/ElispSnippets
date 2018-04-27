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



            
)


