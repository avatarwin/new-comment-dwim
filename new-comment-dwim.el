;;; More complex/complete comment-dwim like behaviour
;;; =================================================
;;;
;;; Copyright 2017 Nicola Archibald
;;; 
;;; Binding M-; to ns/comment-insert and M-j to ns/comment-newline should be the most common
;;; use-case for this code.
;;;
;;; Known problems:
;;;   Currently behaviour is incorrect if semicolons exist on the line but are part of
;;;  source code (ie, in a string)           
;;; 
;;;   The comment-insert behaviour should change on the presence of a universal argument;
;;;  most likely the intended behaviour would to be force a line that already contains text
;;;  to become part of a comment block such as this.
;;;
;;;   The missing case of being able to promote a 3 semicolon comment into a 4 semicolon comment
;;;  when the 3 semicolon comment is part of a comment block at the start of the file. In
;;;  this situation the progression of semicolons should be a ring of [2->3->4]
;;; 
;;;   The code should be extended to work with 'all' programming languages by better recognising
;;;  non-lispy comments and using the comment-leftpad/comment-rightpad/comment-start/comment-end
;;;  facilities already present in emacs.
;;; 
;;; Changelog
;;; 
;;; 20170113     Version 0.1
;;;              - Initial version
;;; License
;;; 
;;; Released under the BSD 2-clause license, see https://opensource.org/licenses/BSD-2-Clause
;;;

(defun ns/extract-line ()
  "Extract a line as undecorated text"
  (save-excursion
    (let ((lep (line-end-position)))
    (beginning-of-line)
    (let ((les (point)))
      (buffer-substring-no-properties les lep)))))

(defun ns/eol-column ()
  "find the end of line column number"
  (save-excursion
    (end-of-line)
    (current-column)))

(defun ns/in-comment ()
  "Return number of comment semicolons used on previous line, or nil if previous line was not a comment"
  (save-excursion
    (beginning-of-line 0)
    (let ((trimmed (string-trim (ns/extract-line))))
      (cond ((and (>= (length trimmed) 3)
                  (string= (substring trimmed 0 3) ";;;"))
             3)
            ((and (>= (length trimmed) 2)
                  (string= (substring trimmed 0 2) ";;"))
             2)
            ((and (>= (length trimmed) 1)
                  (string= (substring trimmed 0 1) ";"))
             1)
            ('t
             nil)))))

(defun ns/comment-insert ()
  "Insert a comment with DWIM style functionality"
  (interactive)
  (let* ((pcomment (ns/in-comment))
         (line-contents (ns/extract-line))
         (trimmed (string-trim line-contents))
         (len (length trimmed))
         (insert-basic
          (lambda (width)
            (insert (comment-padright comment-start (comment-add width)))
            (unless (string= "" comment-end)
              (insert (comment-padleft comment-end (comment-add width))))))
         (delete-and-add
          (lambda (del add)
            (save-excursion
              (beginning-of-line)
              (kill-forward-chars del)
              (funcall insert-basic add)))))
    (cond
     ((= len 0)                       ; line is empty, simplest case
      (if pcomment
          (funcall insert-basic pcomment)
        (funcall insert-basic 2)))
     ((string= ";;" trimmed)          ; line starts with 2 semicolons but no text - promote to 3 semicolons
      (progn
        (funcall delete-and-add 3 3)
        (end-of-line)))
     ((and (> len 2)
           (string= ";; " (substring trimmed 0 3)))
      (funcall delete-and-add 3 3))
     ((string= ";;;" trimmed)           ; line starts with 3 semicolons, demote to 2 semicolons
      (progn
        (funcall delete-and-add 4 2)
        (end-of-line)))
     ((and (> len 3)                    ; 
           (string= ";;; " (substring trimmed 0 4)))
      (funcall delete-and-add 4 2))
     ((string-match ";+ " line-contents)
      (move-to-column (match-end 0)))
     ((string-match ";+" line-contents)
      (move-to-column (match-end 0))
      (insert " "))     
     ('t                                ; fallthrough that implies we're on a non-empty/non-comment line
      (end-of-line)
      (let* ((cpos (ns/eol-column))     
             (ipos (max 40 (* 10 (+ 1 (/ cpos 10)))))    ; round to next 10'th position
             (pad  (- ipos cpos)))
        (insert (make-string pad ? ))
        (funcall insert-basic 1)))      
     )))

(defun ns/comment-newline ()
  "Insert a newline, followed by a comment insert to match the previous line"
  (interactive)
  (newline)
  (ns/comment-insert))

