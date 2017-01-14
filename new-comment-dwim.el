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
;;;   The code should be extended to work with 'all' programming languages by better recognising
;;;  non-lispy comments and using the comment-leftpad/comment-rightpad/comment-start/comment-end
;;;  facilities already present in emacs.
;;; 
;;; Changelog
;;;
;;; 20170114     Version 0.2
;;;              - Support for 4-semicolon comment blocks when at the top of the file
;;;              - removed code that was ultimately redundant by reusing in-comment to determine
;;;                the status of comment in the current line as well as previous lines.
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

(defun ns/kill-forward-whitespace ()
  (while (= (following-char) 32)
    (kill-forward-chars 1)))

(defun ns/test-to-start ()
  "look backwards from point to start of the buffer for lines that are inside a comment"
  (save-excursion
    (let ((is-comment 't)
          (start-point (buffer-end -1)))
      (while (and is-comment
                  (> (point) start-point))
        (forward-line -1)
        (let ((line (string-trim (ns/extract-line))))
          (setq is-comment (and (> (length line) 0) (string= ";" (substring line 0 1))))))
      (and is-comment (= (point) start-point)))))


(defun ns/eol-column ()
  "find the end of line column number"
  (save-excursion
    (end-of-line)
    (current-column)))

(defun ns/in-comment (offset)
  "Return number of comment semicolons used on previous line, or nil if previous line was not a comment"
  (save-excursion
    (beginning-of-line offset)
    (let ((trimmed (string-trim (ns/extract-line))))
      (cond ((and (>= (length trimmed) 4)
                  (string= (substring trimmed 0 4) ";;;;"))
             4)
            ((and (>= (length trimmed) 3)
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

;;;###autoload
(defun ns/comment-insert ()
  "Insert a comment with DWIM style functionality"
  (interactive)
  (let* ((start-block (ns/test-to-start))
         (pcomment (ns/in-comment 0))
         (tcomment (ns/in-comment nil))
         (line-contents (ns/extract-line))
         (trimmed (string-trim line-contents))
         (len (length trimmed))
         (insert-basic
          (lambda (width)
            (insert (comment-padright comment-start (comment-add width)))
            (unless (string= "" comment-end)
              (insert (comment-padleft comment-end (comment-add width))))))
         (handle-ne/nc
          (lambda ()
            (end-of-line)
            (let* ((cpos (ns/eol-column))
                   (ipos (max 40 (* 10 (+ 1 (/ cpos 10)))))    ; round to next 10'th position
                   (pad  (- ipos cpos)))
              (insert (make-string pad ? ))
              (funcall insert-basic 1))))
         (delete-and-add
          (lambda (del add)
            (save-excursion
              (beginning-of-line)
              (ns/kill-forward-whitespace)
              (kill-forward-chars del)
              (ns/kill-forward-whitespace)
              (funcall insert-basic add)))))
    (cond
     ((= len 0)                         ; line is empty, simplest case
      (funcall insert-basic (or pcomment 2)))
     ((equalp tcomment 2)               ; line starts with 2 semicolons
      (progn
        (funcall delete-and-add 2 3)
        (end-of-line)))
     ((equalp tcomment 3)               ; line starts with 3 semicolons
      (progn
        (if start-block
            (funcall delete-and-add 3 4)
          (funcall delete-and-add 3 2))
        (end-of-line)))
     ((equalp tcomment 4)               ; line starts with 4 semicolons
      (funcall delete-and-add 4 2)
      (end-of-line))
     ((string-match ";+ " line-contents)
      (move-to-column (match-end 0)))
     ((string-match ";+" line-contents)
      (move-to-column (match-end 0))
      (if (not (equal (face-at-point) 'font-lock-string-face))
          (insert " ")))
     ('t                                ; fallthrough that implies we're on a non-empty/non-comment line
      (funcall handle-ne/nc)))))

;;;###autoload
(defun ns/comment-newline ()
  "Insert a newline, followed by a comment insert to match the previous line"
  (interactive)
  (newline)
  (ns/comment-insert))

