new-comment-dwim.el
-------------------

About
=====

The standard emacs ``comment-dwim`` command can be somewhat
disappointing, especially on lisp languages. Comment-dwim's docstring
hints at a utopia where pressing M-; can switch between ;; and ;;;
forms, along with automatically determining if you're in a comment
block, and handling the case of adding comments alongside code.

This attempts to make reality a little closer to that utopia.


Usage
=====

Probably the most useful way to use this is to bind M-; and M-j to the replacement
functions, either globally if all you use is lispy languages, or within a mode-hook.

e.g:

.. code:: emacs-lisp
          
    (defun my/new-commenting ()
       (local-set-key (kbd "M-;") 'ns/comment-insert)
       (local-set-key (kbd "M-j") 'ns/comment-newline))

    (add-hook 'lisp-mode-hook 'my/new-commenting)
    (add-hook 'scheme-mode-hook 'my/new-commenting)
    (add-hook 'emacs-lisp-mode-hook 'my/new-commenting)


Future
======

Currently the code is very lisp-centric in a way that makes these functions unusable for
non-lisp commenting styles, emacs's comment-dwim doesn't have this limitation, and it would be
better if this didn't too.

If a semicolon exists inside code when trying to add an 'end of line' style comment, the functions
currently get confused about where the insert point should be (it *should* insert a semicolon, space
and position the cursor for writing a comment at the end of the line; currently it jumps the cursor
to the existing semicolons, assuming it to be an existing comment.

License
=======

This code is released under a BSD 2-clause license.
`See BSD 2-clause License at opensource.org <https://opensource.org/licenses/BSD-2-Clause>`_

Copyright 2017 Nicola Archibald.

