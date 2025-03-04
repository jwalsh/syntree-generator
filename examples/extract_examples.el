(defun syntree-extract-examples (file-path)
  "Extract S-expression examples from FILE-PATH containing parsed output."
  (with-temp-buffer
    (insert-file-contents file-path)
    (let ((examples '())
          (current-example '())
          (in-example nil))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (cond
           ;; Start of a new example
           ((string-match "^SENTENCE" line)
            (when current-example
              (push (string-join (reverse current-example) "\n") examples)
              (setq current-example nil))
            (push line current-example)
            (setq in-example t))
           
           ;; Add line to current example if we're in an example
           (in-example
            (if (string-match "^$\\|^CHUNK\\|^===" line)
                (setq in-example nil)
              (push line current-example)))))
        (forward-line 1))
      
      ;; Add the last example if there is one
      (when current-example
        (push (string-join (reverse current-example) "\n") examples))
      
      ;; Return the examples
      (reverse examples))))

(defun syntree-save-examples (examples output-dir)
  "Save EXAMPLES to separate files in OUTPUT-DIR."
  (unless (file-directory-p output-dir)
    (make-directory output-dir t))
  
  (let ((simple-examples '())
        (medium-examples '())
        (complex-examples '())
        (very-complex-examples '()))
    
    ;; Categorize examples by complexity
    (dolist (example examples)
      (let* ((lines (split-string example "\n"))
             (depth (length lines))
             (nesting (with-temp-buffer
                        (insert example)
                        (goto-char (point-min))
                        (how-many "("))))
        (cond
         ((< depth 10) (push example simple-examples))
         ((< depth 20) (push example medium-examples))
         ((< depth 30) (push example complex-examples))
         (t (push example very-complex-examples)))))
    
    ;; Save examples to files
    (with-temp-file (expand-file-name "simple_examples.lisp" output-dir)
      (insert "# Simple S-expression examples\n\n")
      (dolist (example (reverse (seq-take simple-examples 5)))
        (insert "# Example\n")
        (insert example)
        (insert "\n\n")))
    
    (with-temp-file (expand-file-name "medium_examples.lisp" output-dir)
      (insert "# Medium S-expression examples\n\n")
      (dolist (example (reverse (seq-take medium-examples 5)))
        (insert "# Example\n")
        (insert example)
        (insert "\n\n")))
    
    (with-temp-file (expand-file-name "complex_examples.lisp" output-dir)
      (insert "# Complex S-expression examples\n\n")
      (dolist (example (reverse (seq-take complex-examples 5)))
        (insert "# Example\n")
        (insert example)
        (insert "\n\n")))
    
    (with-temp-file (expand-file-name "very_complex_examples.lisp" output-dir)
      (insert "# Very complex S-expression examples\n\n")
      (dolist (example (reverse (seq-take very-complex-examples 5)))
        (insert "# Example\n")
        (insert example)
        (insert "\n\n")))))

(provide 'syntree-extract-examples)
