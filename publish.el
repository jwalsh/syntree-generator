;; publish.el -- Org-mode publishing configuration for syntree-generator

;; Load org-mode
(require 'org)
(require 'ox-publish)

;; Custom syntax highlighting for S-expressions
(defun setup-sexp-highlighting ()
  "Set up syntax highlighting for S-expression grammar files."
  (require 'lisp-mode)
  
  ;; Create a derived mode for S-expression grammar files
  (define-derived-mode lisp-syntax-mode lisp-mode "S-expr Grammar"
    "Major mode for editing S-expression grammar files."
    (setq-local lisp-indent-function 'lisp-indent-function)
    (setq-local font-lock-defaults
                '((;; Tag names like ROOT, S, NP, VP
                   ("(\\(ROOT\\|S\\|NP\\|VP\\|PP\\|DET\\|N\\|V\\|P\\|ADJ\\|ADV\\|ADVP\\|REL\\|REL-PRO\\|PRO\\|CP\\|SUB\\|PART\\|PST-PART\\|CONJ\\|COND\\|TEMP\\|INF\\|NEG-INF\\|NEG\\)" 1 font-lock-type-face)
                   ;; String literals
                   ("\"\\([^\"]*\\)\"" 1 font-lock-string-face)))))
  
  ;; Associate with .lisp and .lisp files
  (add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-syntax-mode))
  (add-to-list 'auto-mode-alist '("examples/.*\\.lisp\\'" . lisp-syntax-mode)))

;; Initialize the syntax highlighting
(setup-sexp-highlighting)

;; Define the publishing project
(setq org-publish-project-alist
      '(("syntree-generator-docs"
         :base-directory "."
         :base-extension "org"
         :publishing-directory "./docs"
         :recursive nil
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :with-toc t
         :section-numbers nil
         :html-preamble "<div id=\"org-div-home-and-up\">
           <a href=\"index.html\">Home</a> | 
           <a href=\"https://github.com/jwalsh/syntree-generator\">GitHub</a>
         </div>"
         :html-postamble "<div class=\"footer\">
           <p>Author: Jason Walsh (j@wal.sh)</p>
           <p>Created with %c</p>
         </div>"
         :auto-sitemap t
         :sitemap-title "Syntree Generator Documentation"
         :sitemap-filename "index.org"
         :sitemap-sort-files anti-chronologically)
        
        ("syntree-generator-static"
         :base-directory "."
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "./docs"
         :recursive t
         :publishing-function org-publish-attachment)
        
        ("syntree-generator" :components ("syntree-generator-docs" "syntree-generator-static"))))

;; Function to tangle all org files
(defun tangle-all-org-files ()
  "Tangle all org files in the project."
  (interactive)
  (let ((org-files '("sexp-grammar-examples.org" "README.org")))
    (dolist (file org-files)
      (when (file-exists-p file)
        (message "Tangling %s..." file)
        (org-babel-tangle-file file)))))

;; Provide this functionality
(provide 'publish)
