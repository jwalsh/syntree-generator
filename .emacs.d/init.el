;; init.el -- Emacs configuration for S-expression grammar highlighting

;; Basic UI settings
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

;; Enhanced lisp editing
(use-package paredit
  :hook ((emacs-lisp-mode lisp-mode lisp-interaction-mode) . paredit-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Org mode
(use-package org
  :config
  (setq org-src-fontify-natively t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-lisp-eval-fn 'sly-eval))

;; S-expression Grammar mode
(define-derived-mode lisp-syntax-mode lisp-mode "S-expr Grammar"
  "Major mode for editing S-expression grammar files."
  (setq-local lisp-indent-function 'lisp-indent-function)
  (setq-local font-lock-defaults
              '((;; Tag names like ROOT, S, NP, VP
                 ("(\\(ROOT\\|S\\|NP\\|VP\\|PP\\|DET\\|N\\|V\\|P\\|ADJ\\|ADV\\|ADVP\\|REL\\|REL-PRO\\|PRO\\|CP\\|SUB\\|PART\\|PST-PART\\|CONJ\\|COND\\|TEMP\\|INF\\|NEG-INF\\|NEG\\)" 1 font-lock-type-face)
                 ;; String literals
                 ("\"\\([^\"]*\\)\"" 1 font-lock-string-face)))))

;; Associate with .lisp and .lisp files in the examples directory
(add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-syntax-mode))
(add-to-list 'auto-mode-alist '("examples/.*\\.lisp\\'" . lisp-syntax-mode))

;; Enable syntax highlighting for S-expressions in org-mode babel blocks
(defun my/org-mode-setup ()
  "Setup for org-mode."
  (setq org-src-fontify-natively t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (lisp . t)
     (shell . t)
     (python . t))))

(add-hook 'org-mode-hook 'my/org-mode-setup)

;; Function to visualize an S-expression tree in a buffer
(defun visualize-sexp-tree (file)
  "Visualize an S-expression tree from FILE in a new buffer."
  (interactive "fSelect S-expression file: ")
  (let ((buf (get-buffer-create "*S-expression Tree*"))
        (content (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string))))
    (with-current-buffer buf
      (erase-buffer)
      (lisp-syntax-mode)
      (insert content)
      (goto-char (point-min)))
    (switch-to-buffer buf)
    (message "Loaded S-expression tree from %s" file)))

;; Add keybinding for the visualization function
(global-set-key (kbd "C-c v") 'visualize-sexp-tree)

;; Enhanced tree visualization
(defun sexp-tree-view ()
  "Display the current S-expression as a tree visualization."
  (interactive)
  (let ((sexp-string (buffer-substring-no-properties (point-min) (point-max)))
        (buf (get-buffer-create "*S-expression Tree View*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "Syntax Tree Visualization\n")
      (insert "========================\n\n")
      
      ;; Parse and visualize the tree
      (let ((indent-level 0)
            (in-string nil))
        (dolist (char (string-to-list sexp-string))
          (cond
           ((= char ?\() 
            (insert "\n")
            (insert (make-string indent-level ?\s))
            (setq indent-level (+ indent-level 2))
            (insert "("))
           ((= char ?\)) 
            (setq indent-level (- indent-level 2))
            (insert ")"))
           ((= char ?\") 
            (setq in-string (not in-string))
            (insert "\""))
           ((= char ?\n) 
            (unless in-string
              (insert "\n")
              (insert (make-string indent-level ?\s))))
           (t (insert (char-to-string char))))))
      
      (goto-char (point-min)))
    
    (switch-to-buffer buf)
    (read-only-mode 1)
    (message "S-expression tree visualized")))

;; Add keybinding for the tree view
(global-set-key (kbd "C-c t") 'sexp-tree-view)

;; Provide a custom function to extract examples from parser output
(defun extract-examples-from-parser-output (input-file output-dir num-examples)
  "Extract NUM-EXAMPLES from INPUT-FILE and save to OUTPUT-DIR."
  (interactive "fInput file: \nDOutput directory: \nnNumber of examples: ")
  (unless (file-directory-p output-dir)
    (make-directory output-dir t))
  
  (let ((examples '())
        (current-example '())
        (in-example nil)
        (example-count 0))
    
    ;; Extract examples
    (with-temp-buffer
      (insert-file-contents input-file)
      (goto-char (point-min))
      (while (and (not (eobp))
                  (< example-count num-examples))
        (let ((line (buffer-substring-no-properties 
                     (line-beginning-position)
                     (line-end-position))))
          (cond
           ;; Start of a new example
           ((string-match "^SENTENCE" line)
            (when current-example
              (push (string-join (reverse current-example) "\n") examples)
              (setq current-example nil)
              (setq example-count (1+ example-count)))
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
        (push (string-join (reverse current-example) "\n") examples)
        (setq example-count (1+ example-count))))
    
    ;; Save examples
    (let ((examples-reversed (reverse examples)))
      (dotimes (i (min (length examples-reversed) num-examples))
        (let ((example (nth i examples-reversed))
              (filename (format "%s/example_%d.lisp" output-dir (1+ i))))
          (with-temp-file filename
            (insert example))
          (message "Saved example %d to %s" (1+ i) filename))))
    
    (message "Extracted %d examples to %s" example-count output-dir)))

;; Provide this init.el
(provide 'init-sexp-grammar)
