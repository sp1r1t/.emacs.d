
(use-package ob-mermaid)
(setq ob-mermaid-cli-path "/opt/homebrew/bin/mmdc")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((mermaid . t)
   (scheme . t)
   ))
