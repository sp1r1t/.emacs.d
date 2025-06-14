
(use-package ob-mermaid)
(setq ob-mermaid-cli-path "/Users/julian/.nvm/versions/node/v20.18.0/bin/mmdc")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((mermaid . t)
   (scheme . t)
   ))
