;;;
;;; Org Mode
;;;


;(add-to-list 'load-path (expand-file-name "~/Dropbox/etc/org"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org)

(require 'ob-async)
(require 'ox-confluence)

;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-switchb)

(setq org-todo-keyword-faces
           '(("TODO" . org-warning) 
             ("WAITING" . "yellow")
             ("IN_PROGRESS" . (:foreground "black" :background "pink" ))
             ("STAGED" . "pink")
             ("PEER_REVIEW" . "yellow")
             ("WONTDO" . "purple")
             ("STARTED" . (:foreground "orange" :weight bold))))


(setq org-refile-targets
      '((nil :maxlevel . 1)
        (org-agenda-files :maxlevel . 2)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Set to the location of your Org files on your local system
(setq org-directory "~/Dropbox/etc")

;; adds timezone to org timestamps
;; found it in https://emacs.stackexchange.com/questions/13463/specify-timezone-in-org-date-format
(setq org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M %Z>"))


(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((js . t)
   (python . t)
   (http . t)
   (shell . t)
   (sql . t)
   (sqlite . t)
   (plantuml . t)
   (dot . t)
   (php . t)
   (elasticsearch . t)
   ))


(setq org-babel-python-command "python3")


(setq org-export-latex-listings 'minted)
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")
      )
(setq org-ref-completion-library 'org-ref-ivy-cite)
(require 'org-ref)

(setq org-latex-minted-options '(("breaklines" "true")
                                 ("breakanywhere" "true")))


(setq reftex-default-bibliography '("~/Dropbox/etc/refs/references.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/Dropbox/etc/refs/bibliography/notes.org"
      org-ref-default-bibliography '("~/Dropbox/etc/refs/bibliography/references.bib")
      org-ref-pdf-directory "~/Dropbox/etc/refs/bibliography/bibtex-pdfs/")


(setq org-plantuml-jar-path
      (expand-file-name "~/Dropbox/bin/plantuml.jar"))


(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(define-key mc/keymap (kbd "<return>") nil)



;; (setq org-roam-directory "~/Dropbox/zk")
;; (add-hook 'after-init-hook 'org-roam-mode)

(use-package org-roam
      :ensure t
      :custom
      (org-roam-v2-ack t)
      (org-roam-graph-link-hidden-types '( "fuzzy" "attachment" "bbdb" "docview" "doi" "elisp"
                                         "ftp" "gnus" "help" "http" "https" "info" "irc"
                                         "mailto" "mhe" "news" "tel" "rmail" "shell"))
      (org-roam-directory (file-truename "~/Dropbox/etc"))
      (org-roam-db-location "~/Dropbox/etc/zk/org-roam.db")

      (org-roam-capture-templates 
       '(
         ("d" "default" plain "%?"
          :if-new (file+head "zk/%<%Y%m%d>-${slug}.org"
                             "#+title: ${title}\n")
          :unnarrowed t)
         ("p" "people" plain (function org-roam-capture--get-point)
          "%?"
          :if-new (file+head  "ppl/%<%Y%m%d>-${slug}.org"
           "#+title: ${title}\n")
          :unnarrowed t)         
         ("t" "tech" plain 
          "%?"
          :if-new (file+head  "tech/%<%Y%m%d>-${slug}.org"
           "#+title: ${title}\n")
          :unnarrowed t)
         ("c" "company" plain 
          "%?"
          :if-new (file+head  "jobs/%<%Y%m%d>-${slug}.org"
           "#+title: ${title}\n")
          :unnarrowed t)         
         ("w" "workplace" plain 
          (file "~/Dropbox/etc/org/workplace_template.otpl")
          :if-new (file+head  "jobs/%<%Y%m%d>-${slug}.org"
           "#+title: ${title}\n")
          :unnarrowed t)         
         ("T" "ticket" plain 
          (file "~/Dropbox/etc/sctg/tickets/tkt-template.otpl")
          :if-new (file+head  "sctg/tickets/%<%Y%m%d>-${slug}.org"
           "#+title: ${title}\n")
          :unnarrowed t)     
         ("D" "discovery" plain 
          (file "~/Dropbox/etc/discoveries/problem-solving_template.otpl")
          :if-new (file+head  "sctg/tickets/%<%Y%m%d>-${slug}.org"
           "#+title: ${title}\n#+roam_tags: discovery\n")
          :unnarrowed t)     
         ("r" "reinvent" plain 
          (file "~/Dropbox/etc/aws/reinvent/reinvent_template.otpl")
          :if-new (file+head  "aws/reinvent/2020/%<%Y%m%d>-${slug}.org"
           "#+title: ${title}\n#+roam_tags: reinvent re2020\n")
          :unnarrowed t)     
         ("k" "trek10" plain 
          (file "~/Dropbox/etc/trek10/tkt_template.otpl")
          :if-new (file+head  "trek10/%<%Y%m%d>-${slug}.org"
           "#+title: ${title}\n")
          :unnarrowed t)     
         ("i" "t10-invoice" plain 
          (file "~/Dropbox/etc/trek10/invoice_template.otpl")
          :if-new (file+head  "trek10/invoices/%<%Y%m>-${slug}.org"
           "#+title: ${title}\n")
          :unnarrowed t)     
         ("R" "recipe" plain 
          (file "~/Dropbox/etc/recipes/recipe_template.otpl")
          :if-new (file+head  "recipes/%<%Y%m%d>-${slug}.org"
           "#+title: ${title}\n#+roam_tags: recipes\n")
          :unnarrowed t)     
         ("b" "biz - stock trading" plain 
          (file "~/Dropbox/etc/biz/biz_template.otpl")
          :if-new (file+head  "biz/%<%Y%m%d>-${slug}.org"
           "#+title: ${title}\n#+roam_tags: trading\n")
          :unnarrowed t)  
         ("f" "finance - budgeting, forecasting, expenses" plain 
          (file "~/Dropbox/etc/biz/money_template.otpl")
          :if-new (file+head  "biz/%<%Y%m%d>-${slug}.org"
           "#+title: ${title}\n#+roam_tags: finance\n")
          :unnarrowed t)  
         ("a" "adrs" plain 
          (file "~/Dropbox/etc/sctg/adrs/template.otpl")
          :if-new (file+head  "sctg/adrs/%<%Y%m%d>-${slug}.org"
           "#+title: ${title}\n")
          :unnarrowed t)     
         )
)
     :bind (("C-c n l" . org-roam-buffer-toggle)
             ("C-c n f" . org-roam-node-find)
             ("C-c n g" . org-roam-graph)
             ("C-c n i" . org-roam-node-insert)
             ("C-c n c" . org-roam-capture)
             ;; Dailies
             ("C-c n j" . org-roam-dailies-capture-today))
      :config
      (org-roam-setup)
              )


(use-package org-download
    :after org
    :defer nil
    :custom
    (org-download-method 'directory)
    (org-download-image-dir "images")
    (org-download-heading-lvl nil)
    (org-download-timestamp "%Y%m%d-%H%M%S_")
    (org-image-actual-width 300)
    (org-download-screenshot-method "/usr/local/bin/pngpaste %s")
    :bind
    ("C-M-y" . org-download-screenshot)
    :config
    (require 'org-download))


(use-package ox-hugo
  :ensure t            ;Auto-install the package from Melpa (optional)
  :after ox)


(use-package ox-slack
  :ensure t            ;Auto-install the package from Melpa (optional)
  :after ox)


(use-package ob-cypher
  :ensure t
  :config
  (add-to-list 'org-babel-load-languages '(cypher . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("cypher" . "cypher")))

(use-package org-journal
  :ensure t
  :bind
  ("C-c o j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+title: ")
  (org-journal-time-format "%H:%M %Z")
  (org-journal-file-format "%Y%m%d.org")
  (org-journal-dir "~/Dropbox/etc/journal/")
  (org-journal-date-format "%A, %d %B %Y"))
