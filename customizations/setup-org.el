;;;
;;; Org Mode
;;;

;(add-to-list 'load-path (expand-file-name "~/Dropbox/etc/org"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org)
;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-todo-keyword-faces
           '(("TODO" . org-warning) 
             ("WAITING" . "yellow")
             ("IN_PROGRESS" . (:foreground "black" :background "pink" ))
             ("STAGED" . "pink")
             ("PEER_REVIEW" . "yellow")
             ("WONTDO" . "purple")
             ("STARTED" . (:foreground "orange" :weight bold))))

(setq org-agenda-files
      '("~/Dropbox/todo.org" 

))

(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))

;; Set to the location of your Org files on your local system
(setq org-directory "~/Dropbox")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")


(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((js . t)
   (python . t)
   (http . t)
   ))

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
