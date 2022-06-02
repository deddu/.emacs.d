;;;
;;; Org Mode
;;;


;(add-to-list 'load-path (expand-file-name "~/Dropbox/etc/org"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org)

(require 'ob-async)
;; (require 'ox-confluence) ;; not really working anymore


;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key (kbd "C-c C-s") 'org-schedule)
(global-set-key "\C-cb" 'org-switchb)

(setq org-todo-keyword-faces
           '(("TODO" . org-warning)
             ("WAITING" . "yellow")
             ("IN_PROGRESS" . (:foreground "black" :background "pink" ))
             ("STAGED" . "pink")
             ("PEER_REVIEW" . "yellow")
             ("WONTDO" . "purple")
             ("RECUR" . "lightblue")
             ("PARK" . "gray")
             ("STARTED" . (:foreground "orange" :weight bold))))


;;  (setq org-hide-emphasis-markers t)

;; (add-hook 'org-mode-hook 'variable-pitch-mode)

;; ;; see https://zzamboni.org/post/beautifying-org-mode-in-emacs/
;; (let* ((variable-tuple
;;           (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
;;                 ((x-list-fonts "Hack Nerd Font Mono-14") '(:font "Hack Nerd Font Mono-14"))
;;                 ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;                 ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;                 ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;                 (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;          (base-font-color     (face-foreground 'default nil 'default))
;;          (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

;;     (custom-theme-set-faces
;;      'user
;;      `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;      `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;      `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
;;      `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
;;      `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
;;      `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

;; (custom-theme-set-faces
;;    'user
;;    '(variable-pitch ((t (:family "ETBembo" :height 220))))
;;    '(fixed-pitch ((t ( :family "Hack Nerd Font Mono-14" :height 160)))))

;;  (custom-theme-set-faces
;;    'user
;;    '(org-block ((t (:inherit (shadow fixed-pitch) :extend t))))
;;    '(org-code ((t (:inherit (shadow  fixed-pitch) :extend t ))))
;; ;   '(org-document-info ((t (:foreground "dark orange"))))
;; ;   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
;;    '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
;; ;;   '(org-link ((t (:foreground "dark orange" :underline t))))
;;    '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;; ;   '(org-property-value ((t (:inherit fixed-pitch))) t)
;; ;   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;; ;   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
;;    '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.9))))
   ;; '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))


;; (use-package mixed-pitch
;;   :ensure t
;;   :hook
;;   ;; If you want it in all text modes:
;;   (text-mode . mixed-pitch-mode))


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


(define-key global-map (kbd "C-c o") 'org-capture)
(setq org-capture-templates
      `(("i" "Inbox" entry  (file "inbox.org")
        ,(concat "* TODO %?\n"
                 "ENTERED: %U")))
)




(use-package org-roam
      :ensure t
      :init (setq org-roam-v2-ack t)
      :custom
      (org-roam-graph-link-hidden-types '( "file" "fuzzy" "attachment" "bbdb" "docview" "doi" "elisp"
                                         "ftp" "gnus" "help" "http" "https" "info" "irc"
                                         "mailto" "mhe" "news" "tel" "rmail" "shell"))
      (org-roam-directory (file-truename "~/Dropbox/etc"))
      (org-roam-db-location (file-truename (concat "~/Dropbox/etc/" user-login-name ".roam.db")))
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
         ("F" "florence ticket" plain
          (file "~/Dropbox/etc/sctg/tickets/florence/flo-tkt-template.otpl")
          :if-new (file+head  "sctg/tickets/florence/%<%Y%m%d>-${slug}.org"
           "#+title: ${title}\n")
          :unnarrowed t)
         ("D" "discovery" plain
          (file "~/Dropbox/etc/discoveries/problem-solving_template.otpl")
          :if-new (file+head  "sctg/tickets/%<%Y%m%d>-${slug}.org"
           "#+title: ${title}\n#+roam_tags: discovery\n")
          :unnarrowed t)
         ("b" "presentation, talk, beamer " plain
          (file "~/Dropbox/etc/talks/talks-tpl.otpl")
          :if-new (file+head  "talks/%<%Y%m%d>-${slug}.org"
           "")
          :unnarrowed t)
         ("r" "reinvent" plain
          (file "~/Dropbox/etc/aws/reinvent/reinvent_template.otpl")
          :if-new (file+head  "aws/reinvent/2021/%<%Y%m%d>-${slug}.org"
           "#+title: ${title}\n#+roam_tags: reinvent re2021\n")
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
         ("B" "biz - stock trading" plain
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
;; how do i set a template for the daily capture?(org-roam-dailies-capture-templates (quote (("d" "default" plain (function org-roam--capture-get-point) "%?"))))
     :bind (("C-c n l" . org-roam-buffer-toggle)
             ("C-c n f" . org-roam-node-find)
             ;;("C-c n g" . org-roam-graph) ;; crashes really bad, too many links i believe
             ("C-c n i" . org-roam-node-insert)
             ("C-c n c" . org-roam-capture)
             ;; Dailies
             ("C-c n j" . org-roam-dailies-capture-today)
             :map org-mode-map
             ("C-M-i" . completion-at-point)
             :map org-roam-dailies-map
             ("Y" . org-roam-dailies-capture-yesterday)
             ("T" . org-roam-dailies-capture-tomorrow)

)
:bind-keymap
  ("C-c n d" . org-roam-dailies-map)
      :config
      (require 'org-roam-dailies)
;;      (org-roam-setup)
(org-roam-db-autosync-mode)
              )

;; (use-package org-roam-ui
;;   :after org-roam
;;   :ensure t
;;   :demand

;;   :custom
;;   (org-roam-ui-follow         t)
;;   (org-roam-ui-open-on-start  t)
;;   (org-roam-ui-sync-theme     t)
;;   (org-roam-ui-update-on-save t)
;;   )


;; this is the magic sauce to downloading screenshots.
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

;; saw it from matt
(use-package ob-svgbob
  :after org
  :ensure t
  :demand

  :config
  (add-to-list 'org-src-lang-modes '("svgbob" . artist))
  )

;; this can somehow build a blog from a file or a path.
;; i think i've a capture template for it
(use-package ox-hugo
  :ensure t            ;Auto-install the package from Melpa (optional)
  :after ox)


;;neo4j cypher support
(use-package ob-cypher
  :ensure t
  :config
  (add-to-list 'org-babel-load-languages '(cypher . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("cypher" . "cypher")))



;; org-ql
(use-package org-ql
  :quelpa (org-ql :fetcher github :repo "alphapapa/org-ql"
            :files (:defaults (:exclude "helm-org-ql.el"))))


;; (defun org-journal-file-header-func ()
;;   "Custom function to add org id to journal header."
;;   (concat
;;     (pcase org-journal-file-type
;;       (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
;;       (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded")
;;       (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
;;       (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded"))))

(defun dre-org-journal-header-gen (time)
"custom fun to create org id header for journal entries "
(org-id-get-create)
""
)


;; (use-package org-journal
;;   :ensure t
;;   :bind
;;   ("C-c o j" . org-journal-new-entry)
;;   :custom
;;   (org-journal-date-prefix "#+title: ")
;;   (org-journal-time-format "%H:%M %Z")
;;   (org-journal-file-format "%Y-%m-%d.org")
;;   (org-journal-dir "~/Dropbox/etc/daily/")
;;   (org-journal-date-format "%Y-%m-%d")
;;   (org-journal-hide-entries-p nil)
;;   (org-journal-carryover-items "+TODO='TODO'|'PARK'|'RECUR'")
;; ;  (org-journal-skip-carryover-drawers '("LOGBOOK"))
;;  (org-journal-file-header 'dre-org-journal-header-gen)
;; )
