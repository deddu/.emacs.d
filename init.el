;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)

;; (add-to-list 'package-archives
             ;; '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives
             ;; '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("marmalade" . "http://marmalade-repo.org/packages/")
;;                          ("melpa" . "http://melpa-stable.milkbox.net/packages/")))


;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(
    ;; org with extra packages
    org-plus-contrib
    ;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
   ;; clojure-mode

    ;; extra syntax highlighting for clojure
    ;; clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
   ;; cider

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido. 3 of those ido or ivy or helm. install counsel to bring in ivy and deps.
    ;; ido-ubiquitous
    ivy
    swiper
    counsel
    ivy-rich
    
    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    ;; smex

    ;; project navigation
    projectile

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; edit html tags like sexps
    tagedit

    ;; git integration
    magit

    ;; autocomplete
    company

    ;; move text lines and block around
    move-text

    ;; http language support for babel
    ob-http
    ;; async eval for org
    ob-async
    ;; elastic search mode
    es-mode
    ;; yaml support
    yaml-mode
    ;; somehting latex related
    auctex 
    ;; cool theme
    zeno-theme 
    ;; bibtex references for org mode   
    org-ref
    ;; zettelkasten installation
    org-roam
    ;; org-roam-ui
    ;; download snapshots
    org-download
    ;; org-ref-ivy-cite
    ;; python autocompletion
    ;; elpy
    lsp-mode
    lsp-ivy
    flycheck
    py-autopep8
    blacken
    lsp-ui
    ein
    ;; other utilities
    oauth2
    ;; use-packge is a cool thing to help with package inizialization etc. 
    use-package

    ;; multi-cursors
    multiple-cursors
    ;;snippets
    yasnippet))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customization
;;;;
(setq org-element-use-cache nil)
;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; My own functions
(load "dre.el")

;; Language-specific
;;(load "setup-clojure.el")
;;(load "setup-clojurescript.el")
(load "setup-js.el")
;;(load "setup-haskell.el")
(load "setup-python.el")
(load "setup-databases.el")
(load "setup-docker.el")
;;(load "setup-clisp.el")
;;(load "rust.el")
;;(load "feeds.el")
;; OrgMode

(load "setup-org.el")
(setq org-element-use-cache nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(coffee-tab-width 2)
 '(custom-enabled-themes '(tomorrow-night-eighties zeno))
 '(custom-safe-themes
   '("cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" "7b1ea77093c438aa5887b2649ca079c896cc8780afef946d3b6c53931081a726" "81f2e389938b9117dc939845ed75ce48584dd01f870d9315d20a56d36d750423" "db5b906ccc66db25ccd23fc531a213a1afb500d717125d526d8ff67df768f2fc" "9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" "0eccc893d77f889322d6299bec0f2263bffb6d3ecc79ccef76f1a2988859419e" "ef04dd1e33f7cbd5aa3187981b18652b8d5ac9e680997b45dc5d00443e6a46e3" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "52588047a0fe3727e3cd8a90e76d7f078c9bd62c0b246324e557dfa5112e0d0c" default))
 '(fci-rule-color "#2a2a2a")
 '(org-agenda-files
   '("~/Dropbox/etc/sctg/tickets/20210701110520-purchasing_platform.org" "/Users/acadeddu/Dropbox/etc/zk/20220514-moving.org" "/Users/acadeddu/Dropbox/etc/zk/20200903162420-tinkering_forward.org" "/Users/acadeddu/Dropbox/etc/tech/20200905065702-emacsturbation.org" "/Users/acadeddu/Dropbox/etc/sctg/tickets/20210611150237-fortis_erp.org" "/Users/acadeddu/Dropbox/etc/turing/notes/metra-todo.org" "/Users/acadeddu/Dropbox/etc/jobs/20210714064517-streamguys.org" "/Users/acadeddu/Dropbox/etc/sctg/deft_todo.org" "/Users/acadeddu/Dropbox/etc/refile-beorg.org" "/Users/acadeddu/Dropbox/etc/todo.org" "/Users/acadeddu/Dropbox/etc/sctg/florence.org" "/Users/acadeddu/Dropbox/etc/recurring.org"))
 '(org-export-backends '(ascii beamer html icalendar latex md odt confluence))
 '(org-log-into-drawer t)
 '(package-selected-packages
   '(org-ql ov org-super-agenda ts which-key svg-tag-mode svg-lib nano-theme quelpa-use-package quelpa ob-svgbob use-package ein lsp-ui blacken py-autopep8 lsp-ivy org-ref es-mode ob-http move-text counsel swiper yasnippet-snippets docker-tramp oauth2 org-journal elfeed neotree rustic ob-cypher org-plus-contrib ox-hugo ob-async org-download yaml-mode ob-coffee zenburn-theme org-roam multiple-cursors plantuml-mode company-ctags lsp-mssql flycheck lsp-mode elpy all-the-icons-ivy-rich ivy-rich ivy auctex zeno-theme org-jira hindent haskell-mode base16-theme solarized-theme yasnippet tagedit smex rainbow-delimiters projectile paredit ox-jira ox-gfm org-agenda-property magit let-alist ido-ubiquitous git-rebase-mode git-commit-mode exec-path-from-shell clojurescript-mode clojure-mode-extra-font-locking cider))
 '(warning-suppress-types '((comp)))
 '(widget-image-enable nil)
 '(x-underline-at-descent-line t))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
