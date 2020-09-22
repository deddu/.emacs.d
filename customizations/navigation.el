;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.


;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)


;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)
(global-set-key (kbd "C-c c") 'counsel-compile)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c L") 'counsel-git-log)
(global-set-key (kbd "C-c k") 'counsel-rg)
;; (global-set-key (kbd "C-c m") 'counsel-linux-app) not on linux, not used, cmd+space on mac works well enough
(global-set-key (kbd "C-c m") 'counsel-fzf)
;; (global-set-key (kbd "C-c l") 'counsel-locate)
(global-set-key (kbd "C-c J") 'counsel-file-jump)
;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "C-c w") 'counsel-wmctrl)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
;; (global-set-key (kbd "C-c b") 'counsel-bookmark) i like org buffer more
(global-set-key (kbd "C-c d") 'counsel-descbinds)
(global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c o") 'counsel-outline)
(global-set-key (kbd "C-c t") 'counsel-load-theme)
(global-set-key (kbd "C-c F") 'counsel-org-file)

(require 'ivy-rich)
(ivy-rich-mode 1)
'(ivy-switch-buffer
  (:columns
   ((ivy-switch-buffer-transformer (:width 30))    ; add face by the original transformer
    (ivy-rich-switch-buffer-size (:width 7))  ; return buffer size
    (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))  ; return buffer indicator
    (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))            ; return major mode info
    (ivy-rich-switch-buffer-project (:width 15 :face success))               ; return project name `projectile'
    (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
   :predicate
   (lambda (cand) (get-buffer cand)))
  counsel-find-file
  (:columns
   ((ivy-read-file-transformer)
    (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))))
  counsel-M-x
  (:columns
   ((counsel-M-x-transformer (:width 40))
    (ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))) ; return docstring of the command
  counsel-describe-function
  (:columns
   ((counsel-describe-function-transformer (:width 40))
    (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return docstring of the function
  counsel-describe-variable
  (:columns
   ((counsel-describe-variable-transformer (:width 40))
    (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))  ; return docstring of the variable
  counsel-recentf
  (:columns
   ((ivy-rich-candidate (:width 0.8))
    (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))  ; return last modified time of the file
  package-install
  (:columns
   ((ivy-rich-candidate (:width 30))
    (ivy-rich-package-version (:width 16 :face font-lock-comment-face))  ; return package version
    (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))  ; return archive summary
    (ivy-rich-package-install-summary (:face font-lock-doc-face)))))  ; return package description


(defun ivy-rich-switch-buffer-icon (candidate)
  (with-current-buffer
      (get-buffer candidate)
    (let ((icon (all-the-icons-icon-for-mode major-mode)))
      (if (symbolp icon)
          (all-the-icons-icon-for-mode 'fundamental-mode)
        icon))))

(setq ivy-rich-display-transformers-list
      '(ivy-switch-buffer
        (:columns
         ((ivy-rich-switch-buffer-icon (:width 2))
          (ivy-rich-candidate (:width 30))
          (ivy-rich-switch-buffer-size (:width 7))
          (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
          (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
          (ivy-rich-switch-buffer-project (:width 15 :face success))
          (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
         :predicate
         (lambda (cand) (get-buffer cand)))))

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ivy-rich-path-style 'abbrev)




;; projectile everywhere!
(projectile-global-mode)

;; magit 
(global-set-key (kbd "C-x g") 'magit-status)
