;;(elpy-enable)

(require 'lsp-mode)
(add-hook 'python-mode-hook #'lsp-deferred)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-prefer-capf t )

(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.3)
(setq company-minimum-prefix-length 2)

(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))
