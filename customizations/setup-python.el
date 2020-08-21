;;(elpy-enable)

(require 'lsp-mode)
(add-hook 'python-mode-hook #'lsp-deferred)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-prefer-capf t )

(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

