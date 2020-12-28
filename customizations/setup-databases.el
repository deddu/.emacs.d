;; elastic
;;(add-to-list 'load-path "../elpa/es-mode-20191119.2018")
(autoload 'es-mode "es-mode.el"
  "Major mode for editing Elasticsearch queries" t)
(add-to-list 'auto-mode-alist '("\\.es$" . es-mode))

