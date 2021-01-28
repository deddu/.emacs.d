(use-package elfeed
:ensure t
:bind (
"C-x w" . elfeed
)
:custom (
elfeed-feeds '(
("https://lobste.rs/rss" lob)
)
))
