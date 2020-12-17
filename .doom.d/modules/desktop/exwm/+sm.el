(use-package! dbc
  :config
(dbc-add-ruleset "bottom" '((display-buffer-reuse-window display-buffer-in-side-window) .
                            ((side . bottom) (slot . 0) (window-height . 0.23))) 10)
