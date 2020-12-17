;;; desktop/exwm/+sm-window-rules.el -*- lexical-binding: t; -*-
(add-hook 'exwm-update-title-hook
          (defun sm-element-test-title-hook ()
            (when (string-match (rx bol "home" eol) exwm-title)
              (exwm-workspace-rename-buffer "sm-element-window"))))
(add-hook 'exwm-update-title-hook
          (defun sm-knowledge-tree-title-hook ()
            (when (string-match (rx bol "neuron" eol) exwm-title)
              (exwm-workspace-rename-buffer "sm-knowledge-tree"))))
(add-hook 'exwm-update-title-hook
          (defun sm-element-data-title-hook ()
            (when (string-match (rx (| (seq (or "Topic" "Element") space "#" (one-or-more digit) ":") (seq "Element data"))) exwm-title)
              (exwm-workspace-rename-buffer "sm-element-data"))))
