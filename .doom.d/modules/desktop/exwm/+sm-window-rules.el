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

(add-to-list 'exwm-manage-configurations '((string= exwm-title (rx bol "Concepts")) managed t floating t))
(add-hook 'exwm-update-title-hook
          (defun sm-element-data-title-hook ()
            (when (string-match (rx bol "Concepts") exwm-title)
              (exwm-workspace-rename-buffer "concepts"))))

;; (use-package! dbc
;; :config
;; (add-to-list 'display-buffer-alist
;;              `((,<<empty-sm>>
;;                 (display-buffer-no-window))
;;                (,<<tree-rx>>
;;                 (display-buffer-reuse-window display-buffer-in-side-window)
;;                 (side . left)
;;                 (slot . 0)
;;                 (window-width . 0.22))
;;                (,<<lower-bar-rx>>
;;                 (display-buffer-at-bottom)
;;                 ;; (side . bottom)
;;                 ;; (slot . 0)
;;                 (window-height . 0.19))))
(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (string-match-p "sm-knowledge-tree" (buffer-name))
              (display-buffer-in-side-window (current-buffer)
                                             '((side . left)
                                               (slot . 0)
                                               (window-height . 0.16))))))
(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (string-match-p "sm-element-data" (buffer-name))
              (display-buffer-in-side-window (current-buffer)
                                             '((side . bottom)
                                               (slot . 0)
                                               (window-height . 0.20))))))
(remove-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (string-match-p
                   <<lower-bar-rx>>
                   (buffer-name))
              (display-buffer-in-side-window (current-buffer)
                                             '((side . bottom)
                                               (slot . 0)
                                               (window-width . 0.23))))))
;; (dbc-add-ruleset "expose" '((display-buffer-reuse-window display-buffer-pop-up-frame) .
;;                             ((reusable-frames . 0))))
;; 
;; (dbc-add-rule "expose" "main" :newname "sm-element-window")

;; (dbc-add-ruleset "left" '((display-buffer-reuse-window display-buffer-in-side-window) .
;;                           ((side . left) (slot . -1) (window-width . 0.22))) 5)
;; 
;; (dbc-add-rule "left" "tree" :newname "sm-knowledge-tree")

;; (dbc-add-ruleset "bottom" '((display-buffer-reuse-window display-buffer-in-side-window) .
;;                             ((side . bottom) (slot . 0) (window-height . 0.23))) 10)
;; 
;; (dbc-add-rule "bottom" "tray" :newname "sm-element-data")

;; )
