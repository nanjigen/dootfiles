;;; desktop/exwm/config.el -*- lexical-binding: t; -*-
(load! "+funcs")
(use-package! xelb)
(use-package! exwm
  :init
  (add-hook 'exwm-mode-hook #'doom-mark-buffer-as-real-h)
  (set-popup-rule! "^\\*EXWM\\*$" :ignore t)

  (load! "+polybar")
  (setq exwm-workspace-number 7)
  ;;    (when exwm-hide-tiling-modeline
                                        ;(add-hook 'exwm-mode-hook #'hidden-mode-line-mode))
  (setq exwm-input-line-mode-passthrough t)

 (load! "+settings")


  :config
  ;; make sure that displaying transient states gets the keyboard input.
  ;; Borrowed from: https://github.com/abo-abo/hydra/issues/232
  ;; (define-advice hydra-set-transient-map (:around (fun keymap on-exit &optional foreign-keys) exwm-passthrough)
  ;;   (setq exwm-input-line-mode-passthrough t)
  ;;   (let ((on-exit (lexical-let ((on-exit on-exit))
  ;;                    (lambda ()
  ;;                      (setq exwm-input-line-mode-passthrough nil)
  ;;                      (when on-exit (funcall on-exit))))))
  ;;     (funcall fun keymap on-exit foreign-keys)))

  ;; (exwm-input-set-key (kbd "<s-return>")
  ;; (lambda ()
  ;; (interactive)
  ;; (start-process-shell-command exwm-terminal-command nil exwm-terminal-command)))

(add-hook 'exwm-update-class-hook
          (defun my-exwm-update-class-hook ()
            (unless (or (string-prefix-p "Wine" exwm-class-name))
              (exwm/rename-buffer))))

(add-hook 'exwm-update-title-hook
          (defun my-exwm-update-title-hook ()
            (unless (or (string-prefix-p (rx bol "home" eol) exwm-title)
                        (string-prefix-p (rx bol "neuron" eol) exwm-title)
                        (string-prefix-p (rx (| (seq (or "Topic" "Element") space "#" (one-or-more digit) ":") (seq "Element data"))) exwm-title)
                        ;; The below may be necessary
                        ;; (string-prefix-p (rx (seq "Mousepad")) exwm-title))
                        (string-prefix-p "Wine" exwm-class-name))
              (exwm/rename-buffer))))

                                          ;    (add-hook 'exwm-update-class-hook 'exwm/rename-buffer)
                                          ;    (add-hook 'exwm-update-title-hook 'exwm/rename-buffer)

  ;; kick all exwm buffers into insert mode per default
  (add-hook 'exwm-manage-finish-hook 'exwm/enter-insert-state)

  ;; Quick swtiching between workspaces
  (defvar exwm-toggle-workspace 0
    "Previously selected workspace. Used with `exwm/jump-to-last-exwm'.")

  ;; Buffer switching settings:
  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-layout-show-all-buffers t)

  (defadvice exwm-workspace-switch (before save-toggle-workspace activate)
    (setq exwm-toggle-workspace exwm-workspace-current-index))

  ;; `exwm-input-set-key' sets global key bindings, independent of char mode, line mode, and line mode passthru

  ;; + We always need a way to get to normal state if we are in insert state.
  (exwm-input-set-key (kbd "s-<escape>") 'exwm/escape)

  (exwm-input-set-key (kbd "<s-tab>") #'exwm/jump-to-last-exwm)
  ;; + Set shortcuts to switch to a certain workspace.
  (exwm-input-set-key (kbd "s-1")
                      (lambda () (interactive) (exwm-workspace-switch 0)))
  (exwm-input-set-key (kbd "s-2")
                      (lambda () (interactive) (exwm-workspace-switch 1)))
  (exwm-input-set-key (kbd "s-3")
                      (lambda () (interactive) (exwm-workspace-switch 2)))
  (exwm-input-set-key (kbd "s-4")
                      (lambda () (interactive) (exwm-workspace-switch 3)))
  (exwm-input-set-key (kbd "s-5")
                      (lambda () (interactive) (exwm-workspace-switch 4)))
  (exwm-input-set-key (kbd "s-6")
                      (lambda () (interactive) (exwm-workspace-switch 5)))
  (exwm-input-set-key (kbd "s-7")
                      (lambda () (interactive) (exwm-workspace-switch 6)))
  (exwm-input-set-key (kbd "s-8")
                      (lambda () (interactive) (exwm-workspace-switch 7)))
  (exwm-input-set-key (kbd "s-9")
                      (lambda () (interactive) (exwm-workspace-switch 8)))
  (exwm-input-set-key (kbd "s-0")
                      (lambda () (interactive) (exwm-workspace-switch 9)))

  ;; in normal state/line mode, use the familiar i key to switch to input state
  ;;  (evil-define-key 'normal exwm-mode-map (kbd "i") 'exwm-input-release-keyboard)
  ;; (evil-define-key 'normal exwm-mode-map
  (map! :map exwm-mode-map
        :n "i" #'exwm/enter-insert-state)
  ;; (push ?\i exwm-input-prefix-keys)
  ;; (push ?\  exwm-input-prefix-keys)
  ;;      (kbd "i") #'exwm/enter-insert-state)
  ;; (map! :map exwm-mode-map
  ;;       :n "i" #'exwm-input-release-keyboard)

  ;;; Some programs escape EXWM control and need be tamed.  See
  ;; https://github.com/ch11ng/exwm/issues/287
  ;; (add-to-list 'exwm-manage-configurations '((string= exwm-title "WineDesktop - Wine desktop")
  ;;                                            (string= exwm-title "Wine") managed t))

  ;; (add-to-list 'exwm-manage-configurations '((string= exwm-class-name "WineDesktop - Wine desktop") managed t))
  ;; (add-to-list 'exwm-manage-configurations '((string= exwm-class-name "Explorer.exe/Default - Wine desktop") managed t))
  ;; (add-to-list 'exwm-manage-configurations '((string= exwm-class-name "Wine") managed t))
  ;; (add-to-list 'exwm-manage-configurations '((string= exwm-class-name "explorer.exe") managed t))
  (add-to-list 'exwm-manage-configurations '((string= exwm-class-name "sm18.exe") managed t floating nil))
  (add-to-list 'exwm-manage-configurations '((string= exwm-class-name "Mousepad") managed t floating nil))
  (add-to-list 'exwm-manage-configurations '((string= exwm-class-name "Wine") managed t floating nil))

  ;; (push '((lambda (buffer-name action)
  ;; (and (string= buffer-name "*Help*")
  ;; (> (exwm-workspace--count) 2)))
  ;; (lambda (buffer alist)
  ;; (with-selected-frame (elt exwm-workspace--list 2)
  ;; (display-buffer-same-window buffer alist))))
  ;; display-buffer-alist)
  ;;

(load! "+sm-window-rules")
  ;; (use-package! dbc)

  ;; (add-hook 'exwm-manage-finish-hook
  ;; (lambda ()

  ;; (add-to-list 'display-buffer-alist
  ;;  `(,(string-match-p "Mousepad" (buffer-name))
  ;;    (display-buffer-below-selected)
  ;;    (side . bottom)
  ;;    ;; (slot . 0)
  ;;    (window-height . 0.19)))


  ;; (add-to-list 'display-buffer-alist
  ;;  `(("Mousepad"
  ;;    (display-buffer-below-selected)
  ;;    (side . bottom)
  ;;    ;; (slot . 0)
  ;;    (window-height . 0.19))))

  ;; (add-to-list 'display-buffer-alist
  ;;              `(,(rx (seq "Mousepad"))
  ;;                (display-buffer-reuse-window
  ;;                 display-buffer-below-selected)
  ;;                ;; (reusable-frames . visible)
  ;;                ;; (side . bottom)
  ;;                ;; (slot . 0)
  ;;                (window-height . 0.19)))


  ;; (add-hook 'exwm-manage-finish-hook
  ;;           (lambda ()
  ;;             ;; (when (string-match-p "neuron$" (buffer-name))
  ;;             ;;   (display-buffer-in-side-window (current-buffer)
  ;;             ;;                                  '((side . left)
  ;;             ;;                                    (slot . 0)
  ;;             ;;                                    (window-width . 0.23))))
  ;;             (when (string-match-p
  ;;                    (rx (| (seq (or "Topic" "Element") space "#" (one-or-more digit) ":")
  ;;                       (seq "Element")))
  ;;                (buffer-name))
  ;;               (display-buffer-in-side-window (current-buffer)
  ;;                                              '((side . bottom)
  ;;                                                (slot . 0)
  ;;                                                (window-height . 0.16))))
  ;;             ;; (when (string-match-p "" (buffer-name))
  ;;             ;;   (display-buffer-at-bottom (current-buffer)
  ;;             ;;                                  '((slot . 0)
  ;;             ;;                                    (window-height . 0.13))))
  ;;             ))


  ;; (add-hook 'exwm-manage-finish-hook
  ;;           (lambda ()
  ;;             (when (string-match-p "Mousepad" (buffer-name))
  ;;               (display-buffer-in-side-window (current-buffer)
  ;;                                              '((side . bottom)
  ;;                                                (slot . 0)
  ;;                                                (window-height . 0.16))))
  ;;             ))

  ;; FIXME See if I can get this randr code working without affecting the above.
  (use-package! exwm-randr
    :config
    (setq exwm-randr-workspace-output-plist '(0 "HDMI1" 1 "DP2"))
    ;; (setq exwm-monitor-list '("HDMI1" "DP2"))
    ;; https://github.com/ch11ng/exwm/issues/202#issuecomment-559222831
    ;; (setq exwm-workspace-name-alist '((0 . "Dashboard")
    ;;                                   (1 . "Code")
    ;;                                   (2 . "Comms")
    ;;                                   (3 . "Translation")
    ;;                                   (4 . "Study")
    ;;                                   (5 . "Reading")
    ;;                                   (6 . "Extra")))

    ;; (setq exwm-workspace-monitor-alist '(("Dashboard" . "HDMI1")
    ;;                                      ("Code" . "HDMI1")
    ;;                                      ("Comms" . "HDMI1")
    ;;                                      ("Translation" . "HDMI1")
    ;;                                      ("Study" . "HDMI1")
    ;;                                      ("Reading" . "DP2")
    ;;                                      ("Extra" . "DP2")))

    ;; assign programs to workspaces
    ;; https://emacs.stackexchange.com/questions/33107/in-exwm-emacs-x-window-manager-how-can-i-assign-apps-to-particular-workspaces
    ;; (setq exwm-manage-configurations
    ;;       '(((equal exwm-class-name "Anki")
    ;;          workspace (car (rassoc "Study" exwm-workspace-name-alist)))))

    ;; (setq exwm-manage-configurations
    ;;       '(((equal exwm-class-name "Anki")
    ;;          workspace 4)))

    ;; (defun update-exwm-randr-workspace-monitor-plist ()
    ;;       "Update exwm-randr-workspace-monitor-plist based on the current
    ;;        value of exwm-workspace-monitor-alist"
    ;;       (setq exwm-randr-workspace-monitor-plist (mapcan (lambda (workspace->monitor)
    ;;                                                          (let ((workspace-number (car (rassoc (car workspace->monitor)
    ;;                                                                                                exwm-workspace-name-alist)))
    ;;                                                                 (monitor (cdr workspace->monitor)))
    ;;                                                            (list workspace-number monitor)))
    ;;                                                        exwm-workspace-monitor-alist)))
    ;; (update-exwm-randr-workspace-monitor-plist)

    (add-hook 'exwm-randr-screen-change-hook
              (lambda ()
                (start-process-shell-command
                 ;; "xrandr" nil "xrandr --output HDMI1 --primary --mode 1920x1080_60.00 --pos 900x0 --rotate normal")))
                 "xrandr" nil "xrandr --output HDMI1 --primary --mode 1920x1080 --rotate normal --output DP2 --left-of HDMI1 --mode 1440x900 --rotate left"))))

  ;; (setq exwm-randr-workspace-monitor-plist '())

  (exwm-randr-enable)

  ;; (use-package! exwm-systemtray
  ;;   :after exwm
  ;;   :config
  ;;   (setq exwm-systemtray-height 18))
  ;; (exwm-systemtray-enable)
  ;; (display-battery-mode 1)
  ;; (display-time-mode 1)

  (exwm-enable)

;; (setq exwm-input-prefix-keys
;;       '(
;;         ;; ?\C-x
;;         ?\M-x
;;         ;; ?\M-m
;;         ;; ?\C-g
;;         ;; ?\C-m
;;         ;; ?\C-h
;;         ;; ?\C-р                         ; cyrillic
;;         ))

;; (use-package! exwm-firefox-evil
;;   :defer t
;;   :config
;;   (add-to-list 'exwm-firefox-evil-firefox-class-name "firefox")
;;   (add-hook 'exwm-manage-finish-hook 'exwm-firefox-evil-activate-if-firefox))

;; (use-package! +exwm-sm-evil
;; (add-hook 'exwm-manage-finish-hook 'exwm-sm-evil-activate-if-sm))

;; Define super-space as default leader key.
(exwm-input-set-key (kbd "s-SPC") doom-leader-map)

;; EXWM does not bypass exwm-mode-map keybindings in line-mode, so the
;; default bindings are still mapped to C-c.  We remap that to C-s-c.

;; (define-key exwm-mode-map (kbd "C-s-c") (lookup-key exwm-mode-map (kbd "C-c")))
;; (define-key exwm-mode-map (kbd "C-c") nil)

;; Use s-q to close buffers
(exwm-input-set-key (kbd "s-q") 'kill-this-buffer)

;; Basic Bindings
;; (exwm-input-set-key (kbd "s-DEL") #'reboot)
(exwm-input-set-key (kbd "s-~") #'helm-unicode)
;; (exwm-input-set-key (kbd "s-p") #'hide-panel)
(exwm-input-set-key (kbd "s-I") #'proced)
;; (exwm-input-set-key (kbd "s-n") #'ir-hydra/body)
;; (exwm-input-set-key (kbd "s-N") #'org-journal-new-entry)
;; (exwm-input-set-key (kbd "s-N") #'elfeed)
;; (exwm-input-set-key (kbd "s-W") #'nmtui)
(exwm-input-set-key (kbd "s-v") #'exwm-floating-toggle-floating)
;; (exwm-input-set-key (kbd "s-<down-mouse-1>") #'exwm-input-move-event)
(exwm-input-set-key (kbd "s-e") #'mu4e)
(exwm-input-set-key (kbd "s-o") #'ace-link)

;; TODO possibly have N for journal entries and another n for somekind of popup/dropdown notetaking
(exwm-input-set-key (kbd "s-<f1>") #'helm-info)
;; (exwm-input-set-key (kbd "s-<f4>") #'wttrin)
;; (exwm-input-set-key (kbd "s-<f6>") #'transmission)
;; (exwm-input-set-key (kbd "s-<f8>") #'scrot)
;; (exwm-input-set-key (kbd "s-m") #'launch-mpsyt)
;; (exwm-input-set-key (kbd "s-m") #'major-mode-hydra)
;; (define-key exwm-mode-map (kbd "s-v") #'exwm-floating-toggle-floating)

;; (exwm-input-set-key (kbd "s-:") 'helm-M-x)
;; (exwm-input-set-key (kbd "s-;") 'evil-ex)
;; (exwm-input-set-key (kbd "s-g") 'bookmark-jump)
;; Shell (not a real one for the moment)

;;    (push (concat (getenv "HOME") "/.local/share/applications/") counsel-linux-apps-directories)
;;   (defun exwm/counsel-linux-app-format-function (name comment exec)
;;     "Default Linux application name formatter.
;; NAME is the name of the application, COMMENT its comment and EXEC
;; the command to launch it."
;;     (format "% -45s %s"
;;             (propertize name 'face 'font-lock-builtin-face)
;;             (or comment "")))
;;   (setq counsel-linux-app-format-function #'exwm/counsel-linux-app-format-function)
;;
(exwm-input-set-key (kbd "s-d") #'counsel-linux-app)
;; possible use helm because custom actions
;; (exwm-input-set-key (kbd "s-D") #'helm-run-external-command)
(exwm-input-set-key (kbd "s-y") #'org-agenda)
(exwm-input-set-key (kbd "s-a") #'calc)
;; (exwm-input-set-key (kbd "s-x") #'xflock4)
;; (exwm-input-set-key (kbd "s-X") #'logout-menu)
;; (exwm-input-set-key (kbd "s-I") #'launch-htop)
;; (exwm-input-set-key (kbd "s-r") #'launch-ranger)
;; (exwm-input-set-key (kbd "s-r") #'exwm-reset)
(exwm-input-set-key (kbd "s-c") #'org-capture)
;; (global-set-key (kbd "s-w") #'exwm/app-launcher ('firefox))

;; (use-package! uim)

;; (uim-im-switch 'direct)　
;; (uim-im-switch 'mozc)
;; (uim-im-switch 'm17n-en-ispell)　


;; TODO get this working for helm-brain
;; https://occasionallycogent.com/emacs_custom_helm_actions/index.html
;; (after! org-brain
;;   :config
;;   (defun cogent/switch-to-buffer-split-vert (name)
;;     (select-window (split-window-right))
;;     (switch-to-buffer name))
;;   (defun cogent/switch-to-buffer-split-horiz (name)
;;     (select-window (split-window-below))
;;     (switch-to-buffer name))

;;   (defun cogent/helm-brain-switch-vert (parsed-output &optional highlight-matches)
;;     (let ((helm-brain-display-buffer-normal-method #'cogent/switch-to-buffer-split-vert))
;;       (helm-brain--async-action parsed-output highlight-matches)))
;;   (defun cogent/helm-brain-switch-horiz (parsed-output &optional highlight-matches)
;;     (let ((helm-brain-display-buffer-normal-method #'cogent/switch-to-buffer-split-horiz))
;;       (helm-brain--async-action parsed-output highlight-matches)))

;;   ;; helm-brain defines the source when it's loaded, so we can add the action
;;   ;; right away
;;   (helm-add-action-to-source
;;    "Open in horizontal split `C-s'" #'cogent/helm-brain-switch-horiz
;;    helm-brain-process-source)
;;   (helm-add-action-to-source
;;    "Open in vertical split `C-v'" #'cogent/helm-brain-switch-vert
;;    helm-brain-process-source)

;;   (defun cogent/helm-brain-switch-vert-command ()
;;     (interactive)
;;     (with-helm-alive-p
;;       (helm-exit-and-execute-action #'cogent/helm-brain-switch-vert)))
;;   (defun cogent/helm-brain-switch-horiz-command ()
;;     (interactive)
;;     (with-helm-alive-p
;;       (helm-exit-and-execute-action #'cogent/helm-brain-switch-horiz)))

;;   (general-def helm-brain-map
;;     "C-<enter>" #'cogent/helm-brain-switch-horiz-command
;;     "C-v" #'cogent/helm-brain-switch-vert-command))


;; (exwm-input-set-key (kbd "s-\\") #'exwm/multi-org-brain-helm)
(exwm-input-set-key (kbd "s-\\") #'helm-brain)

;; (defun exwm/helm-org-rifle-gtd ()
;;   "A rifle for my gtd files"
;;   (interactive)
;;   (helm-org-rifle-files (list "~/org/personal.org"
;;                               "~/org/work.org"
;;                               "~/org/inbox.org"
;;                               "~/org/someday.org")))

;; (exwm-input-set-key (kbd "s-|") #'exwm/helm-org-rifle-gtd)
;; (exwm-input-set-key (kbd "s-?") #'helm-bibtex)
;; Window management
;; (use-package! ace-window
;;   :init
;;   (setq ace-window-display-mode t))
;; Undo window configurations
;; (exwm-input-set-key (kbd "s-U") #'winner-undo)
(exwm-input-set-key (kbd "s-u") #'+eshell/toggle)
;; (exwm-input-set-key (kbd "s-R") #'winner-redo)
(exwm-input-set-key (kbd "s-`") #'+popup/toggle)
;; Change buffers
;; (exwm-input-set-key (kbd "s-<space>") #'helm-M-x)
(exwm-input-set-key (kbd "s-b") #'helm-mini) ;; try excluding EXWM buffers
(exwm-input-set-key (kbd "s-B") #'helm-exwm)
;; Focusing windows
;; (exwm-input-set-key (kbd "<s-f9>") #'exwm-sm-core-down)
(exwm-input-set-key (kbd "s-<f8>") #'exwm-sm-core-test)
;; (exwm-input-set-key (kbd "<f7>") #'evil-window-left)
(exwm-input-set-key (kbd "s-h") #'evil-window-left)
(exwm-input-set-key (kbd "s-j") #'evil-window-down)
(exwm-input-set-key (kbd "s-k") #'evil-window-up)
(exwm-input-set-key (kbd "s-l") #'evil-window-right)
(exwm-input-set-key (kbd "s-\/") #'split-window-below)
(exwm-input-set-key (kbd "s-<return>") #'split-window-right)
;; Moving Windows
(exwm-input-set-key (kbd "s-H") #'evil-window-move-far-left)
(exwm-input-set-key (kbd "s-J") #'evil-window-move-very-bottom)
(exwm-input-set-key (kbd "s-K") #'evil-window-move-very-top)
(exwm-input-set-key (kbd "s-L") #'evil-window-move-far-right)
;; Resize
(exwm-input-set-key (kbd "s-Y") #'enlarge-window-horizontally)
(exwm-input-set-key (kbd "s-U") #'shrink-window)
(exwm-input-set-key (kbd "s-I") #'enlarge-window)
(exwm-input-set-key (kbd "s-O") #'shrink-window-horizontally)
(exwm-input-set-key (kbd "s-f") #'doom/window-maximize-buffer)

;; Workspaces
(exwm-input-set-key (kbd "s-]") #'next-buffer)
(exwm-input-set-key (kbd "s-[") #'previous-buffer)

(exwm-input-set-key (kbd "s-'") #'helm-bookmarks)
;; (exwm-input-set-key (kbd "s-i") #'exwm-edit--compose)
(exwm-input-set-key (kbd "s--") #'pulseaudio-control-decrease-volume)
(exwm-input-set-key (kbd "s-=") #'pulseaudio-control-increase-volume)
(exwm-input-set-key (kbd "s-0") #'pulseaudio-control-toggle-current-sink-mute)
;; (exwm-input-set-key (kbd "s--") #'amixer-down)
;; (exwm-input-set-key (kbd "s-=") #'amixer-up)
(exwm-input-set-key (kbd "<XF86AudioMute>") #'pulseaudio-control-toggle-current-sink-mute)

;;   (use-package xbacklight
;; :bind (("<XF86MonBrightnessUp>" . xbacklight-increase)
;;        ("<XF86MonBrightnessDown>" . xbacklight-decrease)))

(exwm-enable))
