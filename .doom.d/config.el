;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;(setq user-full-name "Daniel Otto"
      ;user-mail-address (password-store-get "Email/personal/hotmail"))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Fira Code" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq user-home-directory "~/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
;; (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(defconst doom-frame-transparency 94)
(set-frame-parameter (selected-frame) 'alpha doom-frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,doom-frame-transparency))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; If this isn't set EXWM quadruples its memory usage
(setq gc-cons-threshold 100000000)

;; Some custom binds
;; (map! :leader
;;   (:prefix-map ("c" . "code")
;;     :desc "Comment" "l" #'
;;   ))
(setq +zen-text-scale 0.6)

;;taken from https://tecosaur.github.io/emacs-config/config.html#writeroom
(after! writeroom-mode
  (add-hook 'writeroom-mode-hook
            (defun +zen-cleaner-org ()
              (when (and (eq major-mode 'org-mode) writeroom-mode)
                (setq-local -display-line-numbers display-line-numbers
                            display-line-numbers nil)
                (setq-local -org-indent-mode org-indent-mode)
                (org-indent-mode -1))))
  (add-hook 'writeroom-mode-disable-hook
            (defun +zen-dirty-org ()
              (when (eq major-mode 'org-mode)
                (setq-local display-line-numbers -display-line-numbers)
                (when -org-indent-mode
                  (org-indent-mode 1)))))

  ;; (add-hook 'writeroom-mode-hook
  )

(use-package! focus
  :after writeroom-mode
  :config
(add-to-list 'focus-mode-to-thing '(writeroom-mode . paragraph)))
;; (add-hook 'write-room-mode-hook #'line-number-mode-hook)

(use-package! undo-tree
  ;; :demand t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(setq tab-bar-mode t)
;; Because elisp in a snippet complains??
;; (setq warning-supre)


(setq doom-modeline-continuous-word-count-modes '(Tex-Pdf markdown-mode))
  ;; Bookmarks file location
  (setq bookmark-default-file "~/org/bookmarks")
  (setq bookmark-save-flag 1) ;; save after every change

(setq evil-escape-mode nil)
;; (setq global-visual-line-mode t)
;; ;; (setq auto-fill-mode nil)
;; (remove-hook 'org-mode-hook #'auto-fill-mode)
;; (add-hook 'org-mode-hook #'visual-line-mode)
;; (add-hook 'visual-line-mode-hook #'org-brain-visualize-mode)

(use-package! helm-posframe
  :after helm
  :custom-face (internal-border ((t (:background "#c678dd"))))
  :config
(setq helm-posframe-poshandler 'posframe-poshandler-frame-center)
(setq helm-posframe-parameters '((parent-frame nil)
                                 (left-fringe . 10)
                                 (right-fringe . 10)))
(helm-posframe-enable)
)

(use-package! ivy-posframe
  :after ivy
  :custom-face (internal-border ((t (:background "#c678dd"))))
  :config
(setq ivy-posframe-poshandler 'posframe-poshandler-frame-center)
(setq ivy-posframe-parameters '((parent-frame nil)
                                 (left-fringe . 10)
                                 (right-fringe . 10)))
(ivy-posframe-mode 1)
)

(setq scrot-local-path "~/Pictures/screenshots")
(setq eww-download-directory "~/Downloads/eww")



;; (map! "<f8>" #'scrot)
(setq display-battery-mode nil)

(load! "+mail")

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;; (after! persp-mode
;; (add-hook 'exwm-mode #'doom-mark-buffer-as-real-h))

;; File handling
;; (use-package! openwith
;;   :defer t
;;   (setq openwith-associations '(("\\.odt\\'" "libreoffice" (file))
;;                                 ("\\.mp4\\'" "mpv" (file))
;;                                 ("\\.mpg\\'" "mpv" (file)))))
;;   (openwith-mode t)


;; (use-package! pocket-reader
  ;; :defer-incrementally t)

;; Calendar
(use-package! password-store
  :init
  (setq auth-sources '(password-store
                       "~/.authinfo.gpg")))

;(use-package! auth-source-pass)
;(auth-source-pass-enable)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (focus darkroom symbol-navigation-hydra org-sidebar link-hint))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
