;; -*- no-byte-compile: t; -*-
;;; desktop/exwm/packages.el

(package! xelb)
  ;; :recipe (:host github :repo "ch11ng/xelb"))
(package! exwm)
  ;; :recipe (:host github :repo "ch11ng/exwm"))

;; (package! exwm-randr)
(package! dbc)
(package! exwm-edit)
(package! dmenu)
(package! posframe)
(package! ace-link)
(package! helm-exwm)
(package! helm-org-rifle)
(package! pulseaudio-control)
;; (package! statusbar.el :recipe
;;   (:host github
;;    :repo "dakra/statusbar.el"
;;    :files ("*")))
(package! scrot.el :recipe
  (:host github
   :repo "dakra/scrot.el"
   :files ("*")))
(package! helm-unicode)
(package! exwm-firefox-core)
(package! exwm-firefox-evil)
