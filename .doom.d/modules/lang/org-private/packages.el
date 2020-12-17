;; -*- no-byte-compile: t; -*-
;;; desktop/lang/org-private/packages.el

;; (package! org-noter)
;; (package! org-plus-contrib)
(package! org-brain)
(package! org-ql)
(package! org-web-tools)
(package! org-super-agenda)
(package! org-superstar)
(package! persistent-scratch)
(package! org-pdftools)
(package! org-noter-pdftools)
(package! org-sidebar)
;; Because of the link error:
;; (package! org-pdftools :recipe
;;   (:host github
;;    :repo "fbanados/org-pdftools"
;;    :files ("org-pdftools.el")))
;; (package! org-noter-pdftools :recipe
;;   (:host github
;;    :repo "fbanados/org-pdftools"
;;    :files ("org-noter-pdftools.el")))
;; (package! org-pdftools :recipe (:host github :repo "fuxialexander/org-pdftools" :files ("org-pdftools.el")))
;; (package! org-noter-pdftools :recipe (:host github :repo "fuxialexander/org-pdftools" :files ("org-noter-pdftools.el")))
;; (package! org-noter)
(package! toc-mode)
(package! org-ref)
(package! helm-bibtex)
(package! helm-lib-babel)
(package! powerthesaurus)
(package! ox-reveal)
(package! org-drill)
(package! org-cliplink)
(package! ascii-art-to-unicode)
(package! link-hint)
(package! major-mode-hydra)
(package! abridge-diff)
