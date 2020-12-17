;;; lang/org-private/+ir.el -*- lexical-binding: t; -*-

;; Dynamic reading que
;; Use org-mode and agenda to generate a reading list. Write functions
;; to dynamically update list based on last read + priority

;; Key binds etc
(use-package! major-mode-hydra
  :config
  (major-mode-hydra-define pdf-view-mode
    (:title "Incremental Reading Options")
    ("Reading qeue"
     ;; There should a hydra that moves to the next item in the reading que
     (("n" evil-collection-pdf-view-next-line-or-next-page "next")
      ("p" evil-collection-pdf-view-previous-line-or-previous-page "previous"))
     "Test"
     (("s" isearch-forward "search"))
     ;; "Priority"
     ;; Priority setting options
     ;;TODO Should define some functions
     "Drill"
     (("d" nanjigen/org-drill-in-ir-dir "org-drill items")
      ("D" org-drill-resume "Resume last org-drill"))
     )))

  (pretty-hydra-define ir-hydra
    (:color blue :title "Incremental Reading")
    ("Reading que"
     ;; There should a hydra that moves to the next item in the reading que
     (("n" evil-collection-pdf-view-next-line-or-next-page "next")
      ("p" evil-collection-pdf-view-previous-line-or-previous-page "previous"))
     "org-noter"
     (("o" follow-noter-page-link "follow org-pdftools link")
      ;; Should this be a function in a specific workspace?
      ("N" org-noter "Start org-noter session")
      ("S" org-noter-pdftools-create-skeleton "Create org-noter outline"))
     ;; "Priority"
     ;; Priority setting options
     ;;TODO Should define some functions
     "Drill"
     (("d" nanjigen/org-drill-in-ir-dir "org-drill items")
      ("D" org-drill-resume "Resume last org-drill"))
     ))


(map! :localleader
      :map pdf-view-mode-map
          :desc "Insert to Org" "h" 'nanjigen/org-noter-highlight-drill)

;; This is to initiate drill from anywhere
;; Have next PDF fire off instead if all done?
(defun nanjigen/org-drill-in-ir-dir ()
  (interactive)
  (with-current-buffer (find-file-noselect "~/org/article-notes/article-index.org")
    (org-drill)))

;; Capturing highighted text from a PDF
;; [[file:~/.emacs.d/.local/straight/repos/org-noter/org-noter.el::defun org-noter-insert-note (&optional precise-info][org-noter-insert-note]]
(defun nanjigen/org-noter-highlight-drill ()
  "Extract highlighted text into org-noter buffer as org-drill item"
  (interactive)
  (save-window-excursion
    (org-noter-insert-note-toggle-no-questions)
  ;; Now I need to move the header text (which is the extracted pdf text) into the body
    (nanjigen/move-headline-to-contents)
  ;; Then I need to populate the Header with faux text
    ;; (nanjigen/chopper)
    (org-toggle-tag "drill")))

(defun nanjigen/move-headline-to-contents ()
  "Move extracted PDF text to body of subtree"
  (interactive)
  (org-back-to-heading)
  (let* ((headline (org-element-at-point))
         (title (org-element-property :title headline))
         (property-end (org-element-property :contents-end headline))
         ;; (property (org-element-at-point (goto-char property-pos)))
         (indent (org-element-property :level headline))
         (title-start (+ indent (org-element-property :begin headline)))
         (title-end (- (org-element-property :contents-begin headline) 1))
         (id (car (last (s-split ";;" (org-entry-get nil "NOTER_PAGE")))))
         (id-clean (s-chop-suffix "]]" id)))
     ;; (when (eq 'property-drawer (car property))
       ;; (goto-char (org-element-property :end property)))
    (goto-char property-end)
    (insert title "\n")
    (delete-region title-start title-end)
    (goto-char title-start)
    ;;HACK use logic to identify if parent is an org-noter-outline headline
    ;; and then indent with org-mode code, not a hacky *!
    (insert (format "* %s" id-clean)))
  (org-back-to-heading))

(defun nanjigen/chopper ()
  "get 'annot' id from 'ID' property"
  (interactive)
  (let* ((id (car (last (s-split ";;" (org-entry-get nil "NOTER_PAGE")))))
         (id-clean (s-chop-suffix "]]" id)))
    (insert (format "%s" id-clean))))

(defun nanjigen/delete-headline ()
  "delete the original title of text extracted with
(org-noter-insert-note-toggle-no-questions)"
  (interactive)
  (let* ((headline (org-element-at-point))
         (title-start (org-element-property :contents-begin headline))
         (title-end (org-element-property :end headline)))
    (delete-region title-start title-end)))

;; Functions for capturing from org noter
  (defun nanjigen/get-noter-link ()
    "return PROPERTY value of pdftools link"
    (interactive)
    (let ((linkStr (org-entry-get nil "NOTER_PAGE")))
      (insert linkStr)))

  ;;https://emacs.stackexchange.com/questions/32283/how-to-capture-and-link-code-comments-as-org-agenda-items
  (defun capture-noter-link ()
    "insert PROPERTY value of pdftools link"
    (interactive)
    (let ((linkStr
            (save-excursion
              (save-window-excursion
                (switch-to-buffer (plist-get org-capture-plist :original-buffer))
                (car (org-get-outline-path)))
              )))
      ))


  (defun insert-key (&optional p)
    "insert header as key value of org-brain header"
    ;; (unless p (setq p "NOTER_PAGE"))
    ;; (message "property passed is: %s" p)
    (interactive)
    (let ((pvalue
            (save-window-excursion
              (message "%s" (org-capture-get :original-buffer))
              (switch-to-buffer (org-capture-get :original-buffer))
              (message "retrieved header (key) is: %s" (car (org-get-outline-path (point) p)))
              (car (org-get-outline-path (point) p))
              )))
      pvalue))


  (defun key-to-header (&optional p)
    "insert header in org-capture target file as key value of org-brain header"
    ;; (unless p (setq p "NOTER_PAGE"))
    ;; (message "property passed is: %s" p)
    (interactive)
    (let ((heading
            (save-window-excursion
              (message "%s" (org-capture-get :original-buffer))
              (switch-to-buffer (org-capture-get :original-buffer))
              (message "retrieved header (key) is: %s" (car (org-get-outline-path (point) p)))
              (car (org-get-outline-path (point) p))
              )))
      (goto-char (org-find-exact-headline-in-buffer "IR Cards"))
      (unless (search-forward (format "** %s" heading) nil t)
        (org-end-of-subtree)
        (insert (format "\n** %s" heading))
        )))

  (defun insert-property (&optional p)
    "insert PROPERTY value of pdftools link"
    (interactive)
    (unless p (setq p "NOTER_PAGE"))
    (message "property passed is: %s" p)
    (let ((pvalue
            (save-window-excursion
              (message "%s" (org-capture-get :original-buffer))
              (switch-to-buffer (org-capture-get :original-buffer))
              (message "retrieved property is: %s" (org-entry-get (point) p))
              (org-entry-get (point) p)
              )))
      pvalue))

  (defun follow-noter-page-link ()
    "return PROPERTY value of pdftools link and follow in other-window"
    (interactive)
    ;; (run-with-timer 3 nil (lambda ()
      ;; TODO switch-to-buffer?
    (let ((linkStr (org-entry-get nil "NOTER_PAGE")))
      (if (> (length (window-list)) 1)
          (other-window 1)
        ;; (balance-windows)
          ;; (switch-to-buffer-other-window)
        (split-window-right))
      (org-link-open-from-string linkStr)))
;;

  ;; (advice-add 'org-drill-reschedule :after-until #'follow-noter-page-link)
  ;; (advice-add 'org-drill-entry-p :after #'follow-noter-page-link)
  ;; (advice-add '(org-drill-entries (session-done)) :after #'follow-noter-page-link)
  ;; (advice-add 'org-drill-goto-entry :after #'follow-noter-page-link)
  ;; (advice-add 'org-drill-present-simple-card :after #'follow-noter-page-link)
  ;; (advice-add 'org-drill-present-default-answer :after #'follow-noter-page-link)
  ;; (advice-add 'org-drill-present-two-sided-card :after #'follow-noter-page-link)
  ;; (advice-add 'org-drill--edit-key :after #'follow-noter-page-link)
  ;; (advice-add 'org-drill-response-complete-rtn :after #'follow-noter-page-link)
  ;; (advice-add 'org-drill-response-complete :after #'follow-noter-page-link)
  ;; (advice-add 'org-drill-response-quit  :after #'follow-noter-page-link)
  ;; (add-function :after ('org-drill-response-edit) #'follow-noter-page-link)
  ;; (advice-add 'org-drill-response-complete :after #'follow-noter-page-link)
  (advice-add 'org-drill-response-edit :after #'follow-noter-page-link)
  (advice-add 'org-drill-response-edit :after-while #'follow-noter-page-link)
  (advice-add 'org-drill-response-quit  :after #'follow-noter-page-link)
  ;; (advice-add 'org-drill--edit-key :after #'follow-noter-page-link)


;; This triggers the function but not at the right time
;; (add-hook 'org-drill-display-answer-hook #'follow-noter-page-link)
