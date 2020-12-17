;;; lang/org-private/config.el -*- lexical-binding: t; -*-

;; org mode
(after! org
  :init
  (setq org-archive-location (concat org-directory "archive/archive.org::* From %s"))

  :config
  (setq org-superstar-headline-bullets-list '("› "))
  (setq org-startup-indented t
        org-clock-idle-time 5
        ;; org-bullets-bullet-list '("› ")
        org-ellipsis "  "
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-agenda-block-separator ""
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-catch-invisible-edits 'show-and-error
        org-cycle-separator-lines 0
        org-tags-column -79
        org-modules '(org-bibtex
                      org-docview
                      org-habit
                      org-info))

  (setq org-image-actual-width (/ (display-pixel-width) 3))

  (add-hook 'org-mode-hook (lambda ()
                             "Beautify Org Checkbox Symbol"
                             (push '("[ ]" .  "☐") prettify-symbols-alist)
                             (push '("[X]" . "☑" ) prettify-symbols-alist)
                             (push '("[-]" . "❍" ) prettify-symbols-alist)
                             (prettify-symbols-mode)))

  (add-hook! '(org-mode-hook) #'display-line-numbers-mode)

  (defface org-checkbox-done-text
    '((t (:foreground "#71696A")))
    "Face for the text part of a checked org-mode checkbox.")
  (add-to-list 'org-modules 'org-checklist)

  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-preceding-days 14
        ;; org-habit-following-days 1
        org-habit-graph-column 80
        org-habit-show-habits-only-for-today t
        ;; org-habit-show-all-today t)
        )

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "#ffb86c" :weight bold)
                ("NEXT" :foreground "#8be9fd" :weight bold)
                ("DONE" :foreground "#6272a4" :weight bold)
                ("IN-PROGRESS" :foreground "#50fa7b" :weight bold)
                ("PROJECT" :foreground "#0189cc" :weight bold)
                ("WAITING" :foreground "#f8f8f2" :weight bold)
                ("HOLD" :foreground "#a0522d" :weight bold)
                ("CANCELLED" :foreground "#ff5555" :weight bold)))))

(after! org
(setq org-agenda-files '("~/org/personal.org"
                           "~/org/work.org"
                           "~/org/next.org"
                           "~/org/incubation.org"
                           "~/org/org-brain/article-notes/thesis.org"
                           "~/org/ir_stack.org"
                           "~/org/danieru-cal.org"
                           "~/org/habits.org"))

;;TODO set refile to only projects?
  ;; Set refile targets
  (setq org-refile-use-outline-path 'file              ; Show full paths for refiling
        org-outline-path-complete-in-steps nil)        ; Refile in a single go
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-completion-use-ido nil)
  (setq org-refile-targets '(("~/org/next.org" :level . 0)
                             ("~/org/work.org" :maxlevel . 2)
                             ("~/org/personal.org" :maxlevel . 3)
                             ("~/org/wiki/thesis.org" :maxlevel . 3)
                             ("~/org/someday.org" :maxlevel . 2)
                             ("~/org/incubation.org" :maxlevel . 1)
                             ("~/org/read.org" :maxlevel . 2)
                             ("~/org/watch.org" :maxlevel . 2)))


  ;; Org-contacts
  (setq org-contacts-files '("~/org/contacts.org"))

  (setq org-blank-before-new-entry '((heading . nil)))

  (setq org-enable-org-journal-support t)
  (setq org-journal-dir "~/org/journal/")

  ;; Link types for org-mode
  (setq org-file-apps
        '((auto-mode . emacs)
          ("\\.mpg\\'" . "mpv %s")))

  ;; org-todo keywords with interactivity
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "|" "DONE(d)")
          (type "PROJECT(p)" "|" "DONE(d!)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-state-notes-insert-after-drawers nil)

  ;; context tags
  (setq org-tag-alist '((:startgroup)
                            ;;; Contexts
                        ("@home" . ?h)
                        ;; ("@nathans" . ?n)
                        ("@uni" . ?u)
                        ("@errand" . ?e)
                        ("@shops" . ?s)
                        ("@onlineshop" . ?o)
                        ("@training" . ?T)
                        (:endgroup)
                        (:newline)
                            ;;; Tools
                        ("@phone" . ?p)
                        ("@computer" . ?c)
                        ;; ("@anywhere" . ?c)
                        (:newline)
                            ;;; Category
                        ("#email" . ?m)
                        ("#lowenergy" . ?l)
                        ("#translation" . ?t)
                        ;; ("web" . ?t)
                        ;; ("reading" . ?t)
                        (:newline)
                        ("WAITING" . ?W)
                        ("HOLD" . ?H)
                        ("CANCELLED" . ?C)
                        )))

(use-package! org-super-agenda
  ;; :commands (org-super-agenda-mode)
  ;; :init (advice-add #'org-super-agenda-mode :around #'doom-shut-up-a)
  :after org-agenda
  :init
  (setq org-super-agenda-groups
        '((:name "Schedule\n"
           :time-grid t)
          (:name "Habits"
           :habit t)
          (:name "Today\n"
           :scheduled today
           :discard (:habit))
          (:name "Translation"
           :tag "#translation")
          (:name "Due today\n"
           :deadline today)
          (:name "Overdue\n"
           :deadline past)
          (:name "Due soon\n"
           :deadline future)
          (:name "Waiting\n"
           :todo "WAIT"
           :order 98)
          (:name "Scheduled earlier\n"
           :scheduled past)))
  :config
  (org-super-agenda-mode))

;; (use-package! org-rich-yank
;;   :after org)

(use-package! org-ql
  :after org)

(after! org-agenda
  (setq org-agenda-custom-commands
        '(("c" "At computer or laptop" tags-todo "@computer"
           ((org-agenda-overriding-header "@Computer Tasks")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'todo '("PROJECT" "TODO" "WAITING" "DONE" "CANCELLED")))))
          ("e" "Emails to send" tags-todo "#email"
           ((org-agenda-overriding-header "Emails")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'todo '("PROJECT" "TODO" "DONE" "CANCELLED")))))
          ("h" "Tasks around the house" tags-todo "@home"
           ((org-agenda-overriding-header "@Home Tasks")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'todo '("PROJECT" "TODO" "WAITING" "DONE" "CANCELLED")))))
          ("t" "Translation (work) related tasks" tags-todo "#translation"
           ((org-agenda-overriding-header "Translation")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'todo '("PROJECT" "TODO" "WAITING" "DONE" "CANCELLED")))))
          ("o" "Online shopping" tags-todo "@onlineshop"
           ((org-agenda-overriding-header "@Online Shops")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'todo '("PROJECT" "TODO" "DONE" "CANCELLED")))))
          ("e" "Errands out and about" tags-todo "@errand"
           ((org-agenda-overriding-header "Errands")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'todo '("PROJECT" "TODO" "WAITING" "DONE" "CANCELLED")))))
          ("l" "Low energy tasks" tags-todo "#lowenergy"
           ((org-agenda-overriding-header "Low Energy")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'todo '("PROJECT" "TODO" "WAITING" "DONE" "CANCELLED")))))
          ;; HACK Attempt to emulate rudimentary Supermemo IR stack in org
          ("i" "Incremental Reading stack"
           ((org-ql-block '(todo "READING")
                          ((org-ql-block-header "Incremental Reading Stack")))))
          ("p" "Projects list"
           ((org-ql-block '(todo "PROJECT")
                          ((org-ql-block-header "Test PROJECT list")))))
          ;; From https://github.com/alphapapa/org-ql/blob/master/examples.org
          ("s" "Stuck Projects"
           ((org-ql-block '(and (todo "PROJECT")
                                (not (done))
                                (not (descendants (todo "NEXT")))
                                (not (descendants (scheduled))))
                          ((org-ql-block-header "Suck Projects")))))
          ;; List tasks without "PROJECT" parent
          ("O" "Orphaned Tasks"
           ((org-ql-block '(and (todo)
                                (path "personal.org"
                                      "work.org"
                                      "wiki/thesis.org")
                                (not (todo "PROJECT"))
                                (not (ancestors (todo "PROJECT")))))
            ((org-ql-block-header "Orphaned Tasks"))))
          )))

(use-package! ox-reveal
  :config
  (setq org-reveal-root "file:///home/volk/Templates/reveal.js"))

(use-package! org-depend
  :after org
  :config
(defun r-org/org-insert-trigger ()
  "Automatically insert chain-find-next trigger when entry becomes NEXT"
  (cond ((equal org-state "NEXT")
         (unless org-depend-doing-chain-find-next
           (org-set-property "TRIGGER" "chain-find-next(NEXT,from-current,priority-up,effort-down)")))
        ((equal org-state "IN-PROGRESS")
         (unless org-depend-doing-chain-find-next
           (org-set-property "TRIGGER" "chain-find-next(NEXT,from-current,priority-up,effort-down)")))
        ((equal org-state "WAITING")
         (unless org-depend-doing-chain-find-next
           (org-set-property "TRIGGER" "chain-find-next(NEXT,from-current,priority-up,effort-down)")))
        ((not (member org-state org-done-keywords))
         (org-delete-property "TRIGGER")))))

(add-hook 'org-after-todo-state-change-hook 'r-org/org-insert-trigger)

;; Capture
;; TODO Transfer captures to DOCT
(after! org-capture
  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  ;; :ensure nil
  ;; :after org
  ;; :preface
  (defvar my/org-ledger-debitcard1-template "%(org-read-date) %^{Payee}
  Expenses:%^{Account}  $%^{Amount}
  Assets:Bank:Personal" "Template for personal debit card transactions with ledger.")

  (defvar my/org-ledger-debitcard2-template "%(org-read-date) %^{Payee}
  Expenses:%^{Account}  $%^{Amount}
  Assets:Bank:Business" "Template for business debit card transactions with ledger.")

  (defvar my/org-ledger-cash-template "%(org-read-date) * %^{Payee}
  Expenses:%^{Account}  $%^{Amount}
  Assets:Cash:Wallet" "Template for cash transaction with ledger.")

  (defvar my/org-ledger-creditcard-template "%(org-read-date) %^{Payee}
  Expenses:%^{Account}  $%^{Amount}
  Liabilities:Credits Cards:CWB" "Template for credit card transaction with ledger.")
  :custom
  (setq org-default-notes-file "inbox.org"
        org-capture-templates
        `(("t" "Todo [inbox]" entry (file+headline "~/org/inbox.org" "Tasks")
           "** TODO %i%?")
          ("e" "email" entry (file+headline "~/org/inbox.org" "Tasks")
           "** TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
          ("e" "email" entry (file+headline "~/org/inbox.org" "Tasks")
           "** TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
          ("c" "Contacts" entry (file "~/org/contacts.org")
           "** %(org-contacts-template-name)\n:PROPERTIES:\n:ROLE: %^{Role}\n:COMPANY: %^{Company}\n:EMAIL: %^(org-contacts-template-email)\n:CELL:%^{Cellphone}\n:PHONE:%^{Phone}\n:WEBSITE: %^{Website}\n:ADDRESS: %^{289 Cleveland St. Brooklyn, 11206 NY, USA}\n:NOTE: %^{NOTE} \n:END:")
          ;; Ledger
          ("l" "Ledger")
          ("lp" "Personal Bank" plain (file ,(format "~/Documents/ledger/ledger-%s.dat" (format-time-string "%Y"))),
           my/org-ledger-debitcard1-template
           :empty-lines 1
           :immediate-finish t)
          ("lb" "Business Bank" plain (file ,(format "~/Documents/ledger/ledger-%s.dat" (format-time-string "%Y"))),
           my/org-ledger-debitcard2-template
           :empty-lines 1
           :immediate-finish t)
          ("lc" "CWB Credit Card" plain (file ,(format "~/Documents/ledger/ledger-%s.dat" (format-time-string "%Y"))),
           my/org-ledger-creditcard-template
           :empty-lines 1
           :immediate-finish t)
          ("lw" "Cash" plain (file ,(format "~/Documents/ledger/ledger-%s.dat" (format-time-string "%Y"))),
           my/org-ledger-cash-template
           :empty-lines 1
           :immediate-finish t)

          ;; Anki
          ("a" "Anki")
          ("ab" "Anki basic"
           entry
           (file+headline org-my-anki-file "Dispatch Shelf")
           "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: MegaDeck\n:END:\n** Front\n%?\n** Back\n%x\n")
          ("ac" "Anki cloze"
           entry
           (file+headline org-my-anki-file "Dispatch Shelf")
           "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: MegaDeck\n:END:\n** Text\n%x\n** Extra\n")
          ("ai" "Anki IR"
           entry
           (file+function "~/org/anki.org" key-to-header)
           "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: IR_Cloze\n:ANKI_DECK: MegaDeck\n:END:\n** Text\n%x\n** Extra \n\n** Cited \n** Link\n %(insert-property)  \n** Key\n %(insert-key)")
          ;; ("at" "Anki test"
          ;;  entry
          ;;  (file+function "~/org/anki.org" insert-key)
          ;;  "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: IR_Cloze\n:ANKI_DECK: MegaDeck\n:END:\n** Text\n%x ** Extra \n\n** Link\n %(insert-property)%")
          )))

;; "setup org-babel."
(after! ob
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t)
  ;; org-src-tab-acts-natively t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((c          . t)
     (calc       . t)
     ;; (comint     . t)
     ;; (ditaa      . t)
     (dot        . t)
     (emacs-lisp . t)
     (gnuplot    . t)
     ;; (haskell    . t)
     ;; (js         . t)
     (latex      . t)
     (lisp       . t)
     (makefile   . t)
     (python     . t)
     ;; (r          . t)
     ;; (restclient . t)
     ;; (sagemath   . t)
     (scheme     . t)
     (shell      . t)
     (sql        . t)
     (sqlite     . t))))

;; (setq org-mode-hook #'display-line-numbers-mode)
(use-package! async
  :after org-brain
  :config
  (setq org-system-file (expand-file-name (concat org-brain-path "/system.org")))

  (defun tangle-init-async ()
    "If the current buffer is 'system.org' the code-blocks are tangled."
    (when (equal (buffer-file-name) org-system-file)
      (async-start
       `(lambda ()
          (require 'org)
          (org-babel-tangle-file ,org-system-file))
       (lambda (result)
         (message "Tangled file compiled.")))))
  (add-hook 'after-save-hook 'tangle-init-async))

(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode  ; elisp's mechanisms are good enough
            sql-mode         ; sqlformat is currently broken
            tex-mode         ; latexindent is broken
            org-mode
            latex-mode))


;; PDF + synctex
(after! pdf-tools
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t)

  ;; This allows for opening in an indirect buffer
  (setq pdf-sync-backward-display-action t)
  (setq pdf-sync-forward-display-action t)

  ;; to have the buffer refresh after compilation
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

;; org-ref
(use-package! org-ref
  :config
  (setq reftex-default-bibliography "~/Documents/LaTeX/uni.bib"
        org-ref-default-bibliography '("~/Documents/LaTeX/uni.bib")
        org-ref-pdf-directory "~/Documents/PDF/"
        org-ref-bibliography-notes "~/org/org-brain/article-notes"
        bibtex-completion-bibliography "~/Documents/LaTeX/uni.bib"
        bibtex-completion-library-path "~/Documents/PDF"
        bibtex-completion-notes-path "~/org/org-brain/article-notes"
        bibtex-completion-pdf-symbol "⌘"
        bibtex-completion-notes-symbol "✎"
        bibtex-completion-additional-search-fields '(keywords)
        ))
;; bib-library "~/Documents/LaTeX/uni.bib"

;; (use-package! org-noter
;;   ;; :after pdf-tools
;;   :commands (org-noter)
;;   :config
;;   (require 'org-noter-pdftools)
;;   (after! pdf-tools
;;   (setq org-noter-always-create-frame nil
;;         ;; org-noter-insert-note-no-questions t
;;         org-noter-separate-notes-from-heading t
;;         org-noter-auto-save-last-location t
;;         org-noter-notes-search-path '("~/org/article-notes"))))

;; (use-package! org-pdftools
;;   :init (setq org-pdftools-root-dir "~/Documents/PDF/"
;;               org-pdftools-search-string-separator "??")
;;   :config
;;   (after! :org
;; (org-link-set-parameters "pdftools"
;;                          :follow #'org-pdftools-open
;;                          :complete #'org-pdftools-complete-link
;;                          :store #'org-pdftools-store-link
;;                          :export #'org-pdftools-export)
;; (add-hook 'org-store-link-functions 'org-pdftools-store-link)))

;; ;; Using a manual extraction for incremental reading
;; ;; https://github.com/weirdNox/org-noter/issues/88#issuecomment-570904906
;; (defun org-noter-insert-selected-text-inside-note-content ()
;;   (interactive)
;;   (progn (setq currenb (buffer-name))
;;          (org-noter-insert-precise-note)
;;          (set-buffer currenb)
;;          (org-noter-insert-note)))

;; (defun org-noter-init-pdf-view ()
;;   (pdf-view-fit-page-to-window)
;;   ;; (pdf-view-auto-slice-minor-mode)
;;   (run-at-time "0.5 sec" nil #'org-noter))

;; (add-hook 'pdf-view-mode-hook 'org-noter-init-pdf-view))

;; org-brain
(use-package! org-brain
  :defer-incrementally t
  :init
  (setq org-brain-visualize-default-choices 'all
        org-brain-title-max-length 24
        org-brain-include-file-entries nil
        org-brain-backlink t)
  (setq org-brain-path "~/org/org-brain")

  :config
  (set-evil-initial-state! 'org-brain-visualize-mode 'emacs)
  (set-popup-rule! "^\\*org-brain" :side 'right :size 1.00 :select t :ttl nil)
  (setq org-id-track-globally t
        org-id-locations-file "~/org/.org-id-locations")

  (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
  (cl-pushnew '("b" "Brain" plain (function org-brain-goto-end)
                "* %i%?" :empty-lines 1)
              org-capture-templates
              :key #'car :test #'equal))
;; (define-key org-brain-visualize-mode-map (kbd "C-l") #'link-hint-open-link)
(map! :map org-brain-visualize-mode-map "C-o" #'link-hint-open-link)

(after! org-brain
  (use-package! org-cliplink)
  (defun org-brain-cliplink-resource ()
    "Add a URL from the clipboard as an org-brain resource.
    Suggest the URL title as a description for resource."
    (interactive)
    (let ((url (org-cliplink-clipboard-content)))
      (org-brain-add-resource
       url
       (org-cliplink-retrieve-title-synchronously url)
       t))))

(map! :map org-brain-visualize-mode-map "L" #'org-brain-cliplink-resource)
(add-hook 'org-brain-after-visualize-hook #'visual-line-mode)
(after! org-brain
  :init
  ;; first we add keymap features to the existing helm source
(defmacro helm-exit-and-run! (&rest body)
  "Define an action with BODY to be run after exiting Helm."
  (declare (doc-string 1))
  `(lambda ()
     (interactive)
     (with-helm-alive-p
       (helm-exit-and-execute-action (lambda (_candidate) ,@body)))))

  (defun helm-org-brain--visualize-node (_c)
    (dolist (candidate (helm-marked-candidates))
      (org-brain-visualize (or (org-brain-entry-from-id candidate) candidate))))

  (defun helm-org-brain--switch-node (_c)
    (dolist (candidate (helm-marked-candidates))
      (org-brain-goto (or (org-brain-entry-from-id candidate) candidate))))

  (defun helm-org-brain-switch-node-other-window ()
    "Open the current node selected in helm-brain in org"
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'helm-org-brain--switch-node)))

(defvar helm-org-brain-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    ;; (define-key map (kbd "C-c o") (helm-exit-and-run! (helm-org-brain--switch-node)))
    (define-key map (kbd "C-c o") 'helm-org-brain-switch-node-other-window)
    map)
"Keymap for `helm-brain'.")

(defun helm-org-brain-build-source (&optional filter)
  "Build source for org-brain buffers.
See `helm-org-brain' for more details."
  (helm-build-sync-source "org-brain"
    :candidates #'org-brain--all-targets
    ;; :candidate-transformer 'helm-exwm-highlight-buffers
    :action '(("Visualize node" . helm-org-brain--visualize-node)
              ("Switch to node(s) in other window `C-c o`'" . helm-brain--switch-node)
              ("Kill buffer(s) `M-D`" . helm-kill-marked-buffers))
    ;; When follow-mode is on, the persistent-action allows for multiple candidate selection.
    :persistent-action 'helm-buffers-list-persistent-action
    :keymap helm-org-brain-map))


(defun helm-org-brain (&optional filter)
    (interactive)
    (helm :sources (helm-org-brain-build-source filter)
          :buffer "helm-org-brain"))


  :config
  (defun helm-brain--switch-node (_c)
    (dolist (candidate (helm-marked-candidates))
      (org-brain-goto (or (org-brain-entry-from-id candidate) candidate))))

  (defun helm-brain-switch-node-other-window ()
    "Open the current node selected in helm-brain in org"
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'helm-brain--switch-node)))

 ;; (add-to-list 'helm-brain--actions
 ;;              (helm-make-actions
 ;;               '("Switch to org buffer" 'helm-brain--switch-node)))

  (map! :map helm-org-brain-map
        "C-c o" #'helm-org-brain-switch-node-other-window)
        ;; "RET" #'helm-brain--select)
        ;; "C-c m" #'(helm-exit-and-run! (helm-brain--switch-node)))
        ;; "C-c n" #'helm-brain--actions)

(map! :map org-mode-map
      "C-c a"         #'org-agenda
       (:prefix ("C-c b" . "brain")
        "a"            #'org-brain-agenda))
)
;; (defun helm-org-rifle-brain ()
;;   ;; "Rifle files in `org-brain-path'.\"
;;   (interactive)
;;   (helm-org-rifle-directories (list org-brain-path)))
;; Allows me to have differing brains on key press
;; May switch to single monolithic brain at some point
;; (after! org-brain
;;   (defun exwm/multi-org-brain-helm ()
;;     (interactive
;;      (cond
;;       ((equal current-prefix-arg nil)
;;        (if (equal org-brain-path "~/org/org-brain")
;;            (helm-brain)
;;          (org-brain-switch-brain "~/org/org-brain")
;;          (helm-brain)))
;;       ((equal current-prefix-arg 1)
;;        (if (equal org-brain-path "~/org/article-notes")
;;            (helm-brain)
;;          (org-brain-switch-brain "~/org/article-notes")
;;          (helm-brain)))
;;       ((equal current-prefix-arg 2)
;;        ;; TODO Autoswitch to Japanese input for this helm
;;        ;; (uim-im-switch 'mozc)
;;        (if (equal org-brain-path "~/org/kakure-nou")
;;            (helm-brain)
;;          (org-brain-switch-brain "~/org/kakure-nou")
;;          (helm-brain)))))))

;; Prettier line drawing

(defface aa2u-face '((t . nil))
  "Face for aa2u box drawing characters")
(advice-add #'aa2u-1c :filter-return
            (lambda (str) (propertize str 'face 'aa2u-face)))
(defun aa2u-org-brain-buffer ()
  (let ((inhibit-read-only t))
    (make-local-variable 'face-remapping-alist)
    (add-to-list 'face-remapping-alist
                 '(aa2u-face . org-brain-wires))
    (ignore-errors (aa2u (point-min) (point-max)))))
(with-eval-after-load 'org-brain
  (add-hook 'org-brain-after-visualize-hook #'aa2u-org-brain-buffer))

(defun org-brain-insert-resource-icon (link)
  "Insert an icon, based on content of org-mode LINK."
  (insert (format "%s "
                  (cond ((string-prefix-p "brain:" link)
                         (all-the-icons-fileicon "brain"))
                        ((string-prefix-p "info:" link)
                         (all-the-icons-octicon "info"))
                        ((string-prefix-p "help:" link)
                         (all-the-icons-material "help"))
                        ((string-prefix-p "http" link)
                         (all-the-icons-icon-for-url link))
                        (t
                         (all-the-icons-icon-for-file link))))))

(with-eval-after-load 'all-the-icons
  (add-hook 'org-brain-after-resource-button-functions
            'org-brain-insert-resource-icon))

;; Setup org-expiry and define a org-agenda function to compare timestamps
(use-package! org-expiry
  :config
  (setq org-expiry-inactive-timestamps t)
  (defun org-expiry-created-comp (a b)
    "Compare `org-expiry-created-property-name' properties of A and B."
    (let ((ta (ignore-errors
                (org-time-string-to-seconds
                 (org-entry-get (get-text-property 0 'org-marker a)
                                org-expiry-created-property-name))))
          (tb (ignore-errors
                (org-time-string-to-seconds
                 (org-entry-get (get-text-property 0 'org-marker b)
                                org-expiry-created-property-name)))))
      (cond ((if ta (and tb (< ta tb)) tb) -1)
            ((if tb (and ta (< tb ta)) ta) +1))))

  ;; Add CREATED property when adding a new org-brain headline entry
  (add-hook 'org-brain-new-entry-hook #'org-expiry-insert-created)

  ;; Finally add a function which lets us watch the entries chronologically
  (defun org-brain-timeline ()
    "List all org-brain headlines in chronological order."
    (interactive)
    (let ((org-agenda-files (org-brain-files))
          (org-agenda-cmp-user-defined #'org-expiry-created-comp)
          (org-agenda-sorting-strategy '(user-defined-down)))
      (org-tags-view nil (format "+%s>\"\"" org-expiry-created-property-name))))

  )

;; org-noter
;; org-noter + org-brain
;; https://github.com/Kungsgeten/org-brain#org-noter
(add-hook 'org-noter-insert-heading-hook #'org-id-get-create)
(defun org-brain-open-org-noter (entry)
  "Open `org-noter' on the ENTRY. If run interactively, get ENTRY from context."
  (interactive (list (org-brain-entry-at-pt)))
  (org-with-point-at (org-brain-entry-marker entry)
    (org-noter)))

;;    (define-key org-brain-visualize-mode-map (kbd "\C-c n") 'org-brain-open-org-noter)
(map! :map org-brain-visualize-mode-map "\C-c n" #'org-brain-open-org-noter)

;; org-pomodoro
(use-package! org-pomodoro
  :after org
  :config
  (setq org-pomodoro-keep-killed-pomodoro-time t))

;; (use-package! org-drill
;;   :after org
;;   :config
;;   (setq org-drill-scope (quote directory)))

;; (org-drill "~/org/article-notes/article-index.org")

(use-package! org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package! org-noter-pdftools
  :after org-noter
  :config
  ;; (setq org-pdftools-path-generator 'abbreviate-file-name)
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package! org-noter
  :commands (org-noter)
  :config
  (add-hook! org-noter-notes-mode (require 'org-noter-pdftools)))

;; (load! "+ir.el")

(use-package! org-web-tools)
(use-package! org-web-tools-archive)


;; (use-package! org-gcal
;;   :after org
;;   :init
;;   ;; Currently not working https://github.com/kidd/org-gcal.el/issues/58
;;   ;; https://console.cloud.google.com/apis/credentials/
;;   (setq org-gcal-client-id (password-store-get "secrets/org-gcal-client-id")
;;         org-gcal-client-secret (password-store-get "secrets/org-gcal-client-secret"))

;;   (setq org-gcal-file-alist '(("otoo.danieru@gmail.com" . "~/org/danieru-cal.org"))))
