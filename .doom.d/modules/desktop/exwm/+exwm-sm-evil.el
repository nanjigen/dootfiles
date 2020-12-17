(require 'evil)
(require 'evil-core)
(require '+exwm-sm-core)

;; (defvar exwm-sm-evil-sm-class-name '("Excel" "excel.exe" "WineDesktop - Wine desktop" "explorer.exe" "Wine" "Wine desktop")
;;   "The class name use for detecting if a SM buffer is selected.")

(defvar exwm-sm-evil-sm-buffer-name '((rx bol "home" eol))
  "The buffer name used for detecting if a SM buffer is selected.")

;;; State transitions
(defun exwm-sm-evil-normal ()
  "Pass every key directly to Emacs."
  (interactive)
  (setq-local exwm-input-line-mode-passthrough t)
  (evil-normal-state))

(defun exwm-sm-evil-insert ()
  "Pass every key to SM."
  (interactive)
  (setq-local exwm-input-line-mode-passthrough nil)
  (evil-insert-state))

(defun exwm-sm-evil-exit-visual ()
  "Exit visual state properly."
  (interactive)
  ;; Unmark any selection
  (exwm-sm-core-left)
  (exwm-sm-core-right)
  (exwm-sm-evil-normal))

(defun exwm-sm-evil-visual-change ()
  "Change text in visual mode."
  (interactive)
  (exwm-sm-core-cut)
  (exwm-sm-evil-insert))

;;; Keys
(defvar exwm-sm-evil-mode-map (make-sparse-keymap))

;; Bind normal
(define-key exwm-sm-evil-mode-map [remap evil-exit-visual-state] 'exwm-sm-evil-exit-visual)
(define-key exwm-sm-evil-mode-map [remap evil-normal-state] 'exwm-sm-evil-normal)
(define-key exwm-sm-evil-mode-map [remap evil-force-normal-state] 'exwm-sm-evil-normal)
;; Bind insert
(define-key exwm-sm-evil-mode-map [remap evil-insert-state] 'exwm-sm-evil-insert)
(define-key exwm-sm-evil-mode-map [remap evil-insert] 'exwm-sm-evil-insert)
(define-key exwm-sm-evil-mode-map [remap evil-substitute] 'exwm-sm-evil-insert)
(define-key exwm-sm-evil-mode-map [remap evil-append] 'exwm-sm-evil-insert)

        ;;;; Normal
;; Basic movements
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "k") 'exwm-sm-core-up)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "j") 'exwm-sm-core-down)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "h") 'exwm-sm-core-left)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "l") 'exwm-sm-core-right)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "/") 'exwm-sm-core-find)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "t") 'exwm-sm-core-test)

;;; Editing text
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "y") 'exwm-sm-core-copy)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "d") 'exwm-sm-core-cut)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "p") 'exwm-sm-core-paste)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "b") 'exwm-sm-core-test)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "u") 'exwm-sm-core-undo)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "R") 'exwm-sm-core-redo)
;; Editing elements
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "e") 'exwm-sm-core-answer)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "E") 'exwm-sm-core-question)

;;; Learn operations
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "x") 'exwm-sm-core-extract)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "<space>") 'exwm-sm-core-learn)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "P") 'exwm-sm-core-prioritize)
(evil-define-key 'normal exwm-sm-evil-mode-map (kbd "o") 'exwm-sm-core-copy-path)

;;; Mode
;;;###autoload
(define-minor-mode exwm-sm-evil-mode nil nil nil exwm-sm-evil-mode-map
  (if exwm-sm-evil-mode
      (progn
        (exwm-sm-evil-normal))))

;;;###autoload
(defun exwm-sm-evil-activate-if-sm ()
  "Activates exwm-sm mode when buffer is SM.
SM variant can be assigned in 'exwm-sm-evil-sm-name`"
  (interactive)
  (if (member exwm-class-name exwm-sm-evil-sm-buffer-name)
      (exwm-sm-evil-mode 1)))

(provide '+exwm-sm-evil)

;;; +exwm-sm-evil.el ends here
