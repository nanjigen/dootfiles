(require 'exwm)
(require 'exwm-input)

;;; Basic navigation
;;;###autoload
(defun exwm-sm-core-up ()
  "Move up."
  (interactive)
  (exwm-input--fake-key 'up))

;;;###autoload
(defun exwm-sm-core-down ()
  "Move down."
  (interactive)
  (exwm-input--fake-key 'down))

;;;###autoload
(defun exwm-sm-core-left ()
  "Move down."
  (interactive)
  (exwm-input--fake-key 'left))

;;;###autoload
(defun exwm-sm-core-right ()
  "Move down."
  (interactive)
  (exwm-input--fake-key 'right))

(defun my-translate-winpath ()
  "change :C\win\path to 'nix /path/path"
  (let ((path (current-kill 0 'do-not-move)))
    (shell-command-to-string (format "~/.scripts/winpath.sh %s" path))))

(defun my-translate-winpath ()
  "change :C\win\path to 'nix /path/path"
  (let ((path (current-kill 0 'do-not-move)))
    (my-translate-path)))

(defun my-translate-winpath ()
  (interactive)
  (let* ((path (s-chop-prefix "z:" (current-kill 0 'do-not-move)))
         (nixpath (s-replace "\\" "/" path))
         (path1 (s-replace "documents" "Documents" nixpath)))
    (s-replace "sm" "SM" path1)))

(defun exwm-sm-core-test ()
  "Send string to a given exwm buffer"
  (interactive)
(progn)
(my-translate-winpath)
;; (sit-for 3)
(message sm-path-nix))

(defun exwm-sm-core-test ()
  "Send string to a given exwm buffer"
  (interactive)
  (shell-command "sh ~/.scripts/xdotool/get_element_path.sh"))

(defun exwm-sm-core-test ()
  "Send string to a given exwm buffer"
  (interactive)
(progn
  (shell-command "sh ~/.scripts/xdotool/get_element_path.sh")
  (gui--selection-value-internal 'CLIPBOARD)))

(defun exwm-sm-core-test ()
  "Send string to a given exwm buffer"
  (interactive)
  (shell-command "sh ~/.scripts/xdotool/get_element_path.sh")
(lambda ()
)
  (setq sm-path (gui--selection-value-internal 'CLIPBOARD)))
  ;; (shell-command "sh ~/.scripts/xdotool/get_element_path.sh"))

(defun exwm-sm-core-test()
  "test"
  (interactive)
  (exwm--log)
  (let ((exwm-input-line-mode-passthrough t))
         ;; (key "?/C-v"))
         ;; (key (read-key "22")))
    (exwm-input--fake-key 'S-L)))

(exwm-input-set-key (kbd "s-t") #'exwm-sm-core-test)

(defun my-exwm-send-string (string)
  "Send STRING to `exwm-mode' buffer or just insert it."
  (if (eq major-mode 'exwm-mode)
      (mapc #'exwm-input--fake-key (string-to-list string))
    (insert string)))

(defun exwm-sm-core-string-test ()
  (interactive)
  ;; (let ((exwm-input-line-mode-passthrough t))
    ;; (my-exwm-send-string [#o26])))
  (my-exwm-send-string [94 3]))

;; (term-send-raw-string "l")

;; (read-key-sequence-vector)

(exwm-input-set-key (kbd "<f9>") #'exwm-sm-core-string-test)
;; ###autoload
;; (defun exwm-sm-core-test ()
;;   "Send string to a given exwm buffer"
;;   (interactive)
;;   (my-exwm-send-string "sm-get-path ")
;;   (find-file (my-translate-winpath)))

(cl-defun exwm-input-send-sim-key (key)
  "Fake a key event according to the last input key sequence."
  (interactive)
  (exwm--log)
  (unless (derived-mode-p 'exwm-mode)
    (cl-return-from exwm-input-send-simulation-key))
  (let ((keys (gethash (this-single-command-keys)
                       exwm-input--simulation-keys)))
    (dolist (key keys)
      (exwm-input--fake-key key))))

(cl-defun my-exwm-input-send-next-key (keys)
  "Send next key to client window.

EXWM will prompt for the key to send.  This command can be prefixed to send
multiple keys.  If END-KEY is non-nil, stop sending keys if it's pressed."
  (interactive "p")
  (exwm--log)
  (unless (derived-mode-p 'exwm-mode)
    (cl-return-from my-exwm-input-send-next-key))
  (let (key keys)
      ;; Skip events not from keyboard
      (let ((exwm-input-line-mode-passthrough t))
            ;; (setq key (read-key (format (key-description keys))
        (setq key (key-description "?\C-v")))
      (setq keys (vconcat keys (vector key)))
      (exwm-input--fake-key key)))

(defun exwm-sm-core-test()
  "test"
  (interactive)
  (exwm--log)
  (exwm-input-release-keyboard)
  (exwm-input--fake-key ?\C-v))

;;; Find
;;;###autoload
(defun exwm-sm-core-find ()
  "Find."
  (interactive)
  (exwm-input--fake-key ?\C-f))

;;;###autoload
(defun exwm-sm-core-quick-find ()
  "Quick find."
  (interactive)
  (exwm-input--fake-key ?/))

;;;###autoload
(defun exwm-sm-core-find-next ()
  "Find next."
  (interactive)
  (exwm-input--fake-key ?\C-g))

;;;###autoload
(defun exwm-sm-core-find-previous ()
  "Find previous."
  (interactive)
  (exwm-input--fake-key ?\C-\S-g))


;;;###autoload
(defun exwm-sm-core-test()
  "Find previous."
  (interactive)
  (exwm-input--fake-key ?\-g))
;;; Editing
;; ;;;###autoload
;; (defun exwm-sm-core-test ()
;;   "Copy text."
;;   (interactive)
;;   (exwm-input--invoke--m))
;; (exwm-input-invoke-factory "m")

;; (defun exwm-sm-core-test ()
;;   "Copy text."
;;   (interactive)
;;   (with-current-buffer (window-buffer)
;;     (fhd/exwm-input-char-mode)
;;     (exwm-input--invoke--m)))

;; (exwm-input--fake-key ?\C-v)))

;; (defun invoke-ctl-v-map ()
;;   (interactive)
;;   (exwm-input--cache-event ?\C-v t)
;;   (exwm-input--unread-event ?\C-v))

;; (defun exwm-sm-core-paste ()
;;   "Paste text."
;;   (interactive)
;;   (exwm-input-send-next-key (exwm-input--fake-key ?\C-p)))

;; (defun exwm-sm-core-paste ()
;;   "Paste text."
;;   (interactive)
;;   (exwm-input-send-next-key 2 ?\C-p))

;;;###autoload
(defun exwm-sm-core-copy ()
  "Copy text."
  (interactive)
  (exwm-input--fake-key ?\C-c))

;;;###autoload
(defun exwm-sm-core-cut ()
  "Cut text."
  (interactive)
  (exwm-input--fake-key ?\C-x))

;;;###autoload
(defun exwm-sm-core-paste ()
  "Paste text."
  (interactive)
  (exwm-input--fake-key ?\C-v))

;;;###autoload
(defun exwm-sm-core-delete ()
  "Delete text."
  (interactive)
  (exwm-input--fake-key 'delete))

;;;###autoload
(defun exwm-sm-core-undo ()
  "Undo."
  (interactive)
  (exwm-input--fake-key ?\C-z))

;;;###autoload
(defun exwm-sm-core-redo ()
  "Redo."
  (interactive)
  (exwm-input--fake-key ?\C-\S-z))

;;;###autoload
(defun exwm-sm-core-redo-last ()
  "redo last undo (in html text editing)."
  (interactive)
  (exwm-input--fake-key ?\C-y))

;;;###autoload
(defun exwm-sm-core-forward-word ()
  "Move word forward."
  (interactive)
  (exwm-input--fake-key 'C-right))

;;;###autoload
(defun exwm-sm-core-back-word ()
  "Move word backward."
  (interactive)
  (exwm-input--fake-key 'C-left))

;;;###autoload
(defun exwm-sm-core-edit-answer ()
  "Edit the first answer"
  (interactive)
  (exwm-input--fake-key ?\a))

;;;###autoload
(defun exwm-sm-core-edit-texts ()
  "Edit texts"
  (interactive)
  (exwm-input--fake-key ?\e))

;;;###autoload
(defun exwm-sm-core-edit-question ()
  "Edit the first question"
  (interactive)
  (exwm-input--fake-key ?\q))

;;;###autoload
(defun exwm-sm-core-delete-element ()
  "Delete currently displayed element"
  (interactive)
  (exwm-input--fake-key 'delete))

;; ;;;###autoload
;; (defun exwm-sm-core-xxx ()
;;   "xxx"
;;   (interactive)
;;   (exwm-input--fake-key 'xxx))

;; ;;;###autoload
;; (defun exwm-sm-core-xxx ()
;;   "xxx"
;;   (interactive)
;;   (exwm-input--fake-key 'xxx))

;;;; Selection
;;;###autoload
(defun exwm-sm-core-forward-word-select ()
  "Move word forward and select."
  (interactive)
  (exwm-input--fake-key 'C-S-right))

;;;###autoload
(defun exwm-sm-core-back-word-select ()
  "Move word backward and select."
  (interactive)
  (exwm-input--fake-key 'C-S-left))

;;;###autoload
(defun exwm-sm-core-select-all ()
  "Select whole page."
  (interactive)
  (exwm-input--fake-key ?\C-a))

;;;; Learn operations

;;;###autoload
(defun exwm-sm-core-learn ()
  "Start learning"
  (interactive)
  (exwm-input--fake-key ?\C-l))

;;;###autoload
(defun exwm-sm-core-prioritize ()
  "Modify priority of current element"
  (interactive)
  (exwm-input--fake-key 'M-p))

;;;###autoload
(defun exwm-sm-core-reschedule ()
  "Learning: Reschedule to another day"
  (interactive)
  (exwm-input--fake-key ?\C-j))

;;;###autoload
(defun exwm-sm-core-postpone ()
  "Schedule review later today"
  (interactive)
  (exwm-input--fake-key ?\C-\S-j))

;; ;;;###autoload
;; (defun exwm-sm-core-xxx ()
;;   "xxx"
;;   (interactive)
;;   (exwm-input--fake-key 'xxx))

;;;###autoload
(defun exwm-sm-core-extract ()
  "Extract selected text"
  (interactive)
  (exwm-input--fake-key 'M-x))

;;;###autoload
(defun exwm-sm-core-neural ()
  "Go neural"
  (interactive)
  (exwm-input--fake-key 'C-f2))


;;; Misc
;;;###autoload
(defun exwm-sm-core-cancel ()
  "General cancel action."
  (interactive)
  ;; Needs to get user out of search bar, this is the only way i've found to do it
  (exwm-input--fake-key 'escape)
  (exwm-input--fake-key 'tab))

;;;###autoload
(defun exwm-sm-core-open-file ()
  "Open file."
  (interactive)
  (exwm-input--fake-key ?\C-o))

;;;###autoload
(defun exwm-sm-core-quit ()
  "Quit sm."
  (interactive)
  (exwm-input--fake-key ?\C-q))

(provide '+exwm-sm-core)
