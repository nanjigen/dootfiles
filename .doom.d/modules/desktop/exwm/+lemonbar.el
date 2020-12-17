; get workspace list in bar
(defun feb/exwm-workspace-list ()
  "Return a lemonbar string showing workspace list."
  (let* ((num (exwm-workspace--count))
	 (sequence (number-sequence 0 (1- num)))
	 (curr (exwm-workspace--position exwm-workspace--current)))
    (mapconcat (lambda (i)
		 (format (if (= i curr) "[%%{F#00ff00}%d%%{F-}] " "%d ") i))
	       sequence "")
    ))

(defun feb/exwm-report-workspaces-to-lemonbar ()
  (with-temp-file "/tmp/panel-fifo"
    (insert (format "WIN%s\n" (feb/exwm-workspace-list)))))

(add-hook 'exwm-workspace-switch-hook #'feb/exwm-report-workspaces-to-lemonbar)
(add-hook 'exwm-init-hook #'feb/exwm-report-workspaces-to-lemonbar)
