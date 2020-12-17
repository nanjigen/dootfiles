     (defun a/_exwm-statusbar-battery ()
        "Get laptop battery current capacity."
        (let ((bat
               (concat
                (string-trim
                 (shell-command-to-string
                  "cat /sys/class/power_supply/BAT0/capacity"))
                "%")))
          bat))

      (defun a/_exwm-statusbar-cpu-temperature ()
        "Get CPU current temperature."
        (let ((cpu-temp
               (concat (substring
                        (string-trim
                         (shell-command-to-string
                          "cat /sys/class/thermal/thermal_zone1/temp"))
                        0 -3)
                       "°C")))
          cpu-temp))


      (defun a/_exwm-statusbar-cpu-frequency ()
        "Get CPU current frequency."
        (let ((cpu-freq
               (concat (number-to-string
                        (let ((a (split-string (shell-command-to-string
                                                "grep 'cpu ' /proc/stat"))))
                          (/ (* (+ (string-to-number (nth 1 a))
                                   (string-to-number (nth 3 a)))
                                100)
                             (+ (string-to-number (nth 1 a))
                                (string-to-number (nth 3 a))
                                (string-to-number (nth 4 a))))))
                       "%")))
          cpu-freq))

      (defun a/_exwm-statusbar-date-time ()
        "Get System current time and date."
        (let ((time
               (format-time-string "%A %D - %l:%M %p" (current-time))))
          time))

      (defun a/_exwm-statusbar-volume ()
        "Get System current audio volume.
        Supports Pulseaudio."
        (let ((volume (let ((current-volume (shell-command-to-string "amixer get Master")))
                        (string-match "\\([0-9]+%\\)" current-volume)
                        (match-string 0 current-volume))))
          volume))

      (defun a/exwm-statusbar ()
        "Use echo-area to display system stats"

        ;; TODO: Proper all-the-icons usage
        ;; TODO: Emacs package?
        ;; TODO: Unix Portable

        (require 'all-the-icons)
        (require 'subr-x)
        (message " %s /  %s /  %s  /   %s   /  %s  /  %s"
                 (a/exwm-get-workspace-number)
                 (a/_exwm-statusbar-battery)
                 (a/_exwm-statusbar-cpu-frequency)
                 (a/_exwm-statusbar-cpu-temperature)
                 (a/_exwm-statusbar-volume)
                 (a/_exwm-statusbar-date-time)))
