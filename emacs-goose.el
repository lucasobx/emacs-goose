;;; emacs-goose.el --- Goose companion for Emacs  -*- lexical-binding: t; -*-

;; Author: Lucas
;; URL: https://github.com/lucasobx/emacs-goose
;; Package-Requires: ((emacs "29.3"))

;;; Commentary:

;; Displays an animated goose in a child frame.
;; Based on https://github.com/harrybournis/emacs-pets (GPL v3).
;; Enable with `M-x emacs-goose-mode'.

;;; Code:

(require 'cl-lib)
(require 'image)

;;;; customization

(defgroup emacs-goose nil
  "Goose companion for Emacs."
  :group 'games
  :prefix "emacs-goose-")

(defcustom emacs-goose-tick-interval 0.3
  "Seconds between animation frames."
  :type 'float)

(defcustom emacs-goose-max-position 20
  "Maximum number of steps in the horizontal walk cycle."
  :type 'integer)

(defcustom emacs-goose-idle-chance 0.05
  "Probability of entering idle state each tick."
  :type 'float)

(defcustom emacs-goose-idle-duration 3
  "Number of ticks to remain idle."
  :type 'integer)

(defcustom emacs-goose-sleep-chance 0.005
  "Probability of entering sleep state each tick."
  :type 'float)

(defcustom emacs-goose-sleep-duration 60
  "Number of seconds to remain sleeping."
  :type 'integer)

(defcustom emacs-goose-floor-offset 0
  "Pixels to nudge the goose up from the mode-line."
  :type 'integer)

;;;; constants

(defconst emacs-goose--sprite-dir
  (expand-file-name
   "sprites"
   ;; Locate the sprites next to this file, falling back to `default-directory'.
   (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "Directory containing the sprite .txt files.")

(defconst emacs-goose--walk-files
  '("goose-walk-1.txt" "goose-walk-2.txt" "goose-walk-3.txt" "goose-walk-4.txt")
  "Walk cycle animation files, relative to `emacs-goose--sprite-dir'.")

(defconst emacs-goose--idle-files
  '("goose-idle-1.txt" "goose-idle-2.txt")
  "Idle animation files, relative to `emacs-goose--sprite-dir'.")

(defconst emacs-goose--sleep-files
  ;; Repeated frames slow the sleep animation down without extra timing logic.
  '("goose-sleep-1.txt" "goose-sleep-1.txt" "goose-sleep-1.txt"
    "goose-sleep-2.txt" "goose-sleep-2.txt" "goose-sleep-2.txt"
    "goose-sleep-3.txt" "goose-sleep-3.txt" "goose-sleep-3.txt"
    "goose-sleep-4.txt" "goose-sleep-4.txt" "goose-sleep-4.txt")
  "Sleep animation files, relative to `emacs-goose--sprite-dir'.")

(defconst emacs-goose--sprite-colors
  '("  c None"
    "= c #ecb187"
    ". c #ffffff"
    "* c #aa6738"
    "@ c #000000"
    "Z c #508cdc")
  "XPM color map for the goose sprite.")

;; Horizontal placement only; the vertical floor is measured at runtime
;; (see `emacs-goose--floor-pixel').
(defconst emacs-goose--height-factor 1.2
  "Target sprite height as a multiple of the parent frame's character height.")

(defconst emacs-goose--h-start-fraction 0.7
  "Fraction of the parent frame width where the walk region begins (left edge).")

;;;; internal state

(defvar emacs-goose--position 0
  "Current horizontal position (step index).")

(defvar emacs-goose--direction 'right
  "Current movement direction (`left' or `right').")

(defvar emacs-goose--state 'walking
  "Current goose state (`walking', `idle', or `sleeping').")

(defvar emacs-goose--idle-counter 0
  "Remaining ticks of idle state.")

(defvar emacs-goose--sleep-counter 0
  "Remaining seconds of sleep state.")

(defvar emacs-goose--timer nil
  "Timer driving the animation loop.")

(defvar emacs-goose--walk-frames nil
  "Vector of (LEFT-IMAGE . RIGHT-IMAGE) cons cells, one per walk frame.")

(defvar emacs-goose--frame-index 0
  "Index into `emacs-goose--walk-frames' for the current walk frame.")

(defvar emacs-goose--idle-frames nil
  "Vector of (LEFT-IMAGE . RIGHT-IMAGE) cons cells, one per idle frame.")

(defvar emacs-goose--idle-frame-index 0
  "Index into `emacs-goose--idle-frames' for the current idle frame.")

(defvar emacs-goose--sleep-frames nil
  "Vector of (LEFT-IMAGE . RIGHT-IMAGE) cons cells, one per sleep frame.")

(defvar emacs-goose--sleep-frame-index 0
  "Index into `emacs-goose--sleep-frames' for the current sleep frame.")

(defvar emacs-goose--child-frame nil
  "Child frame used to display the goose floating over the buffer.")

(defconst emacs-goose--buffer-name " *emacs-goose*"
  "Name of the hidden buffer holding the goose image.")

;;;; image building

(defun emacs-goose--build-xpm (rows colors)
  "Build an XPM image string from ROWS using the COLORS color map."
  (unless rows
    (error "Cannot build XPM from empty sprite rows"))
  (let ((width (length (car rows)))
        (height (length rows)))
    (concat
     "/* XPM */\nstatic char *goose[] = {\n"
     (format "\"%d %d %d 1\",\n" width height (length colors))
     (mapconcat (lambda (c) (format "\"%s\"" c)) colors ",\n")
     ",\n"
     (mapconcat (lambda (row) (format "\"%s\"" row)) rows ",\n")
     "\n};")))

(defun emacs-goose--mirror-rows (rows)
  "Return ROWS mirrored horizontally."
  (mapcar #'reverse rows))

(defun emacs-goose--load-sprite-file (file)
  "Read FILE and return a list of pixel-row strings.
Trailing blank lines are dropped; remaining rows are padded with spaces
to the width of the longest row."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((lines (split-string (buffer-string) "\n"))
           (lines (let ((l (reverse lines)))
                    (while (and l (string-empty-p (car l)))
                      (setq l (cdr l)))
                    (nreverse l)))
           (max-w (apply #'max (mapcar #'length lines))))
      (mapcar (lambda (row)
                (if (< (length row) max-w)
                    (concat row (make-string (- max-w (length row)) ?\s))
                  row))
              lines))))

(defun emacs-goose--make-frame (rows)
  "Build the left/right images for one sprite from its pixel ROWS.
Rows are doubled vertically, since character cells are about twice as tall
as they are wide, then scaled to a consistent on-screen height."
  (let* ((doubled (cl-mapcan (lambda (row) (list row row)) rows))
         (target-height (* (frame-char-height)
                           (frame-scale-factor (selected-frame))
                           emacs-goose--height-factor))
         (scale (/ target-height (float (length doubled))))
         (xpm-left (emacs-goose--build-xpm doubled emacs-goose--sprite-colors))
         (xpm-right (emacs-goose--build-xpm (emacs-goose--mirror-rows doubled)
                                            emacs-goose--sprite-colors)))
    (cons (create-image xpm-left  'xpm t :ascent 'center :scale scale)
          (create-image xpm-right 'xpm t :ascent 'center :scale scale))))

(defun emacs-goose--load-frames (files)
  "Load sprite frames from FILES, skipping unreadable ones.
FILES are names relative to `emacs-goose--sprite-dir'."
  (vconcat
   (mapcar #'emacs-goose--make-frame
           (cl-loop for f in files
                    for path = (expand-file-name f emacs-goose--sprite-dir)
                    when (file-readable-p path)
                    collect (emacs-goose--load-sprite-file path)))))

(defun emacs-goose--create-images ()
  "Create and cache walk, idle, and sleep frames."
  (when (and (display-graphic-p) (image-type-available-p 'xpm))
    (setq emacs-goose--walk-frames  (emacs-goose--load-frames emacs-goose--walk-files)
          emacs-goose--idle-frames  (emacs-goose--load-frames emacs-goose--idle-files)
          emacs-goose--sleep-frames (emacs-goose--load-frames emacs-goose--sleep-files))))

;;;; child frame display

(defun emacs-goose--ensure-child-frame (iw ih)
  "Ensure the goose child frame exists with dimensions IW x IH pixels.
Creates the frame (and its buffer) if absent or no longer live."
  (unless (frame-live-p emacs-goose--child-frame)
    (setq emacs-goose--child-frame
          (make-frame `((parent-frame             . ,(selected-frame))
                        (minibuffer               . nil)
                        (pixel-width              . ,iw)
                        (pixel-height             . ,ih)
                        (child-frame-border-width . 0)
                        (internal-border-width    . 0)
                        (left-fringe              . 0)
                        (right-fringe             . 0)
                        (vertical-scroll-bars     . nil)
                        (horizontal-scroll-bars   . nil)
                        (menu-bar-lines           . 0)
                        (tool-bar-lines           . 0)
                        (tab-bar-lines            . 0)
                        (no-accept-focus          . t)
                        (no-focus-on-map          . t)
                        (undecorated              . t)
                        (unsplittable             . t)
                        (alpha-background         . 0)
                        (background-color         . "#000000")
                        (visibility               . t))))
    (with-selected-frame emacs-goose--child-frame
      (let ((buf (get-buffer-create emacs-goose--buffer-name)))
        (set-window-buffer (selected-window) buf)
        (with-current-buffer buf
          (setq-local mode-line-format nil
                      header-line-format nil
                      cursor-type nil
                      truncate-lines t
                      left-margin-width 0
                      right-margin-width 0))))))

(defun emacs-goose--set-child-frame-image (img)
  "Update the image displayed in the child frame to IMG."
  (with-current-buffer emacs-goose--buffer-name
    (erase-buffer)
    (insert (propertize " " 'display img))))

(defun emacs-goose--remove-child-frame ()
  "Delete the goose child frame and its buffer."
  (when (frame-live-p emacs-goose--child-frame)
    (delete-frame emacs-goose--child-frame))
  (setq emacs-goose--child-frame nil)
  (when-let* ((buf (get-buffer emacs-goose--buffer-name)))
    (kill-buffer buf)))

(defun emacs-goose--current-frame ()
  "Return the (LEFT-IMAGE . RIGHT-IMAGE) cons for the current state.
Returns nil when that state has no loaded sprites, so callers can skip drawing."
  (cl-flet ((pick (vec idx)
              (when (and (vectorp vec) (> (length vec) 0))
                (aref vec idx))))
    (pcase emacs-goose--state
      ('idle     (pick emacs-goose--idle-frames  emacs-goose--idle-frame-index))
      ('sleeping (pick emacs-goose--sleep-frames emacs-goose--sleep-frame-index))
      (_         (pick emacs-goose--walk-frames  emacs-goose--frame-index)))))

(defun emacs-goose--floor-pixel (frame)
  "Return the Y pixel in FRAME where the goose's feet should land."
  (let ((floor 0))
    (dolist (win (window-list frame 0))
      (unless (window-minibuffer-p win)
        (setq floor (max floor (nth 3 (window-body-pixel-edges win))))))
    (if (> floor 0) floor (frame-inner-height frame))))

(defun emacs-goose--update-child-frame ()
  "Update the goose image and reposition the child frame."
  (when (display-graphic-p)
    (let* ((state-frame (emacs-goose--current-frame))
           (img (when state-frame
                  (if (eq emacs-goose--direction 'left)
                      (car state-frame)
                    (cdr state-frame)))))
      (when img
        (let* ((parent    (selected-frame))
               (size      (image-size img t))
               (iw        (car size))
               (ih        (cdr size))
               (char-w    (frame-char-width parent))
               (parent-w  (frame-pixel-width parent))
               (x         (min (- parent-w iw)
                               (+ (* emacs-goose--position char-w)
                                  (round (* parent-w emacs-goose--h-start-fraction)))))
               (y         (- (emacs-goose--floor-pixel parent)
                             ih emacs-goose-floor-offset)))
          (emacs-goose--ensure-child-frame iw ih)
          (emacs-goose--set-child-frame-image img)
          (modify-frame-parameters emacs-goose--child-frame
                                   `((left . ,x)
                                     (top  . ,y))))))))

;;;; state machine

(defun emacs-goose--advance-frame (index frames)
  "Return the next frame index after INDEX in FRAMES vector.
Returns INDEX unchanged if FRAMES has fewer than 2 elements."
  (if (> (length frames) 1)
      (mod (1+ index) (length frames))
    index))

(defun emacs-goose--move ()
  "Advance the goose one step in the current direction, reversing at boundaries."
  (pcase emacs-goose--direction
    ('right
     (if (>= emacs-goose--position emacs-goose-max-position)
         (setq emacs-goose--direction 'left
               emacs-goose--position (1- emacs-goose--position))
       (setq emacs-goose--position (1+ emacs-goose--position))))
    ('left
     (if (<= emacs-goose--position 0)
         (setq emacs-goose--direction 'right
               emacs-goose--position (1+ emacs-goose--position))
       (setq emacs-goose--position (1- emacs-goose--position))))))

(defun emacs-goose--tick ()
  "Advance one animation frame and redraw."
  (pcase emacs-goose--state
    ('sleeping
     (setq emacs-goose--sleep-frame-index
           (emacs-goose--advance-frame emacs-goose--sleep-frame-index
                                       emacs-goose--sleep-frames)
           emacs-goose--sleep-counter
           (- emacs-goose--sleep-counter emacs-goose-tick-interval))
     (when (<= emacs-goose--sleep-counter 0)
       (setq emacs-goose--state 'walking
             emacs-goose--sleep-frame-index 0)))
    ('idle
     (setq emacs-goose--idle-frame-index
           (emacs-goose--advance-frame emacs-goose--idle-frame-index
                                       emacs-goose--idle-frames)
           emacs-goose--idle-counter (1- emacs-goose--idle-counter))
     (when (<= emacs-goose--idle-counter 0)
       (setq emacs-goose--state 'walking
             emacs-goose--idle-frame-index 0)))
    ('walking
     (cond
      ((< (cl-random 1.0) emacs-goose-sleep-chance)
       (setq emacs-goose--state 'sleeping
             emacs-goose--sleep-counter emacs-goose-sleep-duration
             emacs-goose--sleep-frame-index 0))
      ((< (cl-random 1.0) emacs-goose-idle-chance)
       (setq emacs-goose--state 'idle
             emacs-goose--idle-counter emacs-goose-idle-duration))
      (t
       (emacs-goose--move)
       (setq emacs-goose--frame-index
             (emacs-goose--advance-frame emacs-goose--frame-index
                                         emacs-goose--walk-frames))))))
  (emacs-goose--update-child-frame))

;;;; timer management

(defun emacs-goose--start-timer ()
  "Start the animation timer."
  (unless emacs-goose--timer
    (setq emacs-goose--timer
          (run-with-timer emacs-goose-tick-interval
                          emacs-goose-tick-interval
                          #'emacs-goose--tick))))

(defun emacs-goose--stop-timer ()
  "Stop the animation timer."
  (when emacs-goose--timer
    (cancel-timer emacs-goose--timer)
    (setq emacs-goose--timer nil)))

;;;###autoload
(define-minor-mode emacs-goose-mode
  "Toggle an animated goose floating over the Emacs frame."
  :global t
  (if emacs-goose-mode
      (progn
        (setq emacs-goose--position 0
              emacs-goose--direction 'right
              emacs-goose--state 'walking
              emacs-goose--idle-counter 0
              emacs-goose--sleep-counter 0
              emacs-goose--frame-index 0
              emacs-goose--idle-frame-index 0
              emacs-goose--sleep-frame-index 0)
        (emacs-goose--create-images)
        (add-hook 'window-size-change-functions #'emacs-goose--on-size-change)
        (emacs-goose--start-timer))
    (remove-hook 'window-size-change-functions #'emacs-goose--on-size-change)
    (emacs-goose--stop-timer)
    (emacs-goose--remove-child-frame)))

;;;; live repositioning

(defun emacs-goose--on-size-change (frame)
  "Reposition the goose to track the mode line live on a layout change in FRAME."
  (when (and emacs-goose-mode
             (frame-live-p emacs-goose--child-frame)
             (not (eq frame emacs-goose--child-frame)))
    (emacs-goose--update-child-frame)))

(provide 'emacs-goose)
;;; emacs-goose.el ends here
