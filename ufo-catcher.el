;;; ufo-catcher.el  --- Visually select regions in emacs                         -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Version: 1.0.0
;; Author: Donovan Miller
;; Package-Requires: ((emacs "26.1"))
;; Keywords: tools
;; URL: https://github.com/dmille56/ufo-catcher

;;; Commentary:

;; 

;;; Code:

;; :TODO: add eask

(require 'cl-lib)

(defgroup ufo-catcher nil
  "Customize group for ufo-catcher.el."
  :group 'emacs)

(defcustom ufo-catcher-blackout-buffer-enable t
"Enable blacking out buffer background when selecting."
  :type 'boolean
  :group 'ufo-catcher)

(defcustom ufo-catcher-padding-amount 4
"Amount to pad each select entry."
  :type 'integer
  :group 'ufo-catcher)

;; 32 = space, 45 = -
(defcustom ufo-catcher-padding-character 32
"Character to pad each select entry with."
  :type 'character
  :group 'ufo-catcher)

;; Dracula theme colors
(defcustom ufo-catcher-faces
  '(
    (:background "#8be9fd" :foreground "#282a36")
    (:background "#50fa7b" :foreground "#282a36")
    (:background "#ffb86c" :foreground "#282a36")
    (:background "#ff79c6" :foreground "#282a36")
    (:background "#bd93f9" :foreground "#282a36")
    (:background "#ff5555" :foreground "#282a36")
    (:background "#f1fa8c" :foreground "#282a36")
    )
  "List of faces to use when selecting regions."
  :type '(repeat face)
  :group 'ufo-catcher)

(defcustom ufo-catcher-buffer-blackout-face '(:background "#282a36" :foreground "#6272a4")
"Face to use when blacking out the background when selecting."
  :type 'face
  :group 'ufo-catcher)

(defcustom ufo-catcher-keyboard-characters-list '("a" "s" "d" "f" "g" "h" "j" "k" "l" "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" ";" "'" "z" "x" "c" "v" "b" "n" "m" "," "." "[" "]")
"Character strings to use for selecting."
  :type '(repeat string)
  :group 'ufo-catcher)

(defun ufo-catcher-pad-string (str pad-length)
"Pad STR with spaces so that it is at least PAD-LENGTH characters long."
  (if (< (length str) pad-length)
      (concat str (make-string (- pad-length (length str)) ufo-catcher-padding-character))
    str))

(defun ufo-catcher-expand-keyboard-characters (characters list-length padding-length)
"Expand CHARACTERS into a list of strings equal to LIST-LENGTH.
Pad the strings to atleast PADDING-LENGTH."
  (let ((result '())
        (current-string ""))
    (if (< list-length (length characters))
        (setq result (cl-subseq characters 0 list-length))
      (progn
        (setq result (reverse (cl-subseq characters 0 (length characters))))
        (catch 'done
          (dolist (char1 characters)
            (dolist (char2 characters)
              (setq current-string (concat char1 char2))
              (push current-string result)
              (if (>= (length result) list-length) (throw 'done t)))))
        (setq result (reverse result))))
    (mapcar (lambda (x) (ufo-catcher-pad-string x padding-length)) result)))

(defun ufo-catcher-adjust-list-length (input-list target-length)
  "Adjust the length of INPUT-LIST to TARGET-LENGTH.
By repeating or truncating elements."
  (let ((result ()))  ;; Initialize an empty list to store the result.
    ;; Loop and construct the list with the required elements.
    (cl-loop for i from 0 below target-length
             do (push (nth (mod i (length input-list)) input-list) result))
    ;; Reverse the list to maintain the original order and return.
    (nreverse result)))

(defun ufo-catcher-catch (regions)
  "Start a session to dynamically overlay REGIONS and return the match."
  (interactive)
  (let* ((input "")
         (strings (ufo-catcher-expand-keyboard-characters ufo-catcher-keyboard-characters-list (length regions) ufo-catcher-padding-amount))
         (faces (ufo-catcher-adjust-list-length ufo-catcher-faces (length regions)))
         (abort 'nil)
         (res 'nil)
         (blackout-overlay 'nil)
         (overlays (mapcar (lambda (region)
                             (let* ((start (car region)))
                               (make-overlay start (+ start (length (car strings))))))
                           regions))
         (overlays2 (mapcar (lambda (region)
                              (let* ((end (cdr region)))
                                (make-overlay end (+ end (length (car strings))))))
                            regions)))
    (if ufo-catcher-blackout-buffer-enable (progn
                                               (setq blackout-overlay (make-overlay (window-start) (window-end)))
                                               (overlay-put blackout-overlay 'face ufo-catcher-buffer-blackout-face)))
    ;; Initial overlay setup
    (cl-loop for ov in overlays
             for ov2 in overlays2
             for str in strings
             for face in faces
             do (overlay-put ov 'display str)
             (overlay-put ov2 'display str)
             (overlay-put ov 'face face)
             (overlay-put ov2 'face face))
    ;; User input loop
    (setq abort
          (catch 'exit
            (while t
              (let ((char (read-char-exclusive "Type next character (RET or ESC to abort): ")))
                ;; Exit on RET or ESC
                (if (or (eq char 13) (eq char 27)) ;; 13 = Return, 27 = Escape
                    (throw 'exit t)
                  (setq input (concat input (char-to-string char))))
                ;; Update or clear overlays based on input
                (cl-loop for ov in overlays
                         for ov2 in overlays2
                         for str in strings
                         if (string-prefix-p input str)
                         do (overlay-put ov 'display (substring str (length input)))
                         (overlay-put ov2 'display (substring str (length input)))
                         else do (delete-overlay ov)
                         (delete-overlay ov2))
                ;; Check for completion
                (let ((remaining (cl-remove-if-not (lambda (s) (string-prefix-p input s)) strings)))
                  (when (= (length remaining) 1)
                    (let ((final-pos (nth (cl-position (car remaining) strings :test 'equal) regions)))
                      (setq res final-pos)
                      (mapc 'delete-overlay overlays)
                      (mapc 'delete-overlay overlays2)
                      (throw 'exit 'nil))))))))
    ;; Remove overlays if aborted
    (if (eq abort t)
        (progn
          (message "Aborted.")
          (mapc 'delete-overlay overlays)
          (mapc 'delete-overlay overlays2)))
    (if ufo-catcher-blackout-buffer-enable (delete-overlay blackout-overlay))
    res))

(defun ufo-catcher-generate-random-visible-buffer-regions ()
"Generate 5 random regions within the visible part of the current buffer."
  (interactive)
  (let ((regions '())
        (start (window-start))
        (end (window-end)))
    (dotimes (_ 5 regions)
      (let* ((region-start (+ start (random (1+ (- end start)))))
             (region-end (+ region-start (random (1+ (- end region-start))))))
        (push (cons region-start region-end) regions)))
    regions))

(defun ufo-catcher-test-overlay ()
"Function to test ufo-catcher."
  (interactive)
  (let ((pos (ufo-catcher-catch (sort (ufo-catcher-generate-random-visible-buffer-regions) (lambda (a b) (< (car a) (car b)))))))
    (if pos (goto-char (car pos)))))

(global-set-key (kbd "<f8>") 'ufo-catcher-test-overlay)

(provide 'ufo-catcher)
;;; ufo-catcher.el ends here
