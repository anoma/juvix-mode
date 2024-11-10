;;; juvix-base.el --- Shared juvix library

;;; Commentary:
;; See https://github.com/anoma/juvix-mode

(require 'posframe)
(require 'juvix-customize)

;;; Code:

(defvar juvix-doc-buffer-name "*juvix-doc-buffer*")

(defvar juvix-error-buffer-name "*juvix-error-buffer*")
(defun juvix-error-buffer ()
  "Return the juvix error buffer and create it if it does not exist."
  (get-buffer-create juvix-error-buffer-name))

(defvar juvix-log-buffer-name "*juvix-log-buffer*")
(defun juvix-log-buffer ()
  "Return the juvix log buffer and create it if it does not exist."
  (get-buffer-create juvix-log-buffer-name))

(defun juvix-log (FORMAT-STRING &rest ARGS)
  "Format FORMAT-STRING with ARGS and print it to the juvix-log-buffer."
  (with-current-buffer (juvix-log-buffer)
    (insert (apply #'format FORMAT-STRING ARGS) "\n")))

(defun juvix-get-global-flags ()
  "Return the Juvix global flags as a list."
  (interactive)
  (append juvix-global-flags
          (delq nil (list
                     "--no-colors"
                     (if juvix-disable-embedded-stdlib "--no-stdlib" nil)
                     (if juvix-stdlib-path (concat "--stdlib-path" juvix-stdlib-path) nil)))))

(defun posframe-info (TEXT &optional INIT)
  "Create the info frame with some TEXT.

Example: (posframe-info \"some help text\")

TEXT specifies the contents of the created buffer. If nil, all
posframes will be deleted.

INIT will be evaluated on the created frame."

  (if TEXT
      (when (posframe-workable-p)
        (progn
          (posframe-show juvix-doc-buffer-name
                         :string TEXT
                         :border-color "purple"
                         :poshandler 'posframe-poshandler-window-top-right-corner
                         :border-width 1
                         :hidehandler 'posframe-hidehandler-when-buffer-switch
                         :position (point))
          (when INIT
            (with-current-buffer juvix-doc-buffer-name
              (eval INIT)))))
    (posframe-delete-all)))

(defun juvix-call (&optional STDOUT_BUFFER &rest ARGS)
  "Call the Juvix compiler.

Example: (call-juvix (current-buffer) \"dev\" \"root\")

STDOUT_BUFFER specifies the buffer where the *standard output* goes.

ARGS are the command-line arguments passed to the Juvix
compiler.

Returns the exit code of the Juvix command. A return
value of 0 indicates success; any other value indicates failure.
On failure, stderr will be pasted to juvix-error-buffer"

  ;; we create a temporary file to redirect stderr there. It is not possible to redirect to a buffer
  (let ((tmp-err-file (make-temp-file "temporary-stderr-file")))
    (with-temp-file tmp-err-file
      (let* ((global-flags (juvix-get-global-flags))
             (ALL_ARGS (append global-flags ARGS))
             (destination (list STDOUT_BUFFER tmp-err-file))
             (exit-code (apply #'call-process "juvix" nil destination nil ALL_ARGS)))
        (unless (zerop exit-code)
          (with-current-buffer (juvix-error-buffer)
            (erase-buffer)
            (goto-char (point-max))
            (insert-file-contents tmp-err-file)
            (display-buffer (juvix-error-buffer) 'display-buffer-at-bottom)
            (message "Juvix failed. Read the error message in the buffer %s" juvix-error-buffer-name)))
        exit-code))))

(defun juvix-call-read (&rest ARGS)
  "Call the Juvix compiler and return stdout or nil if it failed.

ARGS are the command-line arguments passed to the Juvix
compiler."
  (with-temp-buffer
    (let
        ((exit-code (apply #'juvix-call (current-buffer) ARGS)))
      (if (zerop exit-code)
          (buffer-string)
        nil))))

(defun juvix-version ()
  "Return the juvix version."
  (let ((res (juvix-call-read "--version")))
    (if res
        (car (split-string res "\n"))
      nil)))

(defun juvix-clear-annotations ()
  "Remove all face annotations from the current buffer."
  (interactive)
  (remove-list-of-text-properties (point-min) (point-max) '(face juvix-info juvix-format juvix-goto)))

(provide 'juvix-base)
;;; juvix-base.el ends here
