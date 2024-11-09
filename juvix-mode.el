;;; juvix-mode --- Major mode for the Juvix programming language

;;; Commentary:
;;; See https://github.com/anoma/juvix-mode
(require 'juvix-customize)
(require 'juvix-base)
(require 'juvix-highlight)
(require 'juvix-input)
(require 'flycheck-juvix)
(require 'juvix-repl)
(require 'posframe)

;;; Code:

(defgroup juvix nil
  "Major mode for Juvix files."
  :group 'languages)

(defvar juvix-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "Juvix")))
    (define-key map (kbd "C-c C-l") 'juvix-load)
    (define-key map (kbd "M-.") 'juvix-goto-definition)
    (define-key map (kbd "C-c C-f") 'juvix-format-buffer)
    map)
  "Keymap for Juvix mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.juvix\\'" . juvix-mode))
(add-to-list 'auto-mode-alist '("\\.juvix.md\\'" . juvix-mode))

(defun juvix-posframe-at-pt ()
  "Display type if available."
  (let ((type-info (get-text-property (point) 'help-echo))
        (format-info (get-text-property (point) 'juvix-format)))
    (when (posframe-workable-p)
      (if (and type-info
               (eq major-mode 'juvix-mode))
          (progn
            (posframe-show juvix-doc-buffer-name
                           :string type-info
                           :border-color "purple"
                           :poshandler 'posframe-poshandler-window-top-right-corner
                           :border-width 1
                           :position (point))
            (when format-info
              (with-current-buffer juvix-doc-buffer-name
                (progn
                (eval format-info)))))
        (posframe-delete-all)))))

(defun juvix-insert-top-module-name ()
  "Insert the suggested top module name."
  ;; we save the buffer to guarantee it exists on the filesystem
  (save-buffer)
  (let ((filename (buffer-file-name)))
    (when (and filename
               (= (buffer-size) 0))
      (let ((res (juvix-call-read "dev" "root" filename)))
        (when res
          (let* ((root (file-name-as-directory (string-trim res)))
                 (relative-name
                  (file-relative-name filename root))
                 (in-project
                  (string-prefix-p root filename))
                 (module-name
                  (file-name-sans-extension (if in-project
                                                (replace-regexp-in-string "/" "." relative-name)
                                              (file-name-nondirectory filename)))))
            (juvix-log "Root: %s" root)
            (juvix-log "Relative Name: %s" relative-name)
            (juvix-log "In Project: %s" in-project)
            (juvix-log "Module Name: %s" module-name)
            (insert (concat "module " module-name ";\n"))
            (juvix-load)))))))

(define-derived-mode juvix-mode prog-mode (juvix-version)
  (font-lock-mode 0)
  (when juvix-auto-input-method
    (set-input-method "juvix"))
  (setq-local comment-start "--")

  (add-hook
   'juvix-mode-hook
   (lambda ()
     (with-eval-after-load 'evil
       (evil-define-key 'normal juvix-mode-map (kbd "SPC m l") 'juvix-load)
       (evil-define-key 'normal juvix-mode-map (kbd "SPC m g") 'juvix-goto-definition)
       (evil-define-key 'normal juvix-mode-map (kbd "SPC m f") 'juvix-format-buffer)
       (evil-define-key 'normal juvix-mode-map (kbd "g d") 'juvix-goto-definition)
       (evil-normalize-keymaps)
       (add-hook 'post-command-hook #'juvix-posframe-at-pt)
       (juvix-insert-top-module-name)
       (juvix-load)))))

(defun juvix-clear-annotations ()
  "Remove all face annotations from the current buffer."
  (interactive)
  (remove-list-of-text-properties (point-min) (point-max) '(face help-echo juvix-format juvix-goto)))

(defun juvix-load ()
  "Load and highlight the current buffer."
  (interactive)
  (save-buffer)
  (juvix-clear-annotations)
  (let ((res (juvix-call-read "dev" "highlight" (buffer-file-name))))
    (when res
      (eval (read res))
      (save-buffer)
      (juvix-repl-load-file (buffer-file-name)))))

(defun juvix-format-buffer ()
  "Format the current buffer."
  (interactive)
  (save-buffer)
  (let ((old-point (point))
        (file-name (buffer-file-name))
        (buff (current-buffer)))
    (with-temp-buffer
      (when (zerop (juvix-call (current-buffer) "format" file-name))
        (let ((text (buffer-string)))
          (with-current-buffer buff
            (erase-buffer)
            (insert text)
            (goto-char old-point)
            (juvix-load)))))))

(defun juvix-goto-definition ()
  "Go to the definition of the symbol at point."
  (interactive)
  (let ((goto-info (get-text-property (point) 'juvix-goto)))
    (xref-push-marker-stack)
    (if goto-info
        (progn
          (find-file (car goto-info))
          (goto-char (cdr goto-info)))
      (message "No goto information found at cursor"))))

(provide 'juvix-mode)
;;; juvix-mode.el ends here
