(require 'juvix-customize)
(require 'juvix-highlight)
(require 'juvix-input)
(require 'flycheck-juvix)
(require 'juvix-repl)

(require 'straight)

(straight-use-package 'posframe)
;; afaik the following requires are only needed to get rid of the warnings
(require 'posframe)

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

(defun juvix-version ()
  "Return the juvix version."
  (let ((version (car (split-string (shell-command-to-string "juvix --version")
                                    "\n"))))
    (if (string-prefix-p "Juvix" version)
        version
      "Juvix")))

(defvar juvix-doc-buffer-name " *juvix-doc-buffer*")

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
  (let ((filename (buffer-file-name)))
    (when (and filename
               (= (buffer-size) 0))
      (let* ((root
              (file-name-as-directory (string-trim (shell-command-to-string (concat "juvix dev root " filename)))))
             (relative-name
              (file-relative-name filename root))
             (in-project
              (string-prefix-p root filename))
             (module-name
              (file-name-sans-extension (if in-project
                                            (replace-regexp-in-string "/" "." relative-name)
                                          (file-name-nondirectory filename)))))
        (message "Root: %s" root)
        (message "Relative Name: %s" relative-name)
        (message "In Project: %s" in-project)
        (message "Module Name: %s" module-name)
        (insert (concat "module " module-name ";\n"))
        (juvix-load)))))

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
       (juvix-insert-top-module-name)))))

(defun juvix-clear-annotations ()
  "Remove all annotations from the current buffer."
  (interactive)
  (remove-list-of-text-properties (point-min) (point-max) '(face)))

(defun juvix-load ()
  "Load current buffer."
  (interactive)
  (save-buffer)
  (juvix-clear-annotations)
  (eval (read (shell-command-to-string
               (concat "juvix " (concat (mapconcat 'identity juvix-global-flags " ") " ") (if juvix-disable-embedded-stdlib "--no-stdlib " "") (if juvix-stdlib-path (concat "--stdlib-path " juvix-stdlib-path " ") "") "dev highlight "
                       (buffer-file-name)))))
  (save-buffer)
  (juvix-repl-load-file (buffer-file-name)))

(defun juvix-format-buffer ()
  "Format the current buffer."
  (interactive)
  (let ((old-point (point))
        (buff-name (buffer-file-name))
        (buff (current-buffer)))
    (with-temp-buffer
      (let ((cmd-str (concat "juvix " (concat (mapconcat 'identity juvix-global-flags " ") " ") (if juvix-disable-embedded-stdlib "--no-stdlib " "") "format "
                             buff-name)))
        (if (zerop (call-process-shell-command
                    cmd-str
                    nil
                    t))
            (progn
              (let ((text (buffer-string)))
                (with-current-buffer buff
                  (erase-buffer)
                  (insert text)
                  (goto-char old-point)
                  (juvix-load))))
          (message "error formatting the buffer"))))))

(defun juvix-goto-definition ()
  "Go to the definition of the symbol at point."
  (interactive)
  (let ((goto-info (get-text-property (point) 'juvix-goto)))
    (if goto-info
        (progn
          (find-file (car goto-info))
          (juvix-load)
          (goto-char (cdr goto-info)))
      (message "No goto information found at cursor"))))

(provide 'juvix-mode)
