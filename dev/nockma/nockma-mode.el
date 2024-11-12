;;; nockma-mode --- Major mode for the Nockma programming language

;;; Commentary:
;;; See https://github.com/anoma/juvix-mode
(require 'juvix-base)
(require 'juvix-highlight)
(require 'flycheck-nockma)
(require 'posframe)
(require 'hideshow)

;;; Code:

(defgroup nockma nil
  "Major mode for Nockma files."
  :group 'languages)

(defvar nockma-doc-table nil
  "An internal variable.
It initialized by evaluating the output of juvix dev nockma highlight.
Maps NockOps to their documentation.")

(defvar nockma-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "Nockma")))
    (define-key map (kbd "C-c C-l") 'nockma-load)
    (define-key map (kbd "C-c C-f") 'nockma-format-buffer)
    map)
  "Keymap for Nockma mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nockma\\'" . nockma-mode))
(add-to-list 'auto-mode-alist '("\\.debug.nockma\\'" . nockma-mode))

(defun nockma-posframe-at-pt ()
  "Display info if available."
  (let* ((info-value (get-text-property (point) 'juvix-info))
         (text-and-init (gethash info-value nockma-doc-table)))
    (if text-and-init
        ;; we show documentation for the nock op
        (posframe-info (car text-and-init) (cdr text-and-init))
      (if info-value
          ;; we show some custom message
          (let ((format-info (get-text-property (point) 'juvix-format)))
            (posframe-info info-value format-info))
        (posframe-info nil)))))

(defun nockma-hide-setup ()
  "Setup hideshow for nockma-mode to hide cells."
  (setq-local hs-block-start-regexp "\\[")
  (setq-local hs-block-end-regexp "\\]")
  (setq-local hs-isearch-open t))

(defun nockma-toggle-cell ()
  "Hide/show the contents of a cell."
  (interactive)
  (hs-toggle-hiding))

(define-derived-mode nockma-mode prog-mode (juvix-version)
  (font-lock-mode 0)
  (setq-local comment-start "--")
  (nockma-hide-setup)
  (add-hook
   'nockma-mode-hook
   (lambda ()
     (with-eval-after-load 'evil
       (evil-define-key 'normal nockma-mode-map (kbd "SPC m l") 'nockma-load)
       (evil-define-key 'normal nockma-mode-map (kbd "SPC m f") 'nockma-format-buffer)
       (evil-define-key 'normal nockma-mode-map (kbd "SPC m t") 'nockma-toggle-cell)
       (evil-normalize-keymaps))
     (add-hook 'post-command-hook #'nockma-posframe-at-pt nil :local)
     (nockma-load))))

(defun nockma-load ()
  "Load and highlight the current buffer."
  (interactive)
  (save-buffer)
  (juvix-clear-annotations)
  (let ((res (juvix-call-read "dev" "nockma" "ide" "highlight" (buffer-file-name))))
    (when res
      (eval (read res))
      (save-buffer))))

(defun nockma-format-buffer ()
  "Format the current buffer."
  (interactive)
  (save-buffer)
  (let ((old-point (point))
        (file-name (buffer-file-name))
        (buff (current-buffer)))
    (with-temp-buffer
      (when (zerop (juvix-call (current-buffer) "dev" "nockma" "format" file-name))
        (let ((text (buffer-string)))
          (with-current-buffer buff
            (erase-buffer)
            (insert text)
            (goto-char old-point)
            (nockma-load)))))))

(provide 'nockma-mode)
;;; nockma-mode.el ends here
