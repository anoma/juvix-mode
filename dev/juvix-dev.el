;;; juvix-dev.el --- Major modes for working with Juvix's internal languages

;;; Commentary:
;;; See https://github.com/anoma/juvix-mode

;;; Code:
(push (expand-file-name "nockma" (file-name-directory (or load-file-name buffer-file-name))) load-path)
(require 'nockma-mode)

(provide 'juvix-dev)
;;; juvix-dev.el ends here
