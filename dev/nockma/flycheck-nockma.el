;;; flycheck-nockma --- Flycheck support for the Nockma programming language

;;; Commentary:
;;; See https://github.com/anoma/juvix-mode

(require 'flycheck)
(require 'juvix-customize)
(require 'juvix-base)

;;; Code:

(defgroup flycheck-nockma nil
  "Nockma support for Flycheck."
  :prefix "flycheck-nockma-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/anoma/juvix"))

(flycheck-define-checker nockma
  "A Nockma syntax checker."
  :command ("juvix"
            (eval juvix-global-flags)
            "--ide-end-error-char" (eval juvix-end-error-char)
            "--no-colors"
            "dev"
            "nockma"
            "ide"
            "check"
            source-original)
  :standard-input t
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": error:" (eval rx-message))
   (error line-start (file-name) ":" line ":" column "-" end-column ": error:" (eval rx-message))
   (error line-start (file-name) ":" line "-" end-line ":" column ": error:" (eval rx-message))
   (error line-start (file-name) ":" line "-" end-line ":" column "-" end-column ": error:" (eval rx-message)))
  :modes nockma-mode)

(add-to-list 'flycheck-checkers 'nockma)

(provide 'flycheck-nockma)
;;; flycheck-nockma.el ends here
