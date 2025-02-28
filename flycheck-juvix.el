;;; flycheck-juvix --- Flycheck support for the Juvix programming language

;;; Commentary:
;;; See https://github.com/anoma/juvix-mode

(require 'flycheck)
(require 'juvix-base)
(require 'juvix-customize)

;;; Code:

(defgroup flycheck-juvix nil
  "Juvix support for Flycheck."
  :prefix "flycheck-juvix-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/anoma/juvix"))

(flycheck-define-checker juvix
  "A Juvix syntax checker."
  :command ("juvix"
            (eval juvix-global-flags)
            "--ide-end-error-char" (eval juvix-end-error-char)
            "--no-colors"
            "--stdin" (option-flag "--no-stdlib" juvix-disable-embedded-stdlib)
            (option "--stdlib-path" juvix-stdlib-path)
            "typecheck"
            source-original)
  :standard-input t
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": error:" (eval rx-message))
   (error line-start (file-name) ":" line ":" column "-" end-column ": error:" (eval rx-message))
   (error line-start (file-name) ":" line "-" end-line ":" column ": error:" (eval rx-message))
   (error line-start (file-name) ":" line "-" end-line ":" column "-" end-column ": error:" (eval rx-message)))
  :modes juvix-mode)

(add-to-list 'flycheck-checkers 'juvix)

(provide 'flycheck-juvix)
;;; flycheck-juvix.el ends here
