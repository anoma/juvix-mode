# Juvix mode
The Juvix Emacs mode

## Installation

### Using straight.el

This installs juvix-mode, to your emacs config

```emacs-lisp
(use-package juvix-mode :straight (:host github :repo "anoma/juvix-mode")
  :mode ("\\.\\(juvix\\)$" . juvix-mode))
```

### Manually

To install juvix-mode, clone this repository to a path of your choice:
``` shell
git clone https://github.com/anoma/juvix-mode /path/of/choice/juvix-mode
```

Then add these lines to your configuration file:
``` emacs-lisp
;; add this only if you don't have flycheck already
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; add this only if you don't have posframe already
(use-package posframe
  :ensure t)

(push "/path/of/choice/juvix-mode" load-path)
(require 'juvix-mode nil t)
```

## Features
Load a file with `M-x juvix-load` or `SPC m l` with evil mode.
After loading, move the cursor to an identifier to see its type and documentation.
![load-file](https://github.com/anoma/juvix-mode/assets/5511599/8fcc3570-b607-4a0c-ab66-e17c03a491c3)

Jump to definition with `M-x juvix-goto-definition` or `g d` with evil mode.
![goto](https://github.com/anoma/juvix-mode/assets/5511599/6c477907-59b3-43ad-b4bd-418c6a2c9862)

Get inline error messages with flycheck.
![error](https://github.com/anoma/juvix-mode/assets/5511599/9b158ff8-0c91-41ca-93dc-ab568a55ca4c)

Format your code automatically with `M-x juvix-format-buffer` or `SPC m f`.
![format](https://github.com/anoma/juvix-mode/assets/5511599/4b922601-3255-4949-a17f-b7e120263c56)

## Known issues
- Incompatible with [Doom indent-guides](https://docs.doomemacs.org/v21.12/modules/ui/indent-guides/).
