# Juvix mode
The Juvix Emacs mode

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
