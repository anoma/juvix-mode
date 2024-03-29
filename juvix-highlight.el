(require 'font-lock)

(defgroup juvix-highlight nil
  "Syntax highlighting for Juvix."
  :group 'juvix)

(defgroup juvix-highlight-faces nil
  "Faces used to highlight Juvix code."
  :group 'juvix-highlight)

(defface juvix-highlight-keyword-face
  '((((background light))
     (:foreground "#399ee6"))
    (((background dark))
     (:foreground "#81a1c1")))
  "The face used for keywords."
  :group 'juvix-highlight-faces)

(defface juvix-highlight-module-face
  '((((background light))
     (:foreground "#478acc"))
    (((background dark))
     (:foreground "#88c0d0")))
  "The face used for modules."
  :group 'juvix-highlight-faces)

(defface juvix-highlight-function-face
  '((((background light))
     (:foreground "#f2ae49"))
    (((background dark))
     (:foreground "#ebcb8b")))
  "The face used for functions."
  :group 'juvix-highlight-faces)

(defface juvix-highlight-type-face
  '((((background light))
     (:foreground "#86b300"))
    (((background dark))
     (:foreground "#a3be8c")))
  "The face used for inductive types."
  :group 'juvix-highlight-faces)

(defface juvix-highlight-constructor-face
  '((((background light))
     (:foreground "#a37acc"))
    (((background dark))
     (:foreground "#b48ead")))
  "The face used for constructors."
  :group 'juvix-highlight-faces)

(defface juvix-highlight-axiom-face
  '((((background light))
     (:foreground "#f07171"))
    (((background dark))
     (:foreground "#bf616a")))
  "The face used for axioms."
  :group 'juvix-highlight-faces)

(defface juvix-highlight-string-face
  '((((background light))
     (:foreground "#f07171"))
    (((background dark))
     (:foreground "#d08770")))
  "The face used for string literals."
  :group 'juvix-highlight-faces)

(defface juvix-highlight-number-face
  '((((background light))
     (:foreground "#000000"))
    (((background dark))
     (:foreground "#d8dee9")))
  "The face used for numbers."
  :group 'juvix-highlight-faces)

(defface juvix-highlight-error-face
  '((((background light))
     (:foreground "#bd3744"))
    (((background dark))
     (:foreground "#bd3744")))
  "The face used for errors."
  :group 'juvix-highlight-faces)

(defface juvix-highlight-delimiter-face
  '((((background light))
     (:foreground "#ba91992e"))
    (((background dark))
     (:foreground "#5e81ac")))
  "The face used for judoc delimiters: semicolons, parentheses, braces and pipes."
  :group 'juvix-highlight-faces)

(defface juvix-highlight-pragma-face
  '((((background light))
     (:foreground "#85714c"))
    (((background dark))
     (:foreground "#cca458")))
  "The face used for pragma comments."
  :group 'juvix-highlight-faces)

(defface juvix-highlight-fixity-face
  '((((background light))
     (:foreground "#ff7f50"))
    (((background dark))
     (:foreground "#deb887")))
  "The face used for fixities."
  :group 'juvix-highlight-faces)

(defface juvix-highlight-judoc-face
  '((((background light))
     (:foreground "#8b2252"
      :slant italic))
    (((background dark))
     (:foreground "#8fbcbb"
      :slant italic)))
  "The face used for judoc comments."
  :group 'juvix-highlight-faces)

(defface juvix-highlight-comment-face
  '((((background light))
     (:foreground "#ba919966"
      :slant italic))
    (((background dark))
     (:foreground "#83898d"
      :slant italic)))
  "The face used for comments."
  :group 'juvix-highlight-faces)

(provide 'juvix-highlight)
