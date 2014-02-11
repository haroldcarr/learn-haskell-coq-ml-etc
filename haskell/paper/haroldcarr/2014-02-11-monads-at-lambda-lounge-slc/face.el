;; https://bbs.archlinux.org/viewtopic.php?id=88612

;; M-x list-colours-display

(defface hc-haskell-keyword-face
  '((t (:foreground "brown")))
  "Custom face for `haskell-keyword-face'."
  :group 'haskell)

(defface hc-haskell-constructor-face
  '((t (:foreground "red")))
  "Custom face for `haskell-constructor-face'."
  :group 'haskell)

(defface hc-haskell-definition-face
  '((t (:foreground "blue")))
  "Custom face for `haskell-definition-face'."
  :group 'haskell)

(defface hc-haskell-operator-face
  '((t (:foreground "brown")))
  "Custom face for `haskell-operator-face'."
  :group 'haskell)

(defun hc-haskell-faces ()
  (setq haskell-keyword-face          'hc-haskell-keyword-face)
  (setq haskell-constructor-face      'hc-haskell-constructor-face)
  (setq haskell-definition-face       'hc-haskell-definition-face)
  (setq haskell-operator-face         'hc-haskell-operator-face)
  (setq haskell-default-face          nil)
  (setq haskell-literate-comment-face 'font-lock-doc-face))

(defun haskell-default-faces ()
  (setq haskell-keyword-face          'font-lock-keyword-face)
  (setq haskell-constructor-face      'font-lock-type-face)
  (setq haskell-definition-face       'font-lock-function-name-face)
  (setq haskell-operator-face         'font-lock-variable-name-face)
  (setq haskell-default-face          nil)
  (setq haskell-literate-comment-face 'font-lock-doc-face))

(hc-haskell-faces)
;; (haskell-default-faces)
