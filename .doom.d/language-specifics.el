;;; $DOOMDIR/language-specifics.el -*- lexical-binding: t; -*-

(setq-default
  c-default-style "bsd"
  c-basic-offset 2)

;; Open bash alias files with sh-mode
(add-to-list 'auto-mode-alist '("\\.?*bash_aliases\\'" . sh-mode))

;; Markdown
(remove-hook 'markdown-mode-hook 'evil-markdown-mode)
