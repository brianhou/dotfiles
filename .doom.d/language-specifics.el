;;; $DOOMDIR/language-specifics.el -*- lexical-binding: t; -*-

(setq-default
  c-default-style "bsd"
  c-basic-offset 2)

;; Open bash alias files with sh-mode
(add-to-list 'auto-mode-alist '("\\.?*bash_aliases\\'" . sh-mode))

;; ROS launch files
(add-to-list 'auto-mode-alist '("\\.launch\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.test\\'" . xml-mode))

;; LaTeX and BibTeX
(defun TeX-word-count (&optional)
  (interactive)
  (let ((command "texcount -sum -1 ")
        (file (buffer-name)))
    (shell-command (concat command file))))
;; (map! :after latex :map latex-mode-map :n "C-c C-w" #'TeX-word-count)
(add-hook! latex-mode
  (local-set-key (kbd "C-c C-w") 'TeX-word-count))

(setq-hook! bibtex-mode
  fill-column (point-max)
  TeX-auto-save nil
  bibtex-field-indentation 2
  bibtex-text-indentation (+ 2 (length "publisher = "))
  bibtex-align-at-equal-sign t
  bibtex-completion-bibliography "~/Documents/Library/references.bib"
  bibtex-completion-library-path "~/Documents/Library/bibtex-pdfs"
  org-ref-bibtex-sort-order
  '(("article"  . ("title" "author" "journal" "year" "volume" "number" "pages"))
    ("inproceedings" . ("title" "author" "booktitle" "year"))))

(after! bibtex
  (defun bibtex-generate-autokey ()
    "Automatically generate a key for a BibTeX entry: [author][year][annote/title]."
    (let* ((bibtex-autokey-names 1)
           (bibtex-autokey-year-length 4)
           (bibtex-autokey-titleword-length nil)
           (bibtex-autokey-titleword-separator "")
           (bibtex-autokey-title-terminators " ")
           (author (bibtex-autokey-get-names))
           (year (bibtex-autokey-get-year))
           (annote (bibtex-autokey-get-field "annote"))
           (title (bibtex-autokey-get-title))
           key)
      (format "%s%s%s" author year (if (s-blank? annote) title annote)))))

(map! :after bibtex :map bibtex-mode-map :leader :nv "cf" #'org-ref-clean-bibtex-entry)
