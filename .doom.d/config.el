;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;;
;;; Things to check:
;;; - iedit?

;; Base Emacs configuration

(setq indicate-buffer-boundaries 'left indicate-empty-lines t)
(setq recenter-positions '(top middle bottom)) ; for C-l and M-r
(setq next-screen-context-lines 2)
(setq indent-tabs-mode nil) ; no tabs
(setq kill-whole-line t)
(setq mouse-wheel-progressive-speed nil)
;; (setq display-line-numbers-type t)
(when IS-MAC (setq mac-command-modifier 'meta) (setq mac-option-modifier nil))

(put 'upcase-region 'disabled nil) ; C-x C-u
(put 'downcase-region 'disabled nil) ; C-x C-l
(put 'narrow-to-region 'disabled nil) ; C-x n n
(put 'suspend-frame 'disabled t)

(windmove-default-keybindings 'meta) ; M-<arrows>
(global-set-key (kbd "<C-S-up>")    'buf-move-up)
(global-set-key (kbd "<C-S-down>")  'buf-move-down)
(global-set-key (kbd "<C-S-left>")  'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)
(global-set-key (kbd "<f2>")        'magit-status)
(global-set-key (kbd "<f6>")        'smex)
(global-set-key (kbd "C-`")         'toggle-truncate-lines)
(global-set-key (kbd "C-c <left>")  'winner-undo) ; restore default keybindings
(global-set-key (kbd "C-c <right>") 'winner-redo) ; restore default keybindings
(global-set-key (kbd "C-x O")       (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "M-;")         'comment-line) ; so confused

;; Doom Emacs configuration
(setq doom-theme 'doom-solarized-light)
(setq doom-big-font-increment 2)  ; big font mode is +2
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

(after! avy
  (setq avy-keys (number-sequence ?a ?z)))

;; Evil configuration
;; TODO: replace define-key with map!
;; https://github.com/noctuid/evil-guide/blob/master/README.org
(after! evil
  ;; move by visual line
  ;; TODO: check evil settings (https://evil.readthedocs.io/en/latest/settings.html)
  ;; want visual line movement, but not on other commands (e.g. delete, visual
  ;; select should still operate on the logical row)
  ;; (define-key evil-motion-state-map "j" "gj")
  ;; (define-key evil-motion-state-map "k" "gk")
  ;; (define-key evil-motion-state-map "$" "g$")
  ;; (define-key evil-motion-state-map "^" "g^")
  ;; (define-key evil-motion-state-map "0" "g0")

  (define-key evil-normal-state-map (kbd "g SPC") 'avy-goto-word-1)
  (define-key evil-normal-state-map (kbd "C-w") 'kill-region)
  (define-key evil-normal-state-map (kbd "C-a") 'move-beginning-of-line)
  (define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-normal-state-map (kbd "C-y") 'yank)
  (define-key evil-normal-state-map (kbd "=") 'er/expand-region)

  ;; https://github.com/hlissner/doom-emacs/blob/master/modules/lang/markdown/config.el
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
  (define-key evil-insert-state-map (kbd "C-w") 'kill-region)
  (define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-insert-state-map (kbd "C-y") 'yank)
  (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
  (define-key evil-insert-state-map (kbd "C-p") 'previous-line)
  (define-key evil-insert-state-map (kbd "C-n") 'next-line)

  (define-key evil-visual-state-map (kbd "g SPC") 'avy-goto-word-1)
  (define-key evil-visual-state-map (kbd "C-w") 'kill-region)
  (define-key evil-visual-state-map (kbd "C-a") 'move-beginning-of-line)
  (define-key evil-visual-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-visual-state-map (kbd "C-y") 'yank)
  (define-key evil-visual-state-map (kbd "=") 'er/expand-region))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(load! "language-specifics.el")

(defun toggle-frame-split ()
  "Rotate from horizontal/vertical split to the opposite (bound to C-c r)."
  (interactive)
  (unless (= (count-windows) 2)
    (error "Can only toggle a window split in two"))
  (let*
      ((this-buffer (window-buffer))
       (this-edges (window-edges))
       (this-window-point (window-point))
       (next-buffer (window-buffer (next-window)))
       (next-edges (window-edges (next-window)))
       (next-window-point (window-point (next-window)))
       (this-2nd
        (not (and (<= (car this-edges) (car next-edges)) ; left
                  (<= (cadr this-edges) (cadr next-edges))))) ; top
       (splitter
        (if (= (car this-edges) (car next-edges))
            'split-window-horizontally 'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-2nd (other-window 1))
      (set-window-buffer (selected-window) this-buffer)
      (set-window-point (selected-window) this-window-point)
      (set-window-buffer (next-window) next-buffer)
      (set-window-point (next-window) next-window-point)
      (select-window first-win)
      (if this-2nd (other-window 1)))))
(global-set-key (kbd "C-c r") 'toggle-frame-split)

(defun reload-file ()
  "Reload a buffer's file (bound to <f5>)."
  (interactive)
  (revert-buffer t (not (buffer-modified-p)) t)
  (font-lock-ensure))
(global-set-key (kbd "<f5>") 'reload-file)

(add-hook! text-mode #'visual-line-mode)
