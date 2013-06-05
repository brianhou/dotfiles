(add-to-list 'load-path "~/emacs.d")
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;;; Adding themes (solarized)
(load-theme 'solarized-dark t)

;;; Customizing variables
(setq
  inhibit-startup-screen t
  inhibit-startup-buffer-menu t)
(setq-default
  fill-column 80
  recenter-positions '(top middle bottom)
  next-screen-context-lines 2
  indent-tabs-mode nil
  echo-keystrokes 0.3
  kill-whole-line 1)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;; Custom key bindings
(global-set-key (kbd "<f6>") 'execute-extended-command) ; M-x sucks
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "<f12>") 'count-lines-region)
(global-set-key (kbd "C-`") 'toggle-truncate-lines)
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

;;; Minor modes
(column-number-mode t) ; View column numbers in the mode line
(global-linum-mode t) ; Show line numbers
(global-subword-mode t) ; Jump between words intelligently
(mouse-avoidance-mode 'exile) ; Banish mouse when mark is near
(tool-bar-mode 0) ; Turn off stupid toolbar
(menu-bar-mode 0) ; Turn off stupid toolbar
(set-scroll-bar-mode nil) ; Turn off scrollbars
(winner-mode t) ; Revert to previous window configuration
(if (display-graphic-p) (progn (global-hl-line-mode t))) ; Highlight line

;;; Turning on ido mode!
(require 'ido)
(ido-mode t)

;;; Matching parentheses customizations
(show-paren-mode t)
(setq-default show-paren-delay 0)
(require 'paren)
(set-face-foreground 'show-paren-match-face "#FFF")
(set-face-attribute 'show-paren-match-face nil :weight 'ultra-bold)

;;; Adding things to auto-mode-alist
;; Make custom bash files show up in sh-mode
(let ((custom-bash-files '((".bash_aliases" . sh-mode)
                           (".private_bash_aliases" . sh-mode))))
  (setq-default auto-mode-alist (append auto-mode-alist custom-bash-files)))
;; .text = markdown-mode
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))

;; Lisp indentation things
(put 'setq 'lisp-indent-function 'defun)
(put 'setq-default 'lisp-indent-function 'defun)
(put 'add-hook 'lisp-indent-function 'defun)
(put 'if 'lisp-indent-function nil)

;;; Adding hooks
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'java-mode-hook
  (lambda ()
    (setq compile-command "javac -g") ; Compile with javac -g
    (local-set-key (kbd "C-x C-e") 'compile)))
(add-hook 'c-mode-hook
  (lambda ()
    (setq compile-command "gcc -g -o") ; Compile with gcc -g -o
    (local-set-key (kbd "C-x C-e") 'compile)))
(add-hook 'term-mode-hook
  (lambda ()
    (setq linum-mode nil)))

;;; Set persistent *scratch* buffer (todo file at ~/.todo) based off
;;; http://dorophone.blogspot.com/2011/11/how-to-make-emacs-scratch-buffer.html
;;; and http://stackoverflow.com/a/358740
(setq initial-major-mode 'markdown-mode) ; set *scratch* mode to markdown
(defadvice kill-buffer (around bury-scratch activate)
  "Bury *scratch* buffer instead of killing it."
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
        ad-do-it)))
(defvar TODO-FILENAME "~/.todo" "to-do file location")
(defun load-todo ()
  "Load the contents of TODO-FILENAME into the *scratch* buffer."
  (with-current-buffer "*scratch*"
    (delete-region (point-min) (point-max))
    (insert-file-contents TODO-FILENAME)))
(defun save-todo ()
  "Save the contents of the *scratch* buffer into TODO-FILENAME."
  (with-current-buffer "*scratch*"
    (write-region (point-min) (point-max) TODO-FILENAME)))
(push 'save-todo kill-emacs-hook) ; Save before killing
(load-todo) ; Run load-todo function at startup

;;; Packages on packages on packages...

;; ace-jump-mode
(require 'ace-jump-mode)
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back" t)
(eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(setq ace-jump-mode-gray-background nil)

;; expand-region.el
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; multiple-cursors.el
(require 'multiple-cursors)
(global-set-key (kbd "C-S-a") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-c <M-right>") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c <M-left>") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c !") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c a") 'mc/mark-all-in-region)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))

(defun zap-up-to-char (arg char)
  "Kill up to, but not including ARGth occurrence of CHAR. Case is ignored if
`case-fold-search' is non-nil in the current buffer. Goes backward if ARG is
negative; error if CHAR not found. Ignores CHAR at point."
  (interactive "p\ncZap up to char: ")
  (let ((direction (if (>= arg 0) 1 -1)))
    (kill-region (point)
                 (progn
                   (forward-char direction)
                   (unwind-protect
                       (search-forward (char-to-string char) nil nil arg)
                     (backward-char direction))
                   (point)))))
(global-set-key (kbd "M-z") 'zap-up-to-char)

(defadvice message (before test-symbol activate)
  "Includes timestamp of message."
  (if (not (string-equal (ad-get-arg 0) "%s%s"))
      (let ((deactivate-mark nil))
        (with-current-buffer "*Messages*"
          (goto-char (point-max))
          (if (not (bolp)) (newline))
          (insert (format-time-string "[%T] "))))))

(defadvice yank (around yank-and-indent)
  "Indents after yanking."
  (let ((point-before (point)))
    ad-do-it
    (indent-region point-before (point))))
(ad-activate 'yank)

(defun kill-line-and-one-space (&optional ARG)
  "Kill a line. Leave one space."
  (defun multiple-spaces ()
    "If both the character at point and one of the characters
     around it are spaces, return true."
    (let ((space 32))
      (and (char-equal (char-after) space)
           (or (char-equal (char-before) space)
               (char-equal (char-after (+ (point) 1)) space)))))
  (interactive)
  (let ((one-space (and (not (bolp)) (eolp))))
    (kill-line ARG)
    (if (and one-space (multiple-spaces))
        (just-one-space 1))))
(global-set-key (kbd "C-k") 'kill-line-and-one-space)

(load "xscheme")

(message "Successfully loaded personal settings.")
