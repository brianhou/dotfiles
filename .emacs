(when (not (eq system-type 'darwin))
  (add-to-list 'load-path "~/.emacs.d"))

;;; Eye candy

;; Adding themes (solarized)
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(defun colorize-paren ()
  "Matching parentheses customizations."
  (show-paren-mode t)
  (setq-default show-paren-delay 0)
  (set-face-foreground 'show-paren-match-face "#FFF")
  (set-face-attribute 'show-paren-match-face nil :weight 'ultra-bold))
(defun colorize-frame (f)
  (with-selected-frame f
    (when (window-system f)
      (load-theme (if (eq system-type 'darwin) 'solarized 'solarized-dark) t) (colorize-paren))))
(when (eq system-type 'darwin)
  (setq frame-background-mode 'dark)) ; for solarized-dark
(if (daemonp)
    (add-hook 'after-make-frame-functions 'colorize-frame)
    (colorize-frame (car (cadr (current-frame-configuration)))))
(when (eq system-type 'darwin)
  (enable-theme 'solarized))

;; Hide bars
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;;; end eye candy

;; package management
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(setq package-enable-at-startup nil)
(load-file "~/.emacs-packages")

;;; Minor modes
(column-number-mode t) ; View column numbers in the mode line
(global-subword-mode t) ; Jump between words intelligently
; (mouse-avoidance-mode 'exile) ; Banish mouse when mark is near
(winner-mode t) ; Revert to previous window configuration
(ido-mode t) ; Turning on ido mode!
; (if (display-graphic-p) (global-hl-line-mode t)) ; Highlight line
(global-git-gutter-mode t) ; Show git diff in gutter
(setq-default indicate-buffer-boundaries 'left indicate-empty-lines t)
; (fancy-narrow-mode) ; buggy, prettier narrowing
; (hs-minor-mode t)

;;; Customizing variables
(setq
  inhibit-startup-screen t
  inhibit-startup-buffer-menu t
  vc-follow-symlinks t
  safe-local-variable-values '((compilation-read-command))
  python-python-command "python3"
  mouse-wheel-progressive-speed nil)
(setq-default
  save-place t ; Remember previous location in file
  fill-column 80
  recenter-positions '(top middle bottom) ; For C-l and M-r
  next-screen-context-lines 2
  indent-tabs-mode nil ; No tabs
  c-basic-offset 4
  echo-keystrokes 0.3
  kill-whole-line 1)
(require 'saveplace)

(put 'upcase-region 'disabled nil) ; C-x C-u
(put 'downcase-region 'disabled nil) ; C-x C-l
(put 'narrow-to-region 'disabled nil) ; C-x n n
(put 'suspend-frame 'disabled t)

;;; Custom key bindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "<f10>") 'show-ws-toggle-show-trailing-whitespace)
(global-set-key (kbd "C-`") 'toggle-truncate-lines)
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "M-RET") 'eshell)
(windmove-default-keybindings 'meta) ; M-<arrows>

;;; Adding things to auto-mode-alist
;; Open custom bash files with sh-mode
(add-to-list 'auto-mode-alist '("\\.[[:alnum:]_]*bash_aliases\\'" . sh-mode))
;; Open .text with markdown-mode
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
;; js2-mode is cool
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; There are a lot of special Ruby files
(let ((ruby-file-types '(("\\.rake\\'" . ruby-mode)
                         ("Rakefile\\'" . ruby-mode)
                         ("\\.gemspec\\'" . ruby-mode)
                         ("\\.ru\\'" . ruby-mode)
                         ("Gemfile\\'" . ruby-mode)
                         ("Guardfile\\'" . ruby-mode))))
  (setq-default auto-mode-alist (append auto-mode-alist ruby-file-types)))

;; Lisp indentation things
(put 'setq 'lisp-indent-function 'defun)
(put 'setq-default 'lisp-indent-function 'defun)
(put 'add-hook 'lisp-indent-function 'defun)
(put 'if 'lisp-indent-function nil)

;; CS 61A things
(add-to-list 'auto-mode-alist '("\\.logic\\'" . scheme-mode))
(add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)

;;; Adding hooks
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook
  (lambda () (local-set-key (kbd "<f9>") 'eval-buffer)))
(add-hook 'java-mode-hook
  (lambda ()
    (setq compile-command "javac -g") ; Compile with javac -g
    (local-set-key (kbd "C-x C-e") 'compile)))
(add-hook 'c-mode-hook
  (lambda ()
    (setq compile-command "gcc -g") ; Compile with gcc -g
    (local-set-key (kbd "C-x C-e") 'compile)))
(add-hook 'c++-mode-hook
  (lambda ()
    (setq compile-command "g++ -std=c++11 -g") ; Compile with g++ -g
    (local-set-key (kbd "C-x C-e") 'compile)))

(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(defun TeX-word-count (&optional)
  (interactive)
  (let ((command "texcount -sum -1 ")
        (file (buffer-name)))
    (shell-command (concat command file))))
(add-hook 'LaTeX-mode-hook
  (lambda ()
    (local-set-key (kbd "C-x C-e") 'compile)
    (local-set-key (kbd "C-c C-w") 'TeX-word-count)))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
     (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)))

(define-minor-mode evil-org-mode
  "Buffer local minor mode for evil-org"
  :init-value nil
  :lighter " EvilOrg"
  :keymap (make-sparse-keymap)
  :group 'evil-org)
;; (add-hook 'org-mode-hook 'evil-org-mode)
;; (evil-define-key 'normal evil-org-mode-map
;;   "{" 'org-backward-element
;;   "}" 'org-forward-element)
(add-hook 'org-mode-hook
  (lambda ()
    (setq paragraph-start "\\|[    ]*$"
          paragraph-separate "[     ]*$")))

;; hippie expand
(setq hippie-expand-try-functions-list
  '(try-expand-line
    try-expand-dabbrev-visible try-expand-dabbrev try-expand-dabbrev-all-buffers
    try-expand-line-all-buffers try-expand-dabbrev-from-kill)
  hippie-expand-verbose t
  hippie-expand-max-buffers 3)
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;;; Ibuffer
(global-set-key [remap list-buffers] 'ibuffer)
(setq ibuffer-expert t)

(defadvice ibuffer-generate-filter-groups (after reverse-ibuffer-groups activate)
  (setq ad-return-value (nreverse ad-return-value)))

(setq ibuffer-saved-filter-groups
  '(("default"
     ("help" (or (name . "\*Help\*")
                 (name . "\*Apropos\*")
                 (name . "\*Completions\*")
                 (name . "\*Occur\*")
                 (name . "\*Messages\*")))
     ("tex" (mode . TeX-output-mode))
     ("dired" (mode . dired-mode))
     ("magit" (or (mode . magit-mode) (name . "\*magit")))
     ("todo" (or (filename . "\\.todo/.*")
                 (name . "\*todo\*")))
     ("config" (filename . "\\.emacs"))
     ("cs61a" (filename . "/coding/cs61a"))
     ("cs189" (or (filename . "/coding/cs189") (filename . "/teaching/sp16-189")))
     ("research" (filename . "/research/"))
     )))

(defun ibuffer-ido-find-file ()
  "Like `ido-find-file', but default to the directory of the buffer at point."
  (interactive
   (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                              (if (buffer-live-p buf)
                                  (with-current-buffer buf default-directory)
                                  default-directory))))
     (ido-find-file-in-dir default-directory))))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-auto-mode t) ; Auto-update buffers
    (setq ibuffer-show-empty-filter-groups nil)
    (ibuffer-switch-to-saved-filter-groups "default")
    (local-set-key (kbd "C-x C-f") 'ibuffer-ido-find-file)))

(defun toggle-frame-split ()
  "Rotate from horizontal/vertical split to the opposite."
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

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (prin1 (eval (read (current-kill 0))) (current-buffer)))
(global-set-key (kbd "C-c C-e") 'eval-and-replace) ; like C-x C-e

;;; Customize some built-in functions
(defun zap-up-to-char (arg char)
  "Kill up to, but not including ARGth occurrence of CHAR. Case is ignored if
`case-fold-search' is non-nil in the current buffer. Goes backward if ARG is
negative; error if CHAR not found. Ignores CHAR at point. Equivalent to vim's
<arg>dt<char>."
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

(defadvice yank (around yank-and-indent)
  "Indent after yanking."
  (let ((point-before (point)))
    ad-do-it
    (indent-region point-before (point))))
(ad-activate 'yank)

(defun goto-line-with-feedback ()
  "Only turn on linum mode when using goto-line"
  (interactive)
  (unwind-protect
      (progn (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun refresh-file ()
  (interactive)
  (revert-buffer t (not (buffer-modified-p)) t)
  (font-lock-fontify-buffer))
(global-set-key (kbd "<f5>") 'refresh-file)

;;; Set persistent TODO-BUFFER (todo file at ~/.todo) based off
;;; http://dorophone.blogspot.com/2011/11/how-to-make-emacs-scratch-buffer.html
;;; and http://stackoverflow.com/a/358740
(defvar TODO-FILENAME "~/.todo/todo" "to-do file location")
(defvar TODO-BUFFER "*todo*" "buffer to load into")
(defadvice kill-buffer (around bury-scratch activate)
  "Bury TODO-BUFFER instead of killing it."
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill TODO-BUFFER) (bury-buffer) ad-do-it)))
(defadvice save-buffer (around save-todo activate)
  "Write contents of *todo* to TODO-FILENAME"
  (let ((buffer-to-save (buffer-name (current-buffer))))
    (if (equal buffer-to-save TODO-BUFFER)
        (write-region nil nil TODO-FILENAME) ad-do-it)))
(defun load-todo ()
  "Load the contents of TODO-FILENAME into the TODO-BUFFER."
  (with-current-buffer (get-buffer-create TODO-BUFFER)
    (org-mode)
    (insert-file-contents TODO-FILENAME)))
(defun save-todo ()
  "Save the contents of the TODO-BUFFER into TODO-FILENAME."
  (let ((todo-buffer (get-buffer TODO-BUFFER)))
    (if (not (null todo-buffer))
        (with-current-buffer todo-buffer (write-file TODO-FILENAME)))))
(push 'save-todo kill-emacs-hook) ; Save before killing
(load-todo) ; Run load-todo function at startup

;;; Packages on packages on packages...

;; evil
(evil-mode 1)
(evilnc-default-hotkeys) ; evil-nerd-commenter

(define-key evil-ex-map "b" 'ido-switch-buffer) ; C-x b?

(define-key evil-motion-state-map "j" "gj")
(define-key evil-motion-state-map "k" "gk")
(define-key evil-motion-state-map "$" "g$")
(define-key evil-motion-state-map "^" "g^")
(define-key evil-motion-state-map "0" "g0")

(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "C-w") 'kill-region)
(define-key evil-normal-state-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-normal-state-map (kbd "C-y") 'yank)
(define-key evil-normal-state-map (kbd "=") 'er/expand-region)

(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(define-key evil-insert-state-map (kbd "C-w") 'kill-region)
(define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-y") 'yank)
(define-key evil-insert-state-map (kbd "C-d") 'delete-char)
(define-key evil-insert-state-map (kbd "C-p") 'previous-line)
(define-key evil-insert-state-map (kbd "C-n") 'next-line)

(define-key evil-visual-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-visual-state-map (kbd "C-w") 'kill-region)
(define-key evil-visual-state-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-visual-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-visual-state-map (kbd "C-y") 'yank)
(define-key evil-visual-state-map (kbd "=") 'er/expand-region)

(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
                `(ruby-mode
                  ,(rx (or "def" "class" "module" "{" "[")) ; Block start
                  ,(rx (or "}" "]" "end"))                  ; Block end
                  ,(rx (or "#" "=begin"))                   ; Comment start
                  ruby-forward-sexp nil)))
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; ace-jump-mode
(add-to-list 'load-path "~/.emacs.d/ace-jump-mode") ; use custom version
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(setq ace-jump-mode-gray-background nil)

;; expand-region.el
(global-set-key (kbd "C-=") 'er/expand-region)

;; multiple-cursors.el
(global-set-key (kbd "C-c .") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-c !") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c ?") 'mc/mark-all-in-region)
(global-set-key (kbd "C-c a") 'mc/mark-all-dwim)
(global-set-key (kbd "C-S-a") 'mc/edit-beginnings-of-lines)

;; fold-this
(add-to-list 'load-path "~/.emacs.d/fold-this.el") ; use custom version
(autoload 'fold-active-region-lines "fold-this" "Folding lines of code" t)
(define-key evil-normal-state-map (kbd "z f") 'fold-active-region-lines)
(setq fold-this-persistent-folds t)

;; yasnippet
(yas-global-mode 1)
(setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))

;; uniquify buffers
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":"
  uniquify-after-kill-buffer-p t)

;; magit
(global-set-key (kbd "<f2>") 'magit-status)
(setq magit-diff-use-overlays nil)
;; (eval-after-load 'magit
;;   '(progn
;;      (set-face-foreground 'magit-diff-add "green")
;;      (set-face-foreground 'magit-diff-del "red")
;;      (when (not window-system)
;;        (set-face-background 'magit-item-highlight "black"))))

;; undo-tree
(global-undo-tree-mode)

;; spotify!
(when (not (eq system-type 'darwin))
  ; "Not implemented for this platform" error on Mac...
  (global-set-key (kbd "<f11>") 'spotify-playpause)
  (global-set-key (kbd "<C-f11>") 'spotify-next)
  (spotify-enable-song-notifications))

;; smex
(global-set-key (kbd "<f6>") 'smex)

;; buffer-move: move a buffer around
(global-set-key (kbd "<C-S-up>") 'buf-move-up)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)
(global-set-key (kbd "<C-S-left>") 'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)

;; iedit
(require 'iedit)

;; auctex
(setq
  TeX-PDF-mode t
  TeX-save-query nil
  TeX-command-force "LaTeX"
  TeX-install-font-lock 'tex-font-setup)

;; regexp-builder
(require 're-builder)
(setq reb-re-syntax 'string)

(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))
(add-hook 'eshell-mode-hook
      '(lambda()
          (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

(when (eq system-type 'darwin)
  (setq
    mac-command-modifier 'meta
    mac-option-modifier nil))


(message "Successfully loaded personal settings.")
