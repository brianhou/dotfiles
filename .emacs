;; Adding ~/.emacs.d to load path
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/multiple-cursors.el")

;; Adding themes (solarized)
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(load-theme 'solarized-dark t)

;; Customize variables
(setq initial-scratch-message ";; Greetings, Mr. Hou. We've been expecting you."
      inhibit-startup-screen t
      inhibit-startup-buffer-menu t
      recenter-positions '(top middle bottom))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Custom key bindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "<f12>") 'count-lines-region)

;; Minor modes
(column-number-mode t) ;; View column numbers in the mode line
(display-time-mode t) ;; Display time
(global-linum-mode t) ;; Show line numbers
(global-subword-mode t) ;; Jumping between words intelligently
(mouse-avoidance-mode 'exile) ;; Banish mouse when mark is near
(tool-bar-mode nil) ;; Turning off stupid toolbar

;; Turning on ido mode!
(require 'ido)
(ido-mode t)

;; Show matching parentheses
(show-paren-mode t)
(setq show-paren-delay 0) ;; No delay for highlighting parentheses
(require 'paren)
(set-face-foreground 'show-paren-match-face "#FFF")
(set-face-attribute 'show-paren-match-face nil :weight 'ultra-bold)

;; Make custom bash files show up in sh-mode
(let ((custom-bash-files '((".bash_aliases" . sh-mode)
                           (".gitprompt" . sh-mode)
                           (".private_bash_aliases" . sh-mode))))
  (setq auto-mode-alist (append auto-mode-alist custom-bash-files)))

;; Associate the .text file format with Markdown, turn on visual-line
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))

;; Copying things from the CS61B .emacs file (mostly for Java coding)
(add-hook 'java-mode-hook
 (lambda ()
  (setq compile-command "javac -g") ;; Compile with javac -g
  (local-set-key (kbd "C-x C-e") 'compile)))
(setq next-screen-context-lines 5 ;; Leave 5 lines of context when scrolling
      indent-tabs-mode nil) ;; Re-tab with spaces, not tabs

;; Copying things from the CS61C .emacs file
(add-hook 'c-mode-hook
 (lambda ()
  (setq compile-command "gcc -o -g") ;; Compile with gcc -o -g
  (local-set-key (kbd "C-x C-e") 'compile)))

;; Set persistent todo file at ~/.todo based off
;; http://dorophone.blogspot.com/2011/11/how-to-make-emacs-scratch-buffer.html
;; and http://stackoverflow.com/a/358740
(setq initial-major-mode 'markdown-mode) ;; set *scratch* mode to markdown
(defvar TODO-FILENAME "~/.todo" "to-do file location")
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  "Bury *scratch* buffer instead of killing it."
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))
(defun save-todo ()
  "Save the contents of the *scratch* buffer into TODO-FILENAME."
  (with-current-buffer (get-buffer "*scratch*")
    (write-region (point-min) (point-max) TODO-FILENAME)))
(defun load-todo ()
  "Load the contents of TODO-FILENAME into the *scratch* buffer."
  (with-current-buffer (get-buffer "*scratch*")
    (delete-region (point-min) (point-max))
    (insert-file-contents TODO-FILENAME)))
(push 'save-todo kill-emacs-hook) ;; Save before killing
(load-todo) ;; Run load-todo function at startup

;; Setting up multiple-cursors-mode
(require 'multiple-cursors)
(global-set-key (kbd "C-S-a") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-c <right>") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c <left>") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c !") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c a") 'mc/mark-all-in-region)

;; Adding timestamps to Messages buffer
(defadvice message (before test-symbol activate)
  (if (not (string-equal (ad-get-arg 0) "%s%s"))
    (let ((deactivate-mark nil))
      (save-excursion
        (set-buffer "*Messages*")
        (goto-char (point-max))
        (if (not (bolp)) (newline))
        (insert (format-time-string "[%T] " (current-time)))))))
