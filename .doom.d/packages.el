;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package:
;;
;;   1. Declare them here in a `package!' statement,
;;   2. Run 'doom sync' in the shell,
;;   3. Restart Emacs.
;;
;; Use 'C-h f package\!' to look up documentation for the `package!' macro.

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
(package! evil-snipe :disable t)

;; Insert/update the pinned commit with `doom/bump-package-at-point'
(package! buffer-move :pin "e7800b3ab1bd76ee475ef35507ec51ecd5a3f065")
(package! smex :pin "55aaebe3d793c2c990b39a302eb26c184281c42c")
(package! jinja2-mode :pin "03e5430a7efe1d163a16beaf3c82c5fd2c2caee1")
