;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

(package! evil-snipe :disable t)

;; Insert/update the pinned commit with `doom/bump-package-at-point'

(package! avy :pin "ba5f035be33693d1a136a5cbeedb24327f551a92")
(package! buffer-move :pin "cb517ecf8409b5fdcda472d7190c6021f0c49751")
(package! expand-region :pin "7e5bbe2763c12bae3e77fe0c49bcad05ff91dbfe")
(package! iedit :pin "27c61866b1b9b8d77629ac702e5f48e67dfe0d3b")
(package! org-ref :pin "ec449195438f815698241f0b01a5395221b3306e")
(package! smex :pin "55aaebe3d793c2c990b39a302eb26c184281c42c")

(package! jinja2-mode :pin "03e5430a7efe1d163a16beaf3c82c5fd2c2caee1")
