;; Basic init file that loads most settings from a org file
;; where settings can be more easily organised.

(require 'package)

(setq package-enable-at-startup nil)
;; Let's start with only stable melpa packages, and go from there.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives '("ublt" . "https://elpa.ubolonton.org/packages/"))
;;(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; Solves issue on emacs 26?
;; from https://francopasut.medium.com/emacs-melpa-and-the-failed-to-download-gnu-archive-error-b834bbe4491e
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")


(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'org)
(require 'ob-tangle)
(org-babel-load-file "~/.emacs.d/readme.org")
(put 'dired-find-alternate-file 'disabled nil)
