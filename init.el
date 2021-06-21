;; init.el --- Emacs configuration

;; LOAD ADDITIONAL CONFIG FILES

(add-to-list 'load-path "~/.emacs.d/orgmode/")
(load-library "orgmode")
(load-library "hl_functions") ;; some useful personally written functions.


;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)

(add-to-list 'package-archives
       '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(elpy
    flycheck
    material-theme))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)


;; markdown mode
;; ---------------------------------

;; (add-to-list 'load-path "~/.emacs.d/markdown/")
;; (load-library "markdown-mode")

;; (autoload 'markdown-mode "markdown-mode"
;;    "Major mode for editing Markdown files" t)
;; (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Regex Customisation
;; ---------------------------------

;; for the regex stuff
(add-to-list 'load-path "C:/Users/thc0hl/AppData/Roaming/.emacs.d/regex/")

(require 'visual-regexp)
(require 'visual-regexp-steroids)
;; Add visual regex, and a different regex engine, as the emacs one is weird and old.
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)

;; Python Customisation
;; ---------------------------------

(elpy-enable)

;; Set the $WORKON_HOME variable for pyvenv
(setenv "WORKON_HOME" "C:/ProgramData/AnacondaEnvs")
(pyvenv-mode 1) ;; < what does this do?


;; (setq elpy-rpc-backend "jedi")

;; (setq elpy-rpc-python-command "c:/Users/THC0HL/AppData/Local/Continuum/anaconda2/envs/venv/pythonw.exe")

;; (setq python-shell-interpreter "c:/Users/THC0HL/AppData/Local/Continuum/anaconda2/envs/venv/python.exe")

;; (pyvenv-activate "c:/Users/THC0HL/AppData/Local/Continuum/anaconda2/envs/venv")

;; (require 'py-autopep8)
;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))


;; Org Mode Customisation
;; ----------------------------------

;; In separate file

(add-hook 'org-mode-hook 'flyspell-mode)
;; This doesn't seem to work when put into a dedicated org-mode config file.

;; flyspell is very slow in org mode, so use the lazy add-on to make it wait.
(require 'flyspell-lazy)
(flyspell-lazy-mode 1)

;; BASIC CUSTOMIZATION
;; --------------------------------------

(setq inhibit-startup-message t) ;; hide the startup message
;; load solarized color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized-master/")
;;(load-theme 'material t) ;; load material theme
(set-frame-parameter nil 'background-mode 'dark)
(load-theme 'solarized t)
(global-linum-mode t) ;; enable line numbers globally
;; fix the line numbers becoming too big for the frame
(set-face-attribute 'linum nil :height 100)
(global-visual-line-mode 1) ;;soft line wrapping

;; Get rid of the bell noise
(setq ring-bell-function 'ignore)

;; overwrite selected text
(delete-selection-mode t)

;; Ido (better suggestions) customisation
;; ------------------------
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))


;; HTML export customisation
;; ------------------------

;; This should be possible via Melpa usually, but since it's blocked we add it manually
;; this colours code snippets correctly when exporting to HTML
(add-to-list 'load-path "~/.emacs.d/htmlize/")

;; Dired customisation
;; ----------------------

(with-eval-after-load 'dired
  (require 'dired-x))
;; (setq-default dired-omit-files-p t) ; Buffer-local variable
;;(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
;; (setq-default dired-hide-details-mode t)

(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

;; The following were the options that worked with v26.1, but changed above to work in 27.1
;;(require 'dired-x)
;; (add-hook 'dired-mode-hook
;; 	  (lambda ()
;; 	    (dired-hide-details-mode)))

;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("c:/Users/thc0hl/Documents/org/general/BSIMZ_organisation.org" "c:/Users/thc0hl/Documents/org/general/development.org" "c:/Users/thc0hl/Documents/org/digitisation/epassport_planning.org" "c:/Users/thc0hl/Documents/org/digitisation/epassport_incidents.org" "c:/Users/thc0hl/Documents/org/digitisation/epassport_data.org" "c:/Users/thc0hl/Documents/org/digitisation/digitisation_general.org" "c:/Users/thc0hl/Documents/org/digitisation/epassport_general.org" "c:/Users/thc0hl/Documents/org/digitisation/map_live_tracking.org" "c:/Users/thc0hl/Documents/org/general/to_do_v01.org")))
 '(org-export-with-sub-superscripts (quote {}))
 '(org-startup-truncated nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(variable-pitch ((t (:foundry "outline" :family "Georgia")))))
(put 'dired-find-alternate-file 'disabled nil)
