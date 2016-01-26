;;; jhernandez-emacs-config.el --- 24/01/2016
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;
;;; Package loading ;;;
;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/")
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(4clojure ac-anaconda ac-cider ac-haskell-process ac-ispell
                      ac-math ace-jump-buffer ace-jump-mode ace-window ag
                      anaconda-mode android-mode anzu async atom-dark-theme
                      atom-one-dark-theme auctex auto-capitalize auto-complete
                      auto-indent-mode avy beacon browse-kill-ring cider
                      clj-refactor clojure-mode clojure-mode-extra-font-locking
                      clojure-snippets company cuda-mode dash diff-hl diminish
                      discover-my-major easy-kill edn elisp-slime-nav epl
                      exec-path-from-shell expand-region f find-file-in-project
                      flx flx-ido flycheck-clojure flycheck-package gh gist
                      git-timemachine git-commit gitconfig-mode gitignore-mode
                      god-mode grizzl guru-mode haskell-mode helm helm-core
                      helm-projectile hydra idle-highlight-mode
                      ido-completing-read+ ido-ubiquitous inf-ruby inflections
                      js2-mode json-mode json-reformat json-rpc json-snatcher
                      latex-pretty-symbols let-alist logito magit magit-popup
                      makey markdown-mode math-symbol-lists move-text
                      multiple-cursors nlinum operate-on-number ov paredit
                      pcache peg pkg-info popup pretty-mode projectile pythonic
                      queue rainbow-delimiters rainbow-mode request
                      rich-minority ruby-tools rust-mode s seq slamhound
                      smart-mode-line smartparens smartrep smex smooth-scroll
                      spinner swiper undo-tree vkill volatile-highlights
                      web-mode with-editor yari yasnippet zenburn-theme
                      zop-to-char))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;;;;;;;;;;;;
;;; Clojure ;;;
;;;;;;;;;;;;;;;
(require 'clojure-mode)

(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\.cljx$" . clojure-mode))

;; clj-refactor: Use C-c C-m. See https://github.com/magnars/clj-refactor.el
(require 'clj-refactor)

(defun clj-refactor-hooks ()
  "Setup clj-refactor."
  (clj-refactor-mode t)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

;; Cider (Clojure REPL) config
(require 'cider)

(defvar cider-repl-popup-stacktraces)
(setq nrepl-hide-special-buffers t)
(setq cider-show-error-buffer t)
(setq cider-repl-popup-stacktraces t)
(setq cider-repl-pop-to-buffer-on-connect t)
(setq cider-repl-history-file "~/.emacs.d/cider-history")
(setq cider-repl-wrap-history t)
(setq cider-repl-use-clojure-font-lock t)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'auto-complete-mode)
(add-hook 'cider-repl-mode-hook 'auto-indent-mode)

;; syntax highlighting in REPL
(add-hook 'cider-repl-mode-hook
          (lambda ()
            (font-lock-mode nil)
            (clojure-font-lock-setup)
            (font-lock-mode t)))

;; sane indentation in Clojure mode
(setq clojure-indent-style ':always-indent)

(add-to-list 'same-window-buffer-names "*cider*")

;; auto complete
(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

(defun set-auto-complete-as-completion-at-point-function ()
  "Tab completion in CIDER buffers."
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'auto-indent-mode)
(add-hook 'clojure-mode-hook 'clj-refactor-hooks)
(add-hook 'clojure-mode-hook 'auto-complete-mode)

;;;;;;;;;;;;;
;;; LaTeX ;;;
;;;;;;;;;;;;;
;;(require 'latex-pretty-symbols)

;;(setq TeX-auto-save t)
;;(setq TeX-parse-self t)
;;(setq TeX-save-query nil)
;;(setq TeX-PDF-mode t)

;;;;;;;;;;;;;
;;; Modes ;;;
;;;;;;;;;;;;;
(require 'pretty-mode)
(global-pretty-mode t)

(require 'auto-complete-config)
(ac-config-default)

;; Enable flycheck for all files
(add-hook 'after-init-hook #'global-flycheck-mode)

;;;;;;;;;;;;;;
;;; Editor ;;;
;;;;;;;;;;;;;;
(require 'smooth-scroll)
(smooth-scroll-mode t)

(add-to-list 'default-frame-alist '(width . 130))

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
          '(lambda() (set-fill-column 80)))

(global-nlinum-mode t)
(setq column-number-mode t)

(add-hook 'nlinum-mode-hook
          (lambda ()
            (setq nlinum-width
                  (length (number-to-string
                           (count-lines (point-min) (point-max)))))))

;; dead keys
(define-key key-translation-map [dead-grave] "`")
(define-key key-translation-map [dead-circumflex] "^")
(define-key key-translation-map [dead-tilde] "~")

(custom-set-variables
  '(blink-cursor-mode nil)
  '(fringe-mode (quote (nil . 0)) nil (fringe))
  '(show-paren-mode t)
  '(size-indication-mode t)
  '(tool-bar-mode nil)
  '(transient-mark-mode t))

;; IDEA integration
(set-default 'server-socket-dir "~/.emacs.d/server")
(if (functionp 'window-system)
    (when (and (window-system)
               (>= emacs-major-version 24))
      (server-start)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Themes and Fonts ;;;
;;;;;;;;;;;;;;;;;;;;;;;;
(disable-theme 'zenburn)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'atom-one-dark)

(custom-set-faces
  '(default ((t (:family "Source Code Pro" :foundry "unknown"
                         :slant normal :weight normal :height 140 :width normal)))))

(provide 'jd-emacs-config)
;;; jd-emacs-config ends here
