;;; emacs-config.el --- jd's Emacs configuration
;;
;; Copyright (c) 2016 Jorge Hernandez
;;
;; Author: Jorge Hernandez <jd.hernandez@gmail.com>
;; URL: https://github.com/jd-hernandez/dotfiles

;; This file is not part of GNU Emacs

;;; Commentary:

;; Emacs configuration using Prelude

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;;; Package loading ;;;
(require 'package)
(package-initialize)

(defvar packages-used '(atom-one-dark-theme auto-indent-mode clj-refactor
                        cljr-helm clojure-mode-extra-font-locking
                        latex-pretty-symbols markdown-mode math-symbol-lists
                        nlinum paredit paredit-everywhere pretty-mode rust-mode
                        smooth-scroll))

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p packages-used)
  (when (not (package-installed-p p))
    (package-install p)))

;;; User ;;;
(setq user-full-name "Jorge Hernandez"
      user-mail-address "jd.hernandez@gmail.com")

;;; Editor ;;;
(disable-theme 'zenburn)
(load-theme 'atom-one-dark)

(custom-set-faces
 '(default ((t (:family "Source Code Pro" :foundry "unknown" :slant normal
                        :weight normal :height 140 :width normal)))))

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

;;; Modes ;;;
(require 'company)
(global-company-mode)

(require 'pretty-mode)
(global-pretty-mode t)

(require 'smooth-scroll)
(smooth-scroll-mode t)
(setq smooth-scroll/vscroll-step-size 1)
(setq smooth-scroll/hscroll-step-size 1)

;;; Clojure ;;;
(require 'clojure-mode)
(require 'clojure-mode-extra-font-locking)
(require 'clj-refactor)

(setq clojure-indent-style ':always-indent)

(add-hook 'clojure-mode-hook #'auto-indent-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'subword-mode)

(defun clj-refactor-hook ()
  "Setup clj-refactor."
  (clj-refactor-mode t)
  (yas-minor-mode 1) ;; for adding require/use/import statements
  (cljr-add-keybindings-with-prefix "C-c C-n"))

(add-hook 'clojure-mode-hook #'clj-refactor-hook)

;;; CIDER ;;;
(require 'cider)

(setq cider-test-show-report-on-success t)
(setq cider-font-lock-dynamically '(macro core function var))
(setq cider-overlays-use-font-lock t)

(add-hook 'cider-repl--mode-hook #'auto-indent-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook #'subword-mode)

;;; Elixir ;;;
(require 'elixir-mode)
(require 'alchemist)

;;; IDEA integration ;;;
(set-default 'server-socket-dir "~/.emacs.d/server")
(if (functionp 'window-system)
    (when (and (window-system)
               (>= emacs-major-version 24))
      (server-start)))

 (provide 'emacs-config)
;;; emacs-config ends here
