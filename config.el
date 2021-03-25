;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Michal Lonski"
      user-mail-address "michal.lonski@du.co")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Make C-v / M-v scroll by half screen
(require 'golden-ratio-scroll-screen)
(global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
(global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up)

;; Start in fullscreen mode
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;;
;; Org config
;;
(setq org-directory "~/Documents/org")
(evil-define-key* 'normal 'global
  (kbd "SPC n e") 'org-babel-execute-src-block)
(add-hook! org-mode :append #'org-appear-mode)
(after! org (setq org-hide-emphasis-markers t))
;; Org templates
(after! (org-capture)
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline (lambda ()(concat org-directory "/todo.org")) "Tasks")
           "* TODO %?\n  %i\n  %a", :prepend t)
          ("n" "Note" entry (file (lambda ()(concat org-directory "/notes.org")))
           "* %?"))))

;;
;; Use rvm for rspec
;;
(setq rspec-use-rvm t)

;;
;; Configure Centaur Tabs
;;
(use-package! centaur-tabs
  :demand
  :init
  (setq centaur-tabs-enable-key-bindings t)
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-group-by-projectile-project)
  (setq centaur-tabs-style "wave"
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t
        centaur-tabs-set-modified-marker t))
(after! centaur-tabs
  (evil-define-key* 'normal 'global
    (kbd "C-x 9") 'centaur-tabs--kill-this-buffer-dont-ask))

;;
;; Robe - ruby completion server
;; Activate ruby using rvm, before starting the server
(advice-add 'inf-ruby-console-auto :before #'rvm-activate-corresponding-ruby)

;;
;; Treemacs config
;;
(use-package! treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-width 40)
    (treemacs-follow-mode t)))

(after! treemacs
  (evil-define-key* 'normal 'global
    (kbd "SPC o s") 'treemacs-select-window))

;; Ace Jump Mode
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(evil-define-key* 'normal 'global
    (kbd "SPC s c") 'ace-jump-mode
    (kbd "SPC s C") 'ace-jump-mode-pop-mark)

;;Exit insert mode by pressing j and then j quickly
;;o
;;
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)

;;
;; Disable evil-snipe (I want vim 's' behaviour)
;;
(evil-snipe-mode 0)
(evil-snipe-override-mode 0)

;;
;; Recent files buffer configuration
;;
(use-package! switch-buffer-functions
  :after recentf
  :preface
  (defun my-recentf-track-visited-file (_prev _curr)
    (and buffer-file-name
         (recentf-add-file buffer-file-name)))
  :init
  (add-hook 'switch-buffer-functions #'my-recentf-track-visited-file))

;;Search config "C-s" - to use swiper
(map! "C-s" #'+default/search-buffer)

;; Reformat buffer
(defun zz/org-reformat-buffer ()
  (interactive)
  (when (y-or-n-p "Really format current buffer? ")
    (let ((document (org-element-interpret-data (org-element-parse-buffer))))
      (erase-buffer)
      (insert document)
      (goto-char (point-min)))))
