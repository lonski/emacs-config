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
;;Global keybindings
;;
(evil-define-key* 'normal 'global
  (kbd "C-/") 'comment-line)
(evil-define-key* 'normal 'global
  (kbd "SPC p w") '+workspace/cycle)

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
           "*  %?")
          ("w" "Work note" entry (file (lambda ()(concat org-directory "/work-notes.org")))
           "*  %?")
          ("j" "Journal" entry (file (lambda ()(concat org-directory "/journal.org")))
           "* %<%d %B %Y> %?"))))
(defun org-archive-done-todo-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'tree))

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
;;Ruby
;;

;; Use rvm for rspec
(setq rspec-use-rvm t)

;; Robe - ruby completion server
;; Activate ruby using rvm, before starting the server
(advice-add 'inf-ruby-console-auto :before #'rvm-activate-corresponding-ruby)
(add-hook 'ruby-mode-hook
    '(lambda ()
        (evil-define-key* 'normal ruby-mode-map
          (kbd "SPC v s") 'rspec-verify-single
          (kbd "SPC v a") 'rspec-verify-all)))

;; Clean cube test database
(defun cube-reset-test-db ()
  "Recreates cube test database"
  (interactive)
  (async-shell-command
   ;; command and parameters
   "RAILS_ENV=test rake db:drop || RAILS_ENV=test rake db:create && RAILS_ENV=test rake db:migrate && RAILS_ENV=test rake db:seed"
   ;; name of the error buffer
   "*Cube clean test db*"
   ;; show error buffer?
   t))

;;
;; Treemacs config
;;
(use-package! treemacs
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

;;
;; Java config
;;
(use-package lsp-mode
  :init
  (setq lsp-prefer-flymake nil)
  :demand t
  :config
  (setq lsp-enable-file-watchers nil))

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t)
  (define-key lsp-ui-mode-map
    [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map
    [remap xref-find-references] #'lsp-ui-peek-find-references)
  :after lsp-mode)

(use-package dap-mode
  :config
  (message "Dap loaded")
  (dap-mode t)
  (dap-ui-mode t))

(use-package lsp-java
  :init
  (defun sps/java-mode-config()
    (evil-define-key* 'normal java-mode-map
      (kbd "SPC c I") 'lsp-java-add-import
      (kbd "SPC v s") 'dap-java-run-test-method
      (kbd "SPC v a") 'dap-java-run-test-class)
    (lsp))
  :config
  ;; Enable dap-java (debugger)
  (require 'dap-java)
  ;; Configure Lombok
  (setq lsp-java-vmargs
      (list "-noverify"
            "-Xmx2G"
            "-XX:+UseG1GC"
            "-XX:+UseStringDeduplication"
            "-javaagent:/home/michallonski/.java_resources/lombok.jar")
      lsp-file-watch-ignored-directories
      '(".idea" "node_modules" ".git" "build" "tmp" "public")
      lsp-java-import-order '["" "java" "javax" "#"]
      ;; Don't organize imports on save
      lsp-java-save-action-organize-imports nil
      ;; Formatter profile
      lsp-java-format-settings-url "file:///home/michallonski/.java_resources/EclipseCodeStyle.xml" )
  :demand t
  :hook
  (java-mode . sps/java-mode-config)
  :after (lsp-mode dap-mode))
