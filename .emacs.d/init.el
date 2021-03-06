(prefer-coding-system 'utf-8)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t) ;; MELPAを追加
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t) ;; MELPA-stableを追加
(package-initialize) ;; 初期化

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; list the packages you want
(setq package-list '(
                     company
                     company-quickhelp
                     projectile
                     pt
                     markdown-mode
                     glsl-mode
                     fish-mode
                     color-theme-sanityinc-tomorrow
                     recentf-ext
                     ggtags
                     google-translate
                     flycheck
                     helm
                     helm-eww
                     helm-gtags
                     helm-projectile
                     helm-git-grep
                     web-mode
                     lsp-mode
                     nodenv
                     add-node-modules-path
                     company-lsp
                     ))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(windmove-default-keybindings)
(set-face-attribute 'default nil :family "Hack" :height 100)
(tool-bar-mode 0)
(menu-bar-mode -1)
(set-scroll-bar-mode nil)
(setq make-pointer-invisible t)
(setq ring-bell-function 'ignore)
(setq make-backup-files nil)
(setq inhibit-startup-message t)
(setq ruby-insert-encoding-magic-comment nil)
;; find-fileのファイル名補完で大文字小文字を区別しない設定
(setq completion-ignore-case t)
(setq eww-search-prefix "https://www.google.co.jp/search?q=")
(setq shr-color-visible-luminance-min 70)
(setq custom-file (locate-user-emacs-file "custom.el"))
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(require 'company)
(global-company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t)
(setq company-dabbrev-downcase nil)
(setq company-transformers '(company-sort-by-backend-importance))
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)

(company-quickhelp-mode 1)
(projectile-mode 1)
(require 'semantic)
(global-semantic-stickyfunc-mode t)

(require 'google-translate)
(require 'google-translate-smooth-ui)
(setq google-translate-translation-directions-alist '(("en" . "ja") ("ja" . "en")))
(global-set-key (kbd "C-c t") 'google-translate-smooth-translate)

(require 'recentf-ext)
(setq recentf-max-saved-items 500)

(defun revert-buffer-no-confirm (&optional force-reverting)
  "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
  (interactive "P")
  ;;(message "force-reverting value is %s" force-reverting)
  (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer :ignore-auto :noconfirm)
    (error "The buffer has been modified")))
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)

(defun other-window-or-split (slide)
  (when (one-window-p)
      (split-window-horizontally))
  (other-window slide))

(global-set-key (kbd "C-<tab>") '(lambda()(interactive)(other-window-or-split 1)))

(defun other-frame-or-make(slide)
  (if (equal (next-frame) (selected-frame))
      (make-frame))
  (other-frame slide))

(if (display-graphic-p)
    (global-set-key (kbd "C-S-<tab>") '(lambda()(interactive)(other-frame-or-make 1))))

(setq-default tab-width 2 indent-tabs-mode nil js-indent-level 2)

(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key helm-find-files-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "<tab>") 'helm-execute-persistent-action)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(define-key helm-buffer-map (kbd "<tab>") 'helm-execute-persistent-action)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(define-key helm-command-map (kbd "o") 'helm-occur)
(define-key helm-command-map (kbd "e") 'helm-eww)
(define-key helm-command-map (kbd "g") 'helm-gtags-select)
(define-key helm-command-map (kbd "i") 'helm-imenu)
(define-key helm-command-map (kbd "C-i") 'helm-semantic-or-imenu)
(define-key helm-command-map (kbd "p") 'helm-do-pt)
(define-key helm-command-map (kbd "C-p") 'helm-projectile-pt)
(require 'eww)
(define-key eww-mode-map (kbd "B") 'helm-eww-bookmarks)
(define-key eww-mode-map (kbd "H") 'helm-eww-history)
(define-key eww-mode-map (kbd "S") 'helm-eww-buffers)

(add-to-list 'auto-mode-alist '("\\.wsf\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

(autoload 'glsl-mode "glsl-mode" nil t)
(add-hook 'glsl-mode-hook
          '(lambda()
             (c-set-style "stroustrup")
             (setq indent-tabs-mode nil)
             ))

(defun my-c-mode-common-init ()
  "common hook for C/C++ mode"
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (add-to-list (make-local-variable 'company-backends)
                '(company-keywords
                  company-semantic
                  company-gtags
                  ;; company-clang ;;too heavy
                  ))
  )

;; C style
(add-hook 'c-mode-hook
          '(lambda()
             (my-c-mode-common-init)
             (c-set-style "k&r")
             ))

;; C++ style
(add-hook 'c++-mode-hook
          '(lambda()
             (my-c-mode-common-init)
             (c-set-style "stroustrup")
             ))

;; css-mode
(add-hook 'css-mode-hook
	  '(lambda ()
	     (setq css-indent-offset 2)
	     ))

;; web-mode
(add-hook 'web-mode-hook
          '(lambda ()
             (nodenv-mode)
             (add-node-modules-path)
             (setq web-mode-code-indent-offset 2)
             (setq web-mode-markup-indent-offset 2)
             ))

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; writing to end for error check
(load-theme 'sanityinc-tomorrow-night t)

(set-face-background 'mode-line "#660000")

(global-hl-line-mode)
(set-face-background 'hl-line "#570000")

(set-frame-parameter (selected-frame) 'alpha 95)
(add-to-list 'default-frame-alist '(alpha . 95))
