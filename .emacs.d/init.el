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
;; find-fileのファイル名補完で大文字小文字を区別しない設定
(setq completion-ignore-case t)
(setq eww-search-prefix "https://www.google.co.jp/search?q=")
(setq shr-color-visible-luminance-min 70)
(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'company)
(global-company-mode) ; 全バッファで有効にする 
(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 2) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
(setq completion-ignore-case t)
(setq company-dabbrev-downcase nil)

(company-quickhelp-mode 1)
(projectile-mode 1)

(which-function-mode t)
(setq mode-line-format (delete (assoc 'which-func-mode
                                      mode-line-format) mode-line-format)
      which-func-header-line-format '(which-func-mode ("" which-func-format)))
(defadvice which-func-ff-hook (after header-line activate)
  (when which-func-mode
    (setq mode-line-format (delete (assoc 'which-func-mode
                                          mode-line-format) mode-line-format)
          header-line-format which-func-header-line-format)))
(eval-after-load "which-func"
  '(setq which-func-modes '(java-mode c-mode c++-mode)))

(require 'recentf-ext)
(setq recentf-max-saved-items 100)
(global-set-key (kbd "C-x o") 'recentf-open-files)

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

(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (if (>= (window-body-width) 270)
        (split-window-horizontally-n 3)
      (split-window-horizontally)))
  (other-window 1))
(global-set-key (kbd "<tab>") 'other-window-or-split)

(defun other-frame-or-make()
  (interactive)
  (if (equal (next-frame) (selected-frame))
      (make-frame))
  (other-frame 1))
(if (display-graphic-p)
    (global-set-key (kbd "C-<tab>") 'other-frame-or-make))

(setq-default tab-width 2 indent-tabs-mode nil)

(autoload 'glsl-mode "glsl-mode" nil t)
(add-hook 'glsl-mode-hook
          '(lambda()
             (c-set-style "stroustrup")
             (setq indent-tabs-mode nil)
             ))

;; C++ style
(add-hook 'c++-mode-hook
          '(lambda()
             (c-set-style "stroustrup")
             (setq indent-tabs-mode nil)
             ))

;; writing to end for error check
(load-theme 'sanityinc-tomorrow-night t)
