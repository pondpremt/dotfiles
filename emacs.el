;; Melpa
(require 'package)

(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
			 ("melpa" . "http://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")))
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;;; powerline
(require 'powerline)
(powerline-center-evil-theme)
(display-time-mode 1)


;;; Latex
;; word count
(defun texcount ()
  (interactive)
  (shell-command (concat "texcount \"" buffer-file-name "\" -inc")))

;; flyspell
(add-hook 'LaTeX-mode-hook '(flyspell-mode t))
;;; Evil
(require 'evil)
(evil-mode 1)

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

(define-key evil-normal-state-map (kbd "C-y") 'previous-buffer)
(define-key evil-normal-state-map (kbd "C-o") 'next-buffer)
(define-key evil-normal-state-map (kbd "C-i") 'ido-switch-buffer)
;; Use emacs key binding in insert mode
(setq evil-insert-state-map (make-sparse-keymap))
(define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)

(global-set-key (kbd "C-q") 'suspend-emacs)

;;; Color
;; (load-theme 'github t)
(load-theme 'gruvbox t)

;;; AUCTeX
(load "auctex.el" nil t t)
(require 'tex-mik)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(setq TeX-PDF-mode t)

;;; Rainbow Delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "color-242" :weight normal))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "color-208"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "color-214"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "color-40"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "color-45"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "color-135"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "color-162"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "color-212"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "color-226")))))

;;; Line number
(global-linum-mode t)
(setq linum-format "%d ")

;;; Move between panes
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "M-p") 'ace-window)

;;; IDO
(require 'ido)
(ido-mode t)

;;; Jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;;; Flycheck
(global-flycheck-mode)

;;; Webmode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jnj\\'" . web-mode))
(setq web-mode-engines-alist
      '(("jinja"    . "\\.jnj\\'")))

;;; NeoTree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;;; Disable the splash scren (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;;; Enable transient mark mode
(transient-mark-mode 1)

;;;;org-mode configuration
;; Enable org-mode
(require 'org)
(require 'ox-md)
;; Make org-mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;;; AUtofill
(auto-fill-mode -1)

;;; Commenting
(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))))
(global-set-key (kbd "C-c C-SPC") 'comment-or-uncomment-line-or-region)

;;; Enable mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (haskell-mode web-mode rainbow-delimiters powerline neotree markdown-mode jedi gruvbox-theme github-theme flycheck evil auctex ace-window))))

(if (file-exists-p "~/dotfiles/localemacs.el")
    (load-file "~/dotfiles/localemacs.el"))

(provide 'emacs)
;;; emacs.el ends here
