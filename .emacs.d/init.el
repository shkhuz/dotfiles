(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(eval-when-compile
  (require 'use-package))

; Change emacs default behaviour
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(transient-mark-mode -1)
(set-frame-font "Source Code Pro 12" nil t)

(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq indent-line-function 'insert-tab)
(setq-default tab-width 4)
(setq tab-stop-list (number-sequence 4 200 4))

(setq backup-directory-alist '(("." . "~/.trash")))

(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

(setq-default
 whitespace-line-column 80
 whitespace-style       '(face lines-tail))
(global-whitespace-mode 1)

(add-hook 'prog-mode-hook #'whitespace-mode)
(setq column-number-mode t)

(setq string-rectangle-history nil)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(global-display-line-numbers-mode 1)

; Startup
;(add-to-list 'default-frame-alist '(fullscreen . maximized))
;(split-window-right)

; Key bindings
(use-package god-mode)
(use-package cc-mode)
(god-mode)

(defun clean-file ()
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))
    (replace-regexp "[[:blank:]]+$" "" nil (point-min) (point-max))))

(add-hook 'before-save-hook 'clean-file)

(defun beginning-of-line++ ()
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

(defun open-line-and-indent-same-level (func)
  (interactive)
  (let ((cur-col (current-column))
        (col (save-excursion
               (back-to-indentation)
               (current-column))))
    (if (< cur-col col)
        (setq col cur-col))
    (funcall func)
    (indent-to-column col)))

(defun newline-and-indent-same-level ()
  (interactive)
  (open-line-and-indent-same-level 'newline))

(define-key input-decode-map [?\C-m] [C-m])
(define-key input-decode-map [?\C-i] [C-i])
(define-key input-decode-map [?\C-j] [C-j])

(defun my-backward-delete-char ()
  (interactive)
  (if
    (and
      (> (point) 4)
      (eq ?  (char-before (point)))
      (eq ?  (char-before (- (point) 1)))
      (eq ?  (char-before (- (point) 2)))
      (eq ?  (char-before (- (point) 3))))
    (backward-delete-char-untabify 4)
    (backward-delete-char-untabify 1)))

(define-minor-mode huz-mode
  "My personal minor mode"
  :lighter " huz"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-a") 'beginning-of-line++)
            (define-key map (kbd "<escape>")
                            (lambda ()
                              (interactive)
                              (god-local-mode t)))
            (define-key map (kbd "TAB") 'tab-to-tab-stop)
            (define-key map (kbd "<return>") 'newline-and-indent-same-level)
            (define-key map (kbd "DEL") 'my-backward-delete-char)
            (define-key map (kbd "C-h") 'left-char)
            (define-key map (kbd "C-j") 'next-line)
            (define-key map (kbd "C-k") 'previous-line)
            (define-key map (kbd "C-l") 'right-char)
            (define-key map (kbd "C-c") 'kill-line)
            (define-key map (kbd "C-w") 'delete-char)
            (define-key map (kbd "C-o") 'right-word)
            (define-key map (kbd "C-n") 'left-word)
            (define-key map (kbd "C-d") 'set-mark-command)
            (define-key map (kbd "C-e") 'kill-region)
            (define-key map (kbd "C-r") 'delete-region)
            (define-key map (kbd "C-f") 'yank)
            (define-key map (kbd "C-;") 'end-of-line)
            (define-key map (kbd "C-v") 'other-window)
            (define-key map (kbd "C-b")
                            (lambda ()
                              (interactive)
                              (switch-to-buffer nil)))
            (define-key map (kbd "M-w") 'find-file)
            (define-key map (kbd "M-q") 'switch-to-buffer)
            (define-key map (kbd "M-s") 'save-buffer)
            (define-key map (kbd "C-x C-1") #'delete-other-windows)
            (define-key map (kbd "C-x C-2") #'split-window-below)
            (define-key map (kbd "C-x C-3") #'split-window-right)
            (define-key map (kbd "C-x C-0") #'delete-window)
            (define-key map (kbd "C-x C-o") #'other-window)
            (define-key map (kbd "C-x C-k") #'kill-buffer)
            (define-key map (kbd "C-,")
                            (lambda ()
                              (interactive)
                              (end-of-line)
                              (god-local-mode -1)
                              (newline-and-indent-same-level)))
            (define-key map (kbd "C-<")
                            (lambda ()
                              (interactive)
                              (end-of-line)
                              (god-local-mode -1)
                              (open-line-and-indent-same-level
                                (lambda ()
                                  (beginning-of-line)
                                  (newline)
                                  (previous-line)))))
            (define-key map (kbd "M-f") 'replace-rectangle)
            (define-key map (kbd "M-c") 'delete-rectangle)
            map))

(add-hook 'after-change-major-mode-hook
          (lambda()
            (electric-indent-mode -1)))

(add-hook 'text-mode-hook 'huz-mode)
(add-hook 'prog-mode-hook 'huz-mode)
(add-hook 'special-mode-hook 'huz-mode)
(add-hook 'find-file-hook 'huz-mode)

(huz-mode 1)

(define-key god-local-mode-map (kbd "m") 'forward-paragraph)
(define-key god-local-mode-map (kbd "u") 'backward-paragraph)

(define-key god-local-mode-map (kbd "i")
  (lambda ()
    (interactive)
    (god-local-mode -1)))
(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)

(defun my-god-mode-update-mode-line ()
  (cond
   (god-local-mode
    (set-face-attribute 'mode-line nil
                        :foreground "#604000"
                        :background "#fff29a")
    (set-face-attribute 'mode-line-inactive nil
                        :foreground "#3f3000"
                        :background "#fff3da"))
   (t
    (set-face-attribute 'mode-line nil
                        :foreground "#0a0a0a"
                        :background "#d7d7d7")
    (set-face-attribute 'mode-line-inactive nil
                        :foreground "#404148"
                        :background "#efefef"))))
(add-hook 'post-command-hook 'my-god-mode-update-mode-line)

(defun my-god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))
(add-hook 'post-command-hook #'my-god-mode-update-cursor-type)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(use-package god-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "ADBO" :family "Source Code Pro"))))
 '(font-lock-comment-face ((t (:foreground "#a8a8a8"))))
 '(font-lock-constant-face ((t (:foreground "#fd28ff"))))
 '(font-lock-function-name-face ((t (:weight bold))))
 '(font-lock-keyword-face ((t (:foreground "#5f5fff" :weight bold))))
 '(font-lock-preprocessor-face ((t (:foreground "#add8e6" :weight bold))))
 '(font-lock-string-face ((t (:foreground "#5fd7af"))))
 '(font-lock-type-face ((t (:foreground "#5f5fff" :weight bold)))))
