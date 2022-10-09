(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(eval-when-compile
  (require 'use-package))

(use-package god-mode)
(god-mode)

(defun beginning-of-line++ ()
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

(global-set-key (kbd "C-a") 'beginning-of-line++)
(global-set-key (kbd "<escape>") #'god-mode-all)
(global-set-key (kbd "C-h") 'left-char)
(global-set-key (kbd "C-j") 'next-line)
(global-set-key (kbd "C-k") 'previous-line)
(global-set-key (kbd "C-l") 'right-char)
(global-set-key (kbd "C-c") 'kill-line)
(global-set-key (kbd "C-o") 'right-word)
(global-set-key (kbd "C-n") 'left-word)
(global-set-key (kbd "C-d") 'set-mark-command)
(global-set-key (kbd "C-e") 'kill-region)
(global-set-key (kbd "C-f") 'yank)

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
