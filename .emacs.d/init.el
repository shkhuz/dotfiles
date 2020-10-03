(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;;(package-refresh-contents)

(require 'doom-themes)
(load-theme 'doom-dark+ t)

(transient-mark-mode 0)
(set-frame-font "Source Code Pro Bold 9" nil t)

(require 'cc-mode)
(require 'compile)

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)
(setq-default tab-width 4
              c-basic-offset 4
              c-default-style "stroustrup"
              indent-tabs-mode t)

(require 'smart-tabs-mode)
(smart-tabs-mode 1)

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

(defun newline-without-break-line ()
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(defun newline-without-break-line-before ()
  (interactive)
  (move-beginning-of-line 1)
  (newline)
  (previous-line))

;; Determine the underlying operating system
(setq huz-aquamacs (featurep 'aquamacs))
(setq huz-linux (featurep 'x))
(setq huz-win32 (not (or huz-aquamacs huz-linux)))

;; (TAB = actually tab, S-TAB = code-completion)
(setq dabbrev-case-replace t)
(setq dabbrev-case-fold-search t)
(setq dabbrev-upcase-means-case-search t)
(abbrev-mode 1)

;; Backup files
(setq backup-directory-alist `(("." . "~/.trash"))) 
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(when huz-win32
  (setq huz-build-script "Makefile")) ;; change to .\build.bat if needed

(when huz-linux
  (setq huz-build-script "Makefile")) ;; change to ./build.sh if neede

;; Compilation
(defun huz-compilation-mode-hook ()
  (setq truncate-lines nil))
(add-hook 'compilation-mode-hook 'huz-compilation-mode-hook)

(setq compilation-directory-locked nil)
(setq compilation-context-lines 0)
(setq compilation-error-regexp-alist
    (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
     compilation-error-regexp-alist))

(defun find-project-directory-recursive ()
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p huz-build-script) t
      (cd "../")
      (find-project-directory-recursive)))

(defun lock-compilation-directory ()
  "The compilation process should NOT hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked t)
  (message "Compilation directory is locked."))

(defun unlock-compilation-directory ()
  "The compilation process SHOULD hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked nil)
  (message "Compilation directory is roaming."))

(defun find-project-directory ()
  "Find the project directory."
  (interactive)
  (setq find-project-from-directory default-directory)
  (switch-to-buffer-other-window "*compilation*")
  (if compilation-directory-locked (cd last-compilation-directory)
  (cd find-project-from-directory)
  (find-project-directory-recursive)
  (setq last-compilation-directory default-directory)))

(defun make-without-asking ()
  "Make the current build."
  (interactive)
  (save-buffer)
  (if (find-project-directory) (compile "make"))
  (other-window 1))

;; Keybindings
(defun smart-beginning-of-line ()
  (interactive)
  (let ((oldpos (point)))
	(back-to-indentation)
	(and (= oldpos (point))
		 (beginning-of-line))))

;; TODO highlighting
(setq todo-modes '(c-mode c++-mode emacs-lisp-mode))
(make-face 'font-lock-todo-face)
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(TODO\\)" 1 'font-lock-todo-face t))))
      todo-modes)
(modify-face 'font-lock-todo-face "White" "Dark Blue" nil t nil nil nil nil)

;; add 'null' keyword in C highlighting
(font-lock-add-keywords 'c-mode
                        '(("null" . font-lock-constant-face)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#222222" "#fc618d" "#7bd88f" "#fce566" "#5ad4e6" "#5ad4e6" "#5ad4e6" "#f7f1ff"])
 '(custom-safe-themes
   (quote
	("285efd6352377e0e3b68c71ab12c43d2b72072f64d436584f9159a58c4ff545a" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "fa3bdd59ea708164e7821574822ab82a3c51e262d419df941f26d64d015c90ee" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "7c4cfa4eb784539d6e09ecc118428cd8125d6aa3053d8e8413f31a7293d43169" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "1d50bd38eed63d8de5fcfce37c4bb2f660a02d3dff9cbfd807a309db671ff1af" "615123f602c56139c8170c153208406bf467804785007cdc11ba73d18c3a248b" "f9cae16fd084c64bf0a9de797ef9caedc9ff4d463dd0288c30a3f89ecf36ca7e" "229c5cf9c9bd4012be621d271320036c69a14758f70e60385e87880b46d60780" "58c3313b4811ed8b30239b1d04816f74d438bcb72626d68181f294b115b7220d" "51956e440cec75ba7e4cff6c79f4f8c884a50b220e78e5e05145386f5b381f7b" "dde8c620311ea241c0b490af8e6f570fdd3b941d7bc209e55cd87884eb733b0e" "f2b56244ecc6f4b952b2bcb1d7e517f1f4272876a8c873b378f5cf68e904bd59" "361f5a2bc2a7d7387b442b2570b0ef35198442b38c2812bf3c70e1e091771d1a" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "1ed5c8b7478d505a358f578c00b58b430dde379b856fbcb60ed8d345fc95594e" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "0ad7f1c71fd0289f7549f0454c9b12005eddf9b76b7ead32a24d9cb1d16cbcbd" "6231254e74298a1cf8a5fee7ca64352943de4b495e615c449e9bb27e2ccae709" "56911bd75304fdb19619c9cb4c7b0511214d93f18e566e5b954416756a20cc80" "845103fcb9b091b0958171653a4413ccfad35552bc39697d448941bcbe5a660d" "6bacece4cf10ea7dd5eae5bfc1019888f0cb62059ff905f37b33eec145a6a430" "d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086" "9b01a258b57067426cc3c8155330b0381ae0d8dd41d5345b5eddac69f40d409b" "7f791f743870983b9bb90c8285e1e0ba1bf1ea6e9c9a02c60335899ba20f3c94" "d5f8099d98174116cba9912fe2a0c3196a7cd405d12fa6b9375c55fc510988b5" "b25d0899b03ec8a9e9691cb6e8801e9bcac0eee59ef6dd743ce23ce591147480" "1526aeed166165811eefd9a6f9176061ec3d121ba39500af2048073bea80911e" "a339f231e63aab2a17740e5b3965469e8c0b85eccdfb1f9dbd58a30bdad8562b" "cb96a06ed8f47b07c014e8637bd0fd0e6c555364171504680ac41930cfe5e11e" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" "3577ee091e1d318c49889574a31175970472f6f182a9789f1a3e9e4513641d86" "a83f05e5e2f2538376ea2bfdf9e3cd8b7f7593b16299238c1134c1529503fa88" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(fci-rule-color "#4C566A")
 '(hl-todo-keyword-faces
   (quote
	(("TODO" . "#dc752f")
	 ("NEXT" . "#dc752f")
	 ("THEM" . "#2d9574")
	 ("PROG" . "#4f97d7")
	 ("OKAY" . "#4f97d7")
	 ("DONT" . "#f2241f")
	 ("FAIL" . "#f2241f")
	 ("DONE" . "#86dc2f")
	 ("NOTE" . "#b1951d")
	 ("KLUDGE" . "#b1951d")
	 ("HACK" . "#b1951d")
	 ("TEMP" . "#b1951d")
	 ("FIXME" . "#dc752f")
	 ("XXX+" . "#dc752f")
	 ("\\?\\?\\?+" . "#dc752f"))))
 '(jdee-db-active-breakpoint-face-colors (cons "#191C25" "#81A1C1"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#191C25" "#A3BE8C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#191C25" "#434C5E"))
 '(objed-cursor-color "#BF616A")
 '(package-selected-packages
   (quote
	(d-mode rust-mode go-mode go markdown-mode color-theme rainbow-blocks smart-tabs-mode sublimity smooth-scrolling smooth-scroll doom-themes spacemacs-theme ace-window ##)))
 '(pdf-view-midnight-colors (cons "#ECEFF4" "#2E3440"))
 '(rustic-ansi-faces
   ["#2E3440" "#BF616A" "#A3BE8C" "#EBCB8B" "#81A1C1" "#B48EAD" "#88C0D0" "#ECEFF4"])
 '(smooth-scrolling-mode t)
 '(vc-annotate-background "#2E3440")
 '(vc-annotate-color-map
   (list
	(cons 20 "#A3BE8C")
	(cons 40 "#bbc28b")
	(cons 60 "#d3c68b")
	(cons 80 "#EBCB8B")
	(cons 100 "#e2b482")
	(cons 120 "#d99d79")
	(cons 140 "#D08770")
	(cons 160 "#c68984")
	(cons 180 "#bd8b98")
	(cons 200 "#B48EAD")
	(cons 220 "#b77f96")
	(cons 240 "#bb7080")
	(cons 260 "#BF616A")
	(cons 280 "#a05b67")
	(cons 300 "#815664")
	(cons 320 "#625161")
	(cons 340 "#4C566A")
	(cons 360 "#4C566A")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Cursor line highlighting
(global-hl-line-mode 1)
(set-face-background 'hl-line "#222233")
(set-face-foreground 'hl-line nil)
(setq-default truncate-partial-width-windows nil)
(setq-default truncate-lines t)

(require 'generic-x)
(defvar ether-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map "\t" 'tab-to-tab-stop)
	(define-key map [S-tab] 'dabbrev-expand)
	(define-key map [backtab] 'dabbrev-expand)
	(define-key map [C-tab] 'indent-region)
	map)
  "Keymap for ether-mode")
(defun ether-mode-init ()
  (use-local-map ether-mode-map))
(add-hook 'ether-mode-hook #'ether-mode-init)

(define-generic-mode 'ether-mode
  '("//")
  '("struct" "if" "elif" "else" "for" "while" "return" "extern" "u32" "i32" "pub" "namespace" "no_instance" "alias" "option" "result" "none" "unwrap" "or" "extension" "with")
  '(("=" . 'font-lock-operator))
  '("\\.eth$")
  nil
  "A major mode for Ether Programming Language by Huzaifa Shaikh")

(font-lock-add-keywords 'ether-mode
						'(("\\(^\\|\\s-+\\|\\[\\)\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\(\\**:\\)" . font-lock-type-face)
						  ("void" . font-lock-type-face)
						  ("true" . font-lock-constant-face)
						  ("false" . font-lock-constant-face)
						  ("null" . font-lock-constant-face)
						  ("fn" . font-lock-constant-face)
						  ("some" . font-lock-constant-face)
						  ("cast" . font-lock-constant-face)))

(defun copy-to-end-of-line ()
  (interactive)
  (save-excursion
	(let ((oldpos (point)))
	  (end-of-line)
	  (message "Copied to end of line")
      (kill-ring-save oldpos (line-end-position)))))

(define-key input-decode-map [?\C-i] [C-i])
(define-key input-decode-map [?\C-m] [C-m])

(require 'd-mode)
(require 'rust-mode)
(require 'markdown-mode)

(dolist (map (list global-map c-mode-map c++-mode-map emacs-lisp-mode-map d-mode-map rust-mode-map markdown-mode-map))
	(define-key map (kbd "C-SPC") 'delete-region)
	(define-key map (kbd "<backspace>") 'backward-delete-char)
	(define-key map (kbd "C-v") 'pop-to-mark-command)
	(define-key map (kbd "C-d") 'set-mark-command)
	(define-key map (kbd "C-b") 'switch-to-buffer)
	(define-key map (kbd "C-u") 'undo)
	(define-key map (kbd "C-f") 'yank)
	(define-key map (kbd "C-`") 'save-buffer)
	(define-key map (kbd "C-q") 'kill-ring-save)
	(define-key map (kbd "C-;") 'other-window)
	(define-key map (kbd "M-p") 'previous-error)
	(define-key map (kbd "M-n") 'next-error)
	(define-key map (kbd "M-h") 'backward-char)
	(define-key map (kbd "M-j") 'next-line)
	(define-key map (kbd "M-k") 'previous-line)
	(define-key map (kbd "M-l") 'forward-char)
	(define-key map (kbd "M-q") 'switch-to-buffer)
	(define-key map (kbd "M-w") 'find-file)
	(define-key map (kbd "M-r") 'eval-buffer)
	(define-key map (kbd "M-s") 'save-buffer)
	(define-key map (kbd "C-z") 'kill-whole-line)
	(define-key map (kbd "C-n") 'delete-forward-char)
	(define-key map (kbd "M-z") 'kill-line)
	(define-key map (kbd "C-'") 'isearch-forward-symbol-at-point)
	(define-key map (kbd "C-h") 'backward-word)
	(define-key map (kbd "C-j") 'forward-paragraph)
	(define-key map (kbd "C-k") 'backward-paragraph)
	(define-key map (kbd "C-l") 'forward-word)
	(define-key map "\em" 'make-without-asking)
	(define-key map (kbd "C-a") 'smart-beginning-of-line)
	(define-key map (kbd "C-o") 'newline-without-break-line)
	(define-key map "\t" 'tab-to-tab-stop)
	(define-key map (kbd "<backtab>") 'dabbrev-expand)
	(define-key map (kbd "C-M-o") 'newline-without-break-line-before))
