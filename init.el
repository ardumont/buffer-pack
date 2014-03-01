;;; buffer-pack.el --- Buffer manipulation

;;; Commentary:

;;; Code:

(install-packs '(exec-path-from-shell
                 fold-dwim
                 multiple-cursors
                 move-text
                 auto-complete
                 git-gutter
                 projectile
                 s
                 dash
                 smartscan
                 ace-jump-mode))

(require 'multiple-cursors)
(require 'git-gutter)
(require 'auto-complete)

(require 'projectile)
(projectile-global-mode)

;; Ace jump mode
(require 'ace-jump-mode)
(global-set-key (kbd "C-o") 'ace-jump-mode)

;; setup the path
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; some text/font/color tweaks

(setq-default fill-column 120)
(set-face-background 'default "black")

(set-language-environment "UTF-8")
(blink-cursor-mode 1)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(require 'shell)
(require 's)
(require 'dash)

(defun buffer-pack/hostname! () "Return the hostname of the current computer." (-> "hostname" shell-command-to-string s-trim))

;; Depending on the hostname, will set a font or another
(let* ((hostname  (buffer-pack/hostname!))
       (font-size (if (string= hostname "dagobah") 140 100)))
  (set-face-attribute 'default nil :height font-size))

(require 'etags)
(require 'smartscan)
(global-smartscan-mode)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (let ((git-gutter-activated-p git-gutter-mode))
    (unwind-protect
        (progn
          (if git-gutter-activated-p (git-gutter-mode 0))
          (linum-mode 1)
          (goto-line (read-number "Goto line: ")))
      (progn
        (linum-mode -1)
        (if git-gutter-activated-p (git-gutter-mode 1))))))

;; Auto refresh buffers (not active by default)
;;(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(add-hook 'ido-setup-hook
 (lambda ()
   ;; Go straight home
   (define-key ido-file-completion-map (kbd "~") (lambda ()
                                                   (interactive)
                                                   (if (looking-back "/")
                                                       (insert "~/")
                                                       (call-interactively 'self-insert-command))))))

(setq
  inhibit-splash-screen t ;; Do not show a splash screen.
   echo-keystrokes 0.1    ;; Show incomplete commands while typing them.
   visible-bell t         ;; Flash the screen on errors.
   column-number-mode t)  ;; column number in the modeline

;; "y" resp. "n" instead of "yes" resp. "no".
(defalias 'yes-or-no-p 'y-or-n-p)

(defvar buffer-pack-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "M-n") 'smart-symbol-go-forward)
    ;; (define-key map (kbd "M-p") 'smart-symbol-go-backward)

    (define-key map (kbd "M-/") 'auto-complete)
    (define-key map (kbd "C-M-h") 'backward-kill-word)
    (define-key map (kbd "M-?") 'help-command)

    ;; multiple-cursors
    (define-key map (kbd "C->") 'mc/mark-next-like-this)
    (define-key map (kbd "C-<") 'mc/mark-previous-like-this)
    (define-key map (kbd "C-c C-<") 'mc/mark-all-like-this)

    (define-key map [remap goto-line] 'goto-line-with-feedback)

    (define-key map (kbd "C-x C-r") 'rgrep)

    (define-key global-map (kbd "C-+") 'text-scale-increase)
    (define-key global-map (kbd "C--") 'text-scale-decrease)

    (define-key map (kbd "C-w") 'kill-region)
    (define-key map (kbd "C-y") 'yank)

    (define-key map (kbd "C-v") (lambda () (interactive) (next-line 10)))
    (define-key map (kbd "M-v") (lambda () (interactive) (previous-line 10)))

    (define-key map (kbd "C-c r r") (lambda () (interactive) (revert-buffer nil t)))

    map)
  "Keymap for Buffer-pack mode.")

(define-minor-mode buffer-pack-mode
  "Minor mode to consolidate Emacs' buffer-pack extensions.

\\{buffer-pack-mode-map}"
  :lighter " BP"
  :keymap buffer-pack-mode-map)

(define-globalized-minor-mode global-buffer-pack-mode buffer-pack-mode buffer-pack-on)

(defun buffer-pack-on ()
  "Turn on `buffer-pack-mode'."
  (buffer-pack-mode +1))

(global-buffer-pack-mode)

;;make ^h delete rather than help
(global-set-key (kbd "C-h") 'delete-backward-char)

;;redefine help shortcut
(global-set-key (kbd "M-?") 'help-command)
(define-key org-mode-map (kbd "M-?") 'help-command)

;;scroll other window
(global-set-key (kbd "C-M-]") 'scroll-other-window)
(global-set-key (kbd "C-M-[") 'scroll-other-window-down)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Window switching.
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

;; Override some default mapping to the minibuffer
;; (add-hook 'minibuffer-setup-hook (lambda ()
;;                                    (define-key minibuffer-local-map (kbd "C-h") 'backward-kill-char)
;;                                    (define-key minibuffer-local-map (kbd "C-M-h") 'backward-kill-word)))

;;diff shortcuts
;; (global-set-key (kbd "C-c d f") 'diff-buffer-with-file)

;;window and buffer movement
(global-set-key (kbd "C-c w s") 'swap-windows)
(global-set-key (kbd "C-c w r") 'rotate-windows)
(global-set-key (kbd "C-c w p") 'buf-move-up)
(global-set-key (kbd "C-c w n") 'buf-move-down)
(global-set-key (kbd "C-c w b") 'buf-move-left)
(global-set-key (kbd "C-c w f") 'buf-move-right)
(global-set-key (kbd "C-c w .") 'shrink-window-horizontally)
(global-set-key (kbd "C-c w ,") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c w /") (lambda () (interactive) (enlarge-window -1)))
(global-set-key (kbd "C-c w '") (lambda () (interactive) (enlarge-window 1)))
;;; buffer-pack.el ends here
