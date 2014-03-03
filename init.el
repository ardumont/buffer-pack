;;; buffer-pack.el --- Buffer manipulation

;;; Commentary:

;;; Code:

(install-packs '(fold-dwim
                 multiple-cursors
                 move-text
                 auto-complete
                 git-gutter
                 projectile
                 s
                 dash
                 ace-jump-mode
                 buffer-move
                 iy-go-to-char))

(require 'multiple-cursors)
(require 'git-gutter)
(require 'auto-complete)
(require 'buffer-move)

(require 'projectile)
(projectile-global-mode)

;; Ace jump mode
(require 'ace-jump-mode)

;; go to char
(require 'iy-go-to-char)

;; (require 'dired)
;; (add-hook 'dired-mode-hook
;;           (define-key dired-mode-map (kbd "q") (lambda () (interactive) (quit-window t))))

(require 's)

(require 'etags)

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond
   ((not (> (count-windows) 1)) (message "You can't rotate a single window!"))
   (t
    (let ((i 1)
          (num-windows (count-windows)))
      (while  (< i num-windows)
        (let* ((w1 (elt (window-list) i))
               (w2 (elt (window-list) (+ (% i num-windows) 1)))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2)))
          (set-window-buffer w1  b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)
          (setq i (1+ i))))))))

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
 (lambda () ;; ~ to go straight home, // to go in /
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

(defalias 'yes-or-no-p 'y-or-n-p) ;; "y" resp. "n" instead of "yes" resp. "no".

(defvar buffer-pack-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-o") 'ace-jump-mode)
    (define-key map (kbd "C-c ;") 'iy-go-to-char)
    (define-key map (kbd "C-c ,") 'iy-go-to-char-backward)

    (define-key map (kbd "M-/") 'auto-complete)
    (define-key map (kbd "C-h") 'delete-backward-char)
    (define-key map (kbd "C-M-h") 'backward-kill-word)
    (define-key map (kbd "M-?") 'help-command)

    ;; multiple-cursors
    (define-key map (kbd "C->") 'mc/mark-next-like-this)
    (define-key map (kbd "C-<") 'mc/mark-previous-like-this)
    (define-key map (kbd "C-c C-<") 'mc/mark-all-like-this)

    (define-key map [remap goto-line] 'goto-line-with-feedback)

    (define-key map (kbd "C-x C-r") 'rgrep)

    (define-key map (kbd "C-+") 'text-scale-increase)
    (define-key map (kbd "C--") 'text-scale-decrease)

    (define-key map (kbd "C-w") 'kill-region)
    (define-key map (kbd "C-y") 'yank)

    (define-key map (kbd "C-v") (lambda () (interactive) (next-line 10)))
    (define-key map (kbd "M-v") (lambda () (interactive) (previous-line 10)))

    (define-key map (kbd "C-c r r") (lambda () (interactive) (revert-buffer nil t)))

    ;;scroll other window
    (define-key map (kbd "C-M-]") 'scroll-other-window)
    (define-key map (kbd "C-M-[") 'scroll-other-window-down)

    ;; Align your code in a pretty way.
    (define-key map (kbd "C-x \\") 'align-regexp)

    ;; Window switching.
    (define-key map (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
    (define-key map (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

    ;;window and buffer movement
    (define-key map (kbd "C-c w r") 'rotate-windows)
    (define-key map (kbd "C-c w p") 'buf-move-up)
    (define-key map (kbd "C-c w n") 'buf-move-down)
    (define-key map (kbd "C-c w b") 'buf-move-left)
    (define-key map (kbd "C-c w f") 'buf-move-right)
    (define-key map (kbd "C-c w .") 'shrink-window-horizontally)
    (define-key map (kbd "C-c w ,") 'enlarge-window-horizontally)
    (define-key map (kbd "C-c w /") (lambda () (interactive) (enlarge-window -1)))
    (define-key map (kbd "C-c w '") (lambda () (interactive) (enlarge-window 1)))

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

;; Override some default mapping to the minibuffer
;; (add-hook 'minibuffer-setup-hook (lambda ()
;;                                    (define-key minibuffer-local-map (kbd "C-h") 'backward-kill-char)
;;                                    (define-key minibuffer-local-map (kbd "C-M-h") 'backward-kill-word)))

;;; buffer-pack.el ends here
