;; Place your bindings here.

;; I almost always want to indent when going to the next line
(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "M-n") 'smart-symbol-go-forward)
(global-set-key (kbd "M-p") 'smart-symbol-go-backward)

(global-set-key (kbd "M-/") 'auto-complete)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "M-?") 'help-command)


;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-a") 'mc/mark-all-like-this)

;;
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(global-set-key (kbd "C-x f") 'ido-recentf-open)
(global-set-key (kbd "C-x C-r") 'rgrep)

(global-set-key (kbd "C-c C-r") 'rename-current-buffer-file)

(global-set-key (kbd "C-c C-z") 'multi-term-once)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; make C-w to cut (even in paredit-mode)
(global-set-key (kbd "C-w") 'kill-region)
(define-key paredit-mode-map (kbd "C-w") 'kill-region)
(define-key paredit-mode-map (kbd "M-s") 'paredit-splice-sexp)
(define-key paredit-mode-map (kbd "M-S") 'paredit-split-sexp)

;; yank
(global-set-key (kbd "C-y") 'yank)

(global-set-key (kbd "C-v") (lambda () (interactive) (next-line 10)))
(global-set-key (kbd "M-v") (lambda () (interactive) (previous-line 10)))

;; some multi term tweaks
;;(require 'multi-term)
(global-set-key (kbd "C-c C-j") 'term-line-mode)

(global-set-key (kbd "M-/") 'complete-symbol)

(global-set-key (kbd "C-c r") 'revert-buffer)
