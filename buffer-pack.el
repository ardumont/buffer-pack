;;; buffer-pack.el --- Buffer manipulation

;;; Commentary:

;;; Code:

(use-package nix-mode)
(use-package move-text)
(use-package dockerfile-mode)

(use-package projectile
  :config
  (custom-set-variables '(projectile-completion-system 'helm))
  (projectile-global-mode))

(use-package dash-functional)

(use-package tramp)

(defun buffer-pack/tramp-protocols ()
  "Filter the protocol methods from the tramp methods."
  (mapcar #'car tramp-methods))

(defun buffer-pack/tramp-method-for-protocol (protocol)
  "Ddtail PROTOCOL method for tramp."
  (-filter (-compose (-partial #'string= protocol) #'car) tramp-methods))

(use-package markdown-mode
  :config (add-hook 'markdown-mode-hook
                    (lambda ()
                      (require 'whitespace)
                      (whitespace-turn-on)
                      (custom-set-variables '(whitespace-line-column 80)))))

(use-package switch-window
  :config (custom-set-variables
           '(switch-window-shortcut-style 'qwerty)))

(use-package iedit)
(use-package markdown-toc)
(use-package multiple-cursors)
(use-package git-gutter)
(use-package buffer-move)

(use-package company
  :config (progn
            (add-hook 'after-init-hook 'global-company-mode)
            ;; Extend the default company mode mapping to the one
            (add-hook 'company-mode-hook (lambda ()
                                           (interactive)
                                           (define-key company-active-map (kbd "C-h") 'delete-backward-char)
                                           (define-key company-active-map (kbd "M-?") 'company-show-doc-buffer)
                                           (define-key company-active-map (kbd "C-n") 'company-select-next)
                                           (define-key company-active-map (kbd "C-p") 'company-select-previous)
                                           (define-key company-active-map (kbd "M-/") 'company-complete)))))

(use-package markdown-mode
  :config (add-to-list 'auto-mode-alist '("\\.txt$" . markdown-mode)))

(use-package ace-jump-mode)
(use-package iy-go-to-char)
;; use popwin to master the popup buffer and C-g to stop them
(use-package popwin
  :config (popwin-mode 1))

(use-package s)
(use-package etags)

(use-package whitespace
  :config
  ;; increase this if not happy about 80 columns
  (custom-set-variables '(whitespace-line-column 80)))

(defun rotate-windows ()
  "Rotate your windows."
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

(defun buffer-pack/--goto-line (n)
  "Internal 'goto-line' to go the line number N."
  (goto-char (point-min))
  (forward-line (1- n)))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (let ((git-gutter-activated-p git-gutter-mode))
    (unwind-protect
        (progn
          (when git-gutter-activated-p (git-gutter-mode 0))
          (linum-mode 1)
          (buffer-pack/--goto-line (read-number "Goto line: ")))
      (progn
        (linum-mode -1)
        (when git-gutter-activated-p (git-gutter-mode 1))))))

(add-hook 'ido-setup-hook
          (lambda () ;; ~ to go straight home, // to go in /
            (define-key ido-file-completion-map (kbd "~") (lambda ()
                                                            (interactive)
                                                            (if (looking-back "/")
                                                                (insert "~/")
                                                              (call-interactively 'self-insert-command))))))

;; activate clipboard
(custom-set-variables '(x-select-enable-clipboard t)
                      ;; Auto refresh buffers (not active by default)
                      ;;(global-auto-revert-mode 1)
                      ;; Also auto refresh dired, but be quiet about it
                      '(global-auto-revert-non-file-buffers t)
                      '(auto-revert-verbose nil)
                      '(inhibit-splash-screen t);; Do not show a splash screen.
                      '(echo-keystrokes 0.1)    ;; Show incomplete commands while typing them.
                      '(visible-bell t)         ;; Flash the screen on errors.
                      '(column-number-mode t)   ;; column number in the modeline
                      )

(defalias 'yes-or-no-p 'y-or-n-p) ;; "y" resp. "n" instead of "yes" resp. "no".

;; -----------------------------

(use-package dash
  :config (dash-enable-font-lock))

(use-package ht)

(defvar BUFFER-PACK/LAST-BUFFER (ht-create)
  "Last buffer visited when switching to term.")

(defconst BUFFER-PACK/TERM-BUFFER "buffer-pack-term"
  "The term buffer's name in which the term buffer is running.")

(defun buffer-pack/term-name (buffer-name)
  "Compute the BUFFER-NAME's name."
  (format "*%s*" buffer-name))

(defun buffer-pack/switch-to-process (buffer-name fn)
  "Switch to process corresponding to buffer BUFFER-NAME.
If BUFFER-NAME does not exist, then spawn the process with FN and switch to it."
  (-if-let (buffer-process- (get-buffer-process (buffer-pack/term-name buffer-name)))
      (let ((buffer (process-buffer buffer-process-)))
        (if (buffer-live-p buffer)
            (pop-to-buffer buffer)
          (funcall fn)))
    (funcall fn)))

(defun buffer-pack/switch-to-term! ()
  "Select the term buffer, when possible in an existing window.
The buffer chosen is based on the file open in the current buffer."
  (interactive)
  (let ((current-buf (current-buffer)) ;; current-buffer-name from which we switch to
        (term-buffer (buffer-pack/switch-to-process BUFFER-PACK/TERM-BUFFER (lambda () (ansi-term "zsh" buffer-name)))))
    (ht-set BUFFER-PACK/LAST-BUFFER term-buffer current-buf)))

(defun buffer-pack/switch-to-last-buffer! ()
  "Switch to the last buffer from whence we come to term."
  (interactive)
  (let ((term-buffer (current-buffer)));; this is the term buffer
    (message "current-buffer '%s' " (buffer-name term-buffer))
    (-when-let (last-buffer (ht-get BUFFER-PACK/LAST-BUFFER term-buffer))
      (message "Trying to switch from '%s' to '%s'" (buffer-name term-buffer) last-buffer)
      (when (buffer-live-p last-buffer)
        (pop-to-buffer last-buffer)))))

(defun buffer-pack/switch-to-term-or-get-back-to-buffer! ()
  "If on terminal switch to last buffer from whence we came.
Otherwise, we go inside a terminal."
  (interactive)
  (funcall (if (string= (buffer-pack/term-name BUFFER-PACK/TERM-BUFFER) (buffer-name (current-buffer)));; on buffer
               'buffer-pack/switch-to-last-buffer!
             'buffer-pack/switch-to-term!)))

;; -----------------------------

(defun buffer-pack/backward-upcase-word! ()
  "Backward upper case word."
  (interactive)
  (upcase-word -1))

(defun buffer-pack/call-fn-number-at-point (update-fn)
  "Call UPDATE-FN on number at point."
  (let ((number (number-at-point))
        (point (point)))
    (when number
      (forward-word)
      (search-backward (number-to-string number))
      (replace-match (number-to-string (funcall update-fn number)))
      (goto-char point))))

(defun buffer-pack/increment-number-at-point ()
  "Increment number at point."
  (interactive)
  (buffer-pack/call-fn-number-at-point #'1+))

(defun buffer-pack/decrement-number-at-point ()
  "Decrement number at point."
  (interactive)
  (buffer-pack/call-fn-number-at-point #'1-))

(defvar buffer-pack-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c j") 'ace-jump-mode)
    (define-key map (kbd "C-c g f") 'iy-go-to-char)
    (define-key map (kbd "C-c g b") 'iy-go-to-char-backward)

    (define-key map (kbd "M-/") 'company-complete)
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

    (define-key map (kbd "C-v") (lambda () (interactive) (forward-line 10)))
    (define-key map (kbd "M-v") (lambda () (interactive) (forward-line -10)))

    (define-key map (kbd "C-c r r") (lambda () (interactive) (revert-buffer nil t)))

    ;;scroll other window
    (define-key map (kbd "C-M-]") 'scroll-other-window)
    (define-key map (kbd "C-M-[") 'scroll-other-window-down)

    ;; Align your code in a pretty way.
    (define-key map (kbd "C-x \\") 'align-regexp)

    ;; Window switching.
    (define-key map (kbd "C-x O") (lambda () (interactive) (other-window -1)))  ;; back one
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
    (define-key map (kbd "C-c w s") 'switch-window)

    (define-key map (kbd "C-c b u") 'browse-url-at-point)
    (define-key map (kbd "C-c b U") 'browse-url)

    (define-key map (kbd "C-c M-z") 'buffer-pack/switch-to-term-or-get-back-to-buffer!)

    (define-key map (kbd "C-M-SPC") 'er/expand-region)
    (define-key map (kbd "C-c b ;") 'iedit-mode)

    (define-key map (kbd "C-c b +") 'buffer-pack/increment-number-at-point)
    (define-key map (kbd "C-c b -") 'buffer-pack/decrement-number-at-point)

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

(global-prettify-symbols-mode 1)

(setq load-prefer-newer t)

(defun dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))

(define-minor-mode dubcaps-mode
  "Toggle `dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
  :init-value nil
  :lighter " ψ"
  (if dubcaps-mode
      (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))

(add-hook 'text-mode-hook #'dubcaps-mode)

(provide 'buffer-pack)
;;; buffer-pack.el ends here
