(install-packs '(exec-path-from-shell
                 ;; manipulate folding sexp
                 fold-dwim
                 ;; to make some awesome stuff on multiple line in one time
                 multiple-cursors
                 move-text
                 auto-complete
                 git-gutter
                 projectile
                 s
                 dash))

(require 'multiple-cursors)
(require 'git-gutter)
(require 'auto-complete)

(require 'projectile)
(projectile-global-mode)

;; setup the path
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; some text/font/color tweaks

(setq-default fill-column 120)
(set-face-background 'default "black")

(set-language-environment "UTF-8")
(blink-cursor-mode 1)

;; C-x C-l to lower case ; C-x C-u to upper case

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(require 'shell)
(require 's)
(require 'dash)

(defun buffer-pack/hostname! () "Return the hostname of the current computer." (-> "hostname" shell-command-to-string s-trim))

(let* ((hostname  (buffer-pack/hostname!))
       (font-size (if (string= hostname "dagobah") 140 100)))
  ;; keep the default font but globally set the size
  (set-face-attribute 'default nil :height font-size))

;; etags

(require 'etags)

;; to improve the movement in files

(defvar smart-use-extended-syntax nil
  "If t the smart symbol functionality will consider extended
syntax in finding matches, if such matches exist.")

(defvar smart-last-symbol-name ""
  "Contains the current symbol name.

This is only refreshed when `last-command' does not contain
either `smart-symbol-go-forward' or `smart-symbol-go-backward'")

(make-local-variable 'smart-use-extended-syntax)

(defvar smart-symbol-old-pt nil
  "Contains the location of the old point")

(defun smart-symbol-goto (name direction)
  "Jumps to the next NAME in DIRECTION in the current buffer.

DIRECTION must be either `forward' or `backward'; no other option
is valid."

  ;; if `last-command' did not contain
  ;; `smart-symbol-go-forward/backward' then we assume it's a
  ;; brand-new command and we re-set the search term.
  (unless (memq last-command '(smart-symbol-go-forward
                               smart-symbol-go-backward))
    (setq smart-last-symbol-name name))
  (setq smart-symbol-old-pt (point))
  (message (format "%s scan for symbol \"%s\""
                   (capitalize (symbol-name direction))
                   smart-last-symbol-name))
  (unless (catch 'done
            (while (funcall (cond
                             ((eq direction 'forward) ; forward
                              'search-forward)
                             ((eq direction 'backward) ; backward
                              'search-backward)
                             (t (error "Invalid direction"))) ; all others
                            smart-last-symbol-name nil t)
              (unless (memq (syntax-ppss-context
                             (syntax-ppss (point))) '(string comment))
                (throw 'done t))))
    (goto-char smart-symbol-old-pt)))

(defun smart-symbol-go-forward ()
  "Jumps forward to the next symbol at point"
  (interactive)
  (smart-symbol-goto (smart-symbol-at-pt 'end) 'forward))

(defun smart-symbol-go-backward ()
  "Jumps backward to the previous symbol at point"
  (interactive)
  (smart-symbol-goto (smart-symbol-at-pt 'beginning) 'backward))

(defun smart-symbol-at-pt (&optional dir)
  "Returns the symbol at point and moves point to DIR (either `beginning' or `end') of the symbol.

If `smart-use-extended-syntax' is t then that symbol is returned
instead."
  (with-syntax-table (make-syntax-table)
    (if smart-use-extended-syntax
        (modify-syntax-entry ?. "w"))
    (modify-syntax-entry ?_ "w")
    (modify-syntax-entry ?- "w")
    ;; grab the word and return it
    (let ((word (thing-at-point 'word))
          (bounds (bounds-of-thing-at-point 'word)))
      (if word
          (progn
            (cond
             ((eq dir 'beginning) (goto-char (car bounds)))
             ((eq dir 'end) (goto-char (cdr bounds)))
             (t (error "Invalid direction")))
            word)
        (error "No symbol found")))))

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

;; some personal functions that extends the one loaded from user.el

(defun exists-session-or-spawn-it (session-name session-command)
  "Given a session-name, check the existence of such a session. If it doesn't exist, spawn the session via the command session-command"
  (let ((proc (get-buffer-process session-name)))
    (unless (and proc (eq (process-status proc) 'run))
      (funcall session-command))))

(defun switch-to-buffer-or-nothing (process-name buffer-name)
  "Given a process name, switch to the corresponding buffer-name (if the process is running) or does nothing."
  (unless (string= (buffer-name) buffer-name)
    (let ((proc (get-buffer-process process-name)))
      (if (and proc (eq (process-status proc) 'run))
          (switch-to-buffer buffer-name)))))

;; examples
;; (switch-to-buffer-or-nothing "*swank*" "*slime-repl nil*")    ;; clojure-jack-in
;; (switch-to-buffer-or-nothing "*terminal<1>*" "*terminal<1>*") ;; multi-term

(defun multi-term-once ()
  "Check the existence of a terminal with multi-term.
If it doesn't exist, launch it. Then go to this buffer in another buffer."
  (interactive)
  (unless (exists-session-or-spawn-it "*terminal<1>*" 'multi-term)
    (switch-to-buffer-or-nothing "*terminal<1>*" "*terminal<1>*")))

(add-hook 'ido-setup-hook
 (lambda ()
   ;; Go straight home
   (define-key ido-file-completion-map (kbd "~") (lambda ()
                                                   (interactive)
                                                   (if (looking-back "/")
                                                       (insert "~/")
                                                       (call-interactively 'self-insert-command))))))

;; Must-have setup for Emacs to operate like a modern application.
(setq
 ;; Do not show a splash screen.
 inhibit-splash-screen t
 ;; Show incomplete commands while typing them.
 echo-keystrokes 0.1
 ;; Flash the screen on errors.
 visible-bell t
 column-number-mode t)

;; "y" resp. "n" instead of "yes" resp. "no".
(defalias 'yes-or-no-p 'y-or-n-p)

(defvar buffer-pack-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") 'smart-symbol-go-forward)
    (define-key map (kbd "M-p") 'smart-symbol-go-backward)

    (define-key map (kbd "M-/") 'auto-complete)
    (define-key map (kbd "C-M-h") 'backward-kill-word)
    (define-key map (kbd "M-?") 'help-command)

    ;; multiple-cursors
    (define-key map (kbd "C->") 'mc/mark-next-like-this)
    (define-key map (kbd "C-<") 'mc/mark-previous-like-this)
    (define-key map (kbd "C-c C-<") 'mc/mark-all-like-this)

    (define-key map [remap goto-line] 'goto-line-with-feedback)

    (define-key map (kbd "C-x C-r") 'rgrep)

    (define-key map (kbd "C-c C-z") 'multi-term-once)

    (define-key global-map (kbd "C-+") 'text-scale-increase)
    (define-key global-map (kbd "C--") 'text-scale-decrease)

    (define-key map (kbd "C-w") 'kill-region)
    (define-key map (kbd "C-y") 'yank)

    (define-key map (kbd "C-v") (lambda () (interactive) (next-line 10)))
    (define-key map (kbd "M-v") (lambda () (interactive) (previous-line 10)))

    (define-key map (kbd "M-/") 'complete-symbol)

    (define-key map (kbd "C-c r r") 'revert-buffer)

    map)
  "Keymap for Buffer-pack mode.")

(define-minor-mode buffer-pack-mode
  "Minor mode to consolidate Emacs' buffer-pack extensions.

\\{buffer-pack-mode-map}"
  :lighter " BP"
  :keymap buffer-pack-mode-map)

(define-globalized-minor-mode buffer-pack-global-mode buffer-pack-mode buffer-pack-on)

(defun buffer-pack-on ()
  "Turn on `buffer-pack-mode'."
  (buffer-pack-mode +1))

(buffer-pack-global-mode)
