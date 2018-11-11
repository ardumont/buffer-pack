;;; buffer-pack.el --- Buffer manipulation

;; Copyright (C) 2018  Antoine R. Dumont (@ardumont)
;; Author: Antoine R. Dumont (@ardumont) <antoine.romain.dumont@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'move-text)
(require 'dockerfile-mode)
(require 'dash)
(dash-enable-font-lock)
(require 'dash-functional)
(require 'tramp)

(defun buffer-pack/tramp-protocols ()
  "Filter the protocol methods from the tramp methods."
  (mapcar #'car tramp-methods))

(defun buffer-pack/tramp-method-for-protocol (protocol)
  "Ddtail PROTOCOL method for tramp."
  (-filter (-compose (-partial #'string= protocol) #'car) tramp-methods))

(require 'iedit)
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.txt$" . markdown-mode))
(require 'markdown-toc)
(require 'multiple-cursors)
(require 'git-gutter)
(require 'buffer-move)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;; Extend the default company mode mapping to the one
(add-hook 'company-mode-hook (lambda ()
                               (interactive)
                               (define-key company-active-map (kbd "C-h") 'delete-backward-char)
                               (define-key company-active-map (kbd "M-?") 'company-show-doc-buffer)
                               (define-key company-active-map (kbd "C-n") 'company-select-next)
                               (define-key company-active-map (kbd "C-p") 'company-select-previous)
                               (define-key company-active-map (kbd "M-/") 'company-complete)))

(require 'ace-window)
(require 'iy-go-to-char)
(require 'popwin) ;; master the popup buffer and C-g to stop them
(popwin-mode 1)

(require 's)
(require 'etags)
(require 'whitespace)

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
            (define-key ido-file-completion-map (kbd "~")
              (lambda ()
                (interactive)
                (if (looking-back "/")
                    (insert "~/")
                  (call-interactively 'self-insert-command))))))

(custom-set-variables
 '(whitespace-line-column 79)
 '(aw-keys '(?a ?s ?d ?f ?j ?k ?l))
 '(aw-background 'grey-out-the-back-during-selection)
 '(avy-keys '(?a ?s ?d ?e ?f ?g ?h ?j ?k ?l ?v ?m ?r ?u))
 '(make-backup-files nil)  ;; stop creating backup~ files
 '(auto-save-default nil)  ;; stop creating #autosave# files
 '(select-enable-clipboard t)
 '(select-enable-primary t)
 '(global-auto-revert-mode 1) ;; Auto refresh buffers
 '(global-auto-revert-non-file-buffers t)  ;; auto refresh dired
 '(auto-revert-verbose nil) ;; but be quiet about it
 '(inhibit-splash-screen t) ;; Do not show a splash screen.
 '(echo-keystrokes 0.1)     ;; Show incomplete commands while typing them.
 '(visible-bell t)          ;; Flash the screen on errors.
 '(column-number-mode t)    ;; column number in the modeline
 '(load-prefer-newer t)     ;; prefer newest file if present
 '(save-abbrevs 'silently)  ;; save abbreviations silently
 '(create-lockfiles nil)    ;; stop creating symlink named #something
 )

(defalias 'yes-or-no-p 'y-or-n-p) ;; "y" resp. "n" instead of "yes" resp. "no".

;; -----------------------------

(require 'ht)

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
    ;; Scroll other window
    (define-key map (kbd "C-M-]") 'scroll-other-window)
    (define-key map (kbd "C-M-[") 'scroll-other-window-down)
    ;; Align your code in a pretty way.
    (define-key map (kbd "C-x \\") 'align-regexp)
    ;; window and buffer movement
    (define-key map (kbd "C-c w r") 'rotate-windows)
    (define-key map (kbd "C-c w p") 'buf-move-up)
    (define-key map (kbd "C-c w n") 'buf-move-down)
    (define-key map (kbd "C-c w b") 'buf-move-left)
    (define-key map (kbd "C-c w f") 'buf-move-right)
    (define-key map (kbd "C-c w .") 'shrink-window-horizontally)
    (define-key map (kbd "C-c w ,") 'enlarge-window-horizontally)
    (define-key map (kbd "C-c w /") (lambda () (interactive) (enlarge-window -1)))
    (define-key map (kbd "C-c w '") (lambda () (interactive) (enlarge-window 1)))
    ;; jump to a window...
    (define-key map (kbd "C-x C-o") 'ace-window)
    (define-key map (kbd "C-x o")   'ace-window)
    ;; or a word
    (define-key map (kbd "C-c j") 'avy-goto-word-1)
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
  :lighter " β"
  :keymap buffer-pack-mode-map
  :t global)

(define-globalized-minor-mode global-buffer-pack-mode buffer-pack-mode buffer-pack-on)

(defun buffer-pack-on ()
  "Turn on `buffer-pack-mode'."
  (buffer-pack-mode +1))

(global-prettify-symbols-mode 1)

;; fix typos

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
(add-hook 'text-mode-hook #'smartscan-mode)

;; fix typos

(define-key ctl-x-map "\C-i" #'endless/ispell-word-then-abbrev)

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev.
Otherwise it will be global."
  (interactive "P")
  (let ((bef (downcase (or (thing-at-point 'word)
			   "")))
	aft)
    (call-interactively 'ispell-word)
    (setq aft (downcase
	       (or (thing-at-point 'word) "")))
    (unless (or (string= aft bef)
		(string= aft "")
		(string= bef ""))
      (message "\"%s\" now expands to \"%s\" %sally"
	       bef aft (if p "loc" "glob"))
      (define-abbrev
	(if p local-abbrev-table global-abbrev-table)
	bef aft))))

(add-hook 'isearch-mode-hook
	  (lambda ()
	    (define-key isearch-mode-map (char-to-string help-char) nil) ;; unbind C-h
	    (define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)
	    (define-key isearch-mode-map (kbd "C-d") 'delete-forward-char)
	    (define-key isearch-mode-map (kbd "M-?") isearch-help-map)

	    (define-key minibuffer-local-isearch-map (kbd "C-h") 'isearch-delete-char)
	    (define-key minibuffer-local-isearch-map (kbd "C-d") 'delete-forward-char)))

(provide 'buffer-pack)
;;; buffer-pack.el ends here
