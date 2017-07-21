;;; .emacs - an emacs initialization file created by Bill Clementson

;;__________________________________________________________________________
;;;;    Site-Specific Variables

;; See if we're on MS Windows or some other OS
(defvar mswindows-p (string-match "windows" (symbol-name system-type)))
(defvar macosx-p (string-match "darwin" (symbol-name system-type)))

;; Some file locations are relative to the HOME or the BIN directory
(defvar use-home)
(setq use-home (concat (expand-file-name "~") "/"))
(defvar use-bin
  (if mswindows-p
      "c:/bin/"
    (concat (expand-file-name "~") "/bin/")))

;; Common Lisp documentation (CLtL2, HyperSpec, ACL docs) locations
(defvar cltl2-root-url (concat use-home "docs/cltl/"))
(defvar common-lisp-hyperspec-root (concat use-home "docs/Hyperspec/"))
(defvar common-lisp-hyperspec-symbol-table (concat common-lisp-hyperspec-root "Data/Map_Sym.txt"))

;; Specify where backup files are stored
(setq backup-directory-alist (quote ((".*" . "~/.backups"))))

;; Location of Info documentation
(setq-default Info-default-directory-list
	      (list (expand-file-name (concat use-home "info"))
		    (expand-file-name (concat (getenv "EMACS_DIR") "/info"))))

;;__________________________________________________________________________
;;;;    Initial Code Load

(require 'cl)
(require 'dired)
(require 'font-lock)
(require 'recentf)
(require 'hippie-exp)
(require 'browse-url)
(require 'comint)
(ignore-errors (require 'color-theme))
(ignore-errors (require 'w3-auto))
(ignore-errors (require 'ecb))

;;__________________________________________________________________________
;;;;    System Customizations

;; Set buffer behaviour
(setq next-line-add-newlines nil)
(setq scroll-step 1)
(setq scroll-conservatively 5)

;; Enable emacs functionality that is disabled by default
(put 'eval-expression 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq enable-recursive-minibuffers t)

;; Misc customizations
(fset 'yes-or-no-p 'y-or-n-p)           ;replace y-e-s by y
(setq inhibit-startup-message t)        ;no splash screen
(defconst use-backup-dir t)             ;use backup directory
(defconst query-replace-highlight t)    ;highlight during query
(defconst search-highlight t)           ;highlight incremental search
(setq ls-lisp-dirs-first t)             ;display dirs first in dired
(global-font-lock-mode t)               ;colorize all buffers
(setq ecb-tip-of-the-day nil)           ;turn off ECB tips
(recentf-mode 1)                        ;recently edited files in menu

;; Conventional mouse/arrow movement & selection
(delete-selection-mode t)

;; Ediff customizations
(defconst ediff-ignore-similar-regions t)
(defconst ediff-use-last-dir t)
(defconst ediff-diff-options " -b ")

;; Dired customizations
(setq dired-listing-switches "-l")

(defun dired-mouse-find-file (event)
  "In dired, visit the file or directory name you double-click on (EVENT)."
  (interactive "e")
  (let (file)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(setq file (dired-get-filename))))
    (select-window (posn-window (event-end event)))
    (find-file (file-name-sans-versions file t))))

(defun my-dired-find-file ()
  "In dired, visit the file or directory name you are on (in the same window)."
  (interactive)
  (let (file)
    (save-excursion
      (setq file (dired-get-filename))
      (find-file (file-name-sans-versions file t)))))

(add-hook 'dired-mode-hook
	  '(lambda()
	     (define-key dired-mode-map [delete] 'dired-do-delete)
	     (define-key dired-mode-map [C-return] 'dired-find-file-other-window)
	     (define-key dired-mode-map [C-down-mouse-1] 'mouse-buffer-menu)
	     (define-key dired-mode-map [double-down-mouse-1] 'dired-mouse-find-file)
	     (define-key dired-mode-map [return] 'my-dired-find-file)))

;; Word completion customizations
(defconst dabbrev-always-check-other-buffers t)
(defconst dabbrev-abbrev-char-regexp "\\sw\\|\\s_")

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-file-name-partially
	try-complete-file-name
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol
	try-expand-whole-kill))

;; Set the name of the host and current path/file in title bar:
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
	    '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Column & line numbers in mode bar
(column-number-mode t)
(line-number-mode t)

;; Code display options (highlight parens & colorize)
(show-paren-mode 1)

;;__________________________________________________________________________
;;;;    Programming - Common Lisp

;; Specify modes for Lisp file extensions
(setq auto-mode-alist
      (append '(
		("\\.emacs$" . emacs-lisp-mode)
		("\\.lisp$" . lisp-mode)
		("\\.lsp$" . lisp-mode)
		("\\.cl$" . lisp-mode)
		("\\.system$" . lisp-mode)
		("\\.scm$" . scheme-mode)
		("\\.ss$" . scheme-mode)
		("\\.sch$" . scheme-mode)
		)auto-mode-alist))

;; Lisp hook customizations
(add-hook 'lisp-mode-hook
	  (lambda ()
	    (imenu-add-to-menubar "Symbols")
	    (outline-minor-mode)
	    (make-local-variable 'outline-regexp)
	    (setq outline-regexp "^(.*")
	    (ignore-errors (semantic-default-elisp-setup))
	    (set (make-local-variable lisp-indent-function)
		 'common-lisp-indent-function)))

;;__________________________________________________________________________
;;;;    Programming - Common Lisp (functions not provided by CL modes)

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t nil)))


(defun insert-balanced-comments (arg)
  "Insert a set of Common Lisp balanced comments around the
s-expression containing point.  If this command is invoked repeatedly
(without any other command occurring between invocations), the comment
region progressively moves outward over enclosing expressions."
  (interactive "*p")
  (save-excursion
    (when (eq last-command this-command)
      (when (search-backward "#|" nil t)
        (save-excursion
          (delete-char 2)
          (while (and (< (point) (point-max)) (not (looking-at " *|#")))
            (forward-sexp))
          (replace-match ""))))
    (while (> arg 0)
      (backward-char 1)
      (cond ((looking-at ")") (incf arg))
            ((looking-at "(") (decf arg))))
    (insert "#|")
    (forward-sexp)
    (insert "|#")))

(defun remove-balanced-comments ()
  "Remove a set of Common Lisp balanced comments enclosing point."
  (interactive "*")
  (save-excursion
    (when (search-backward "#|" nil t)
      (delete-char 2)
      (while (and (< (point) (point-max)) (not (looking-at " *|#")))
	(forward-sexp))
      (replace-match ""))))

;;__________________________________________________________________________
;;;;    Programming - Lisp in Info (allow eval of sexps in Info docs)

(add-hook 'Info-mode-hook
	  '(lambda ()
	     (interactive)
	     (define-key Info-mode-map [(control c) (x)] 'copy-eval-dwim-lisp)))

;;__________________________________________________________________________
;;;;    Lisp Key Overrides

;; Lisp documentation
(global-set-key [f1]
		'(lambda (arg)
		   (interactive "P")
		   (ignore-errors
		     (let ((common-lisp-hyperspec-root
			    (if macosx-p
				(concat "file://" common-lisp-hyperspec-root)
			      common-lisp-hyperspec-root)))
		       (load-library hyperspec-prog)
		       (if arg
			   (common-lisp-hyperspec-format (char-to-string (char-after (point))))
			 (common-lisp-hyperspec (thing-at-point 'symbol)))))))

(global-set-key [(shift f1)]
		'(lambda (arg)
		   (interactive "P")
		   (ignore-errors
		     (let ((browse-url-browser-function 'browse-url-w3)
			   (common-lisp-hyperspec-root (concat "file://" common-lisp-hyperspec-root)))
		       (load-library hyperspec-prog)
		       (if arg
			   (common-lisp-hyperspec-format (char-to-string (char-after (point))))
			 (common-lisp-hyperspec (thing-at-point 'symbol)))))))

(global-set-key [(control f1)]
		'(lambda ()
		   (interactive)
		   (ignore-errors
		     (let ((acldoc-local-root
			    (if macosx-p
				(concat "file://" acldoc-local-root)
			      acldoc-local-root)))
		       (require 'url)
		       (require 'acldoc)
		       (unless acldoc-index-alist (acldoc-build-index))
		       (acldoc (thing-at-point 'symbol))))))

(global-set-key [(control shift f1)]
		'(lambda ()
		   (interactive)
		   (ignore-errors
		     (let ((browse-url-browser-function 'browse-url-w3)
			   (acldoc-local-root (concat "file://" acldoc-local-root)))
		       (require 'url)
		       (require 'acldoc)
		       (unless acldoc-index-alist (acldoc-build-index))
		       (acldoc (thing-at-point 'symbol))))))

(global-set-key [(meta f1)]
		'(lambda ()
		   (interactive)
		   (let ((cltl2-root-url
			  (if macosx-p
			      (concat "file://" cltl2-root-url)
			    cltl2-root-url)))
		     (load-library cltl2-prog)
		     (cltl2-lookup (thing-at-point 'symbol)))))

(global-set-key [(meta shift f1)]
		'(lambda ()
		   (interactive)
		   (ignore-errors
		     (let ((browse-url-browser-function 'browse-url-w3)
			   (cltl2-root-url (concat "file://" cltl2-root-url)))
		       (load-library cltl2-prog)
		       (cltl2-lookup (thing-at-point 'symbol))))))

(global-set-key [(control meta f1)]
		'(lambda ()
		   (interactive)
		   (ignore-errors
		     (info (concatenate 'string "(gcl) " (thing-at-point 'symbol))))))

;;__________________________________________________________________________
;;;;    Standard Key Overrides

;; Completions in minibuffer
(define-key minibuffer-local-map [tab] 'comint-dynamic-complete)

;; Mouse
(global-set-key [down-mouse-2] 'imenu)

;; Disable mouse-2 event that was appending text into documents
(global-set-key [mouse-2] nil)

;; Prevent accidentally killing emacs.
(global-set-key [(control x) (control c)]
		'(lambda ()
		   (interactive)
		   (if (y-or-n-p-with-timeout "Do you really want to exit Emacs ? " 4 nil)
		       (save-buffers-kill-emacs))))

;; Common buffer/window control shortcuts
(global-set-key [f6] 'other-window)
(global-set-key [f7] 'delete-other-windows)

;; Shells
(global-set-key [f12]
		'(lambda ()
		   (interactive)
		   (eshell)))

(global-set-key [(control f12)]
		'(lambda ()
		   (interactive)
		   (cond
		    (mswindows-p
		     (let ((explicit-shell-file-name
			    (expand-file-name (concat (getenv "EMACS_DIR") "/bin/cmdproxy.exe")))
			   (shell-file-name "cmdproxy.exe"))
		       (shell)))
		    (t (shell)))))

(global-set-key [(meta f12)]
		'(lambda ()
		   (interactive)
		   (let ((explicit-shell-file-name
			  (if mswindows-p
			      "bash.exe"
			    "bash"))
			 (shell-file-name
			  (if mswindows-p
			      "bash.exe"
			    "bash")))
		     (shell))))

;; Shortcuts to common functions
(global-set-key [(control c) (f)] 'find-function-at-point)
(global-set-key [(control c) (F)] 'ffap)

(global-set-key [(control c) (s)]
		(function
		 (lambda ()
		   (interactive)
		   (let ((arg (thing-at-point 'symbol)))
		     (search-forward arg)))))

(global-set-key [(control c) (r)]
		(function
		 (lambda ()
		  (interactive)
		  (let ((arg (thing-at-point 'symbol)))
		    (search-backward arg)))))

(global-set-key [(control c) (/)] 'hippie-expand)
(global-set-key [(control c) (\])] 'goto-match-paren)
(global-set-key [(control c) (g)] 'goto-line)
(global-set-key [(control c) (a)] 'mark-whole-buffer)

;;__________________________________________________________________________
;;;;    MS Windows Customizations

;; Note that the cua-emul, gnuserve & cua libraries are optional
(if mswindows-p
    (progn
      ;; Ctrl-tab, Ctrl-F4, etc like Windows
      (ignore-errors
	(progn
	  (require 'cua-emul)
	  (setq cua-emul-force t)
	  (turn-on-cua-emul-mode)))

      ;; Grep equivalent on Windows
      ;;(setq grep-command "c:/cygwin/bin/grep -n -a -e ")
      (setq grep-command "findstr /n /s ")

      ;; Windows Execute from dired
      (define-key dired-mode-map "w"
	(function
	 (lambda ()
	   (interactive)
	   (setq w32-shellex-no-dired-hook t)
	   (require 'w32-shellex)
	   (w32-shellex-dired-on-objects))))

      ;; Start gnuserv on Windows
      (if (or (eq window-system 'w32) (eq window-system 'win32))
	  (ignore-errors
	    (progn
	      (require 'gnuserv)
	      (setq server-done-function 'bury-buffer
		    gnuserv-frame (car (frame-list)))
	      (gnuserv-start)
	      ;; Open buffer in existing frame instead of creating new one...
	      (setq gnuserv-frame (selected-frame))
	      (message "gnuserv started."))))

      ;; C-z=Undo, C-c=Copy, C-x=Cut, C-v=Paste
      (ignore-errors
	(progn
	  (require 'cua)
	  (CUA-mode t)))))

;;__________________________________________________________________________
;;;;    Mac OS X Customizations

(if macosx-p
    (progn
      ;; Set some common keys
      (global-set-key [kp-delete] 'delete-char)
      (global-set-key [(control kp-home)] 'beginning-of-buffer)
      (global-set-key [(control kp-end)] 'end-of-buffer)
      ;; Custom code to open browser on Mac OS X
      (setq browse-url-browser-function
	    '(lambda (url &optional new-win)
	       (do-applescript (concat "open location \""
				       url "\""))))))

;; end of emacs.el
