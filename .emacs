;;; .emacs - an emacs initialization file created by Bill Clementson

;;__________________________________________________________________________
;;;;    Site-Specific Variables 

;; Some file locations are relative to the HOME directory
(defvar use-home)
(setq use-home (concat (expand-file-name "~") "/"))

;; Common Lisp documentation locations
(defvar cltl2-root-url (concat use-home "docs/cltl/"))
(defvar cltl2-prog (concat use-home "site/ilisp/extra/cltl2"))
(defvar common-lisp-hyperspec-root (concat use-home "docs/Hyperspec/"))
(defvar common-lisp-hyperspec-symbol-table (concat common-lisp-hyperspec-root "Data/Map_Sym.txt"))
(defvar hyperspec-prog (concat use-home "site/ilisp/extra/hyperspec"))

;; Gnu CLISP - ILISP (switches for ANSI, ILISP & no banner)
(defvar clisp-hs-program "clisp -ansi -I -q")

;; Corman Common Lisp - ESHELL
(defvar cormanlisp-program "corman.bat")

;; Franz Allegro Common Lisp - ELI
(defvar fi:common-lisp-image-name "C:/Program Files/ACL/allegro-ansi.exe")
(defvar fi:common-lisp-directory "C:/Program Files/ACL/")

;; Xanalys LispWorks - ILISP
(defvar lispworks-program "lispworks")

;; Default Lisp implementation to use
(defvar lisp-used :clisp-ilisp "Recognized values are :clisp-ilisp, :acl-eli, :lw-ilisp, :corman-eshell")

;; Set up load path 
(setq load-path (append (list (concat use-home "")
                              (concat use-home "site")
                              (concat use-home "site/ilisp")
                              (concat fi:common-lisp-directory "eli")
                              (concat use-home "site/w3/lisp"))
                        load-path))

;; Specify where backup files are stored
(setq backup-directory-alist (quote ((".*" . "c:/.backups"))))

;;__________________________________________________________________________
;;;;    Initial Code Load

(require 'cl)
(require 'dired)
(require 'font-lock)
(require 'recentf)
(recentf-mode 1) 
(require 'pc-mode)
(require 'mouse-sel)
(require 'hippie-exp)
(require 'browse-url)

(autoload 'dabbrev-expand "dabbrev" "Word completion." t)
(autoload 'turn-on-lazy-lock "lazy-lock" "Force enable Lazy Lock mode.")

(eval-when (compile)
  (load "esh-mode")
  (load "fi-site-init")
  (require 'ilisp))

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
(put 'eval-expression 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq enable-recursive-minibuffers t)

;; Misc customizations
(fset 'yes-or-no-p 'y-or-n-p)           ;replace y-e-s by y
(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-message t)        ;no splash screen
(defconst use-backup-dir t)             ;use backup directory
(defconst query-replace-highlight t)    ;highlight during query
(defconst search-highlight t)           ;highlight incremental search
(setq ls-lisp-dirs-first t)             ;display dirs first in dired
(global-font-lock-mode t)               ;colorize all buffers
(if (string= "w32" window-system)
    (w32-send-sys-command 61488))       ;maximize window on startup

;; Ediff customizations
(defconst ediff-ignore-similar-regions t)
(defconst ediff-use-last-dir t)
(defconst ediff-diff-options " -b ")

;; Dired customizations
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
	  (function
	   (lambda()
	     (define-key dired-mode-map [delete] 'dired-do-delete)
	     (define-key dired-mode-map [C-return] 'dired-find-file-other-window)
	     (define-key dired-mode-map [C-down-mouse-1] 'mouse-buffer-menu)
	     (define-key dired-mode-map [double-down-mouse-1] 'dired-mouse-find-file)	     
	     (define-key dired-mode-map [return] 'my-dired-find-file))))

;; Set the name of the host and current path/file in title bar:
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
	    '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(column-number-mode t)		        ;column number in modeline (status)
(line-number-mode t)		        ;line number in modeline (status bar)

;; Grab possible word completions from all active sessions
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

;; Code display options
(show-paren-mode 1)		        ;highlight matching parenthesis
(add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)

;;__________________________________________________________________________
;;;;    Programming - Common Lisp

;; Specify modes for lisp file extensions
(setq auto-mode-alist
      (append '(
		("\\.emacs$" . emacs-lisp-mode)
		("\\.lisp$" . lisp-mode)
		("\\.lsp$" . lisp-mode)
		("\\.cl$" . lisp-mode)
		)auto-mode-alist))

;; Lisp documentation
(load-library cltl2-prog)
(load-library hyperspec-prog)

(defun start-lisp ()
  "Set up environment for the lisp implementation that was chosen"
  (interactive)
  (setq lisp-indent-function 'common-lisp-indent-function)
  (add-hook
   'lisp-mode-hook
   (function
    (lambda ()
      (imenu-add-to-menubar "Symbols"))))

  ;; Start up Lisp 
  (cond
   ((or (eq lisp-used :clisp-ilisp)
	(eq lisp-used :lw-ilisp))
    ;; CLISP, LispWorks or ACL using ILISP
    ;; Change default key prefix from C-Z to C-c FSF standard
    (setq ilisp-*use-fsf-compliant-keybindings* t
	  ilisp-*arglist-message-lisp-space-p* t
	  ilisp-print-info-message-command t
	  lisp-no-popper t)	
	
    (require 'completer)
    (require 'ilisp)
    (require 'clisp-indent)

    ;; Fix clisp interaction buffer (Windows)
    (modify-coding-system-alist 'process "lisp" 'unix)

    ;; All the *.d and *.lisp sources are in UTF-8 encoding.
    (modify-coding-system-alist 'file "\\.\\(d\\|lisp\\)\\'" 'utf-8)
	
     (add-hook 'ilisp-load-hook
               (function
                 (lambda ()
	     ;; Set a keybinding for the COMMON-LISP-HYPERSPEC command
                   (defkey-ilisp "" 'common-lisp-hyperspec)
                   (message "Running ilisp-load-hook")
		;; Set the inferior Lisp directory to the directory of
                   ;; the buffer that spawned it on the first prompt.
                   (add-hook 'ilisp-init-hook
                             '(lambda ()
                               (default-directory-lisp ilisp-last-buffer))))))
     (cond
       ((eq lisp-used :clisp-ilisp)
         (clisp-hs))
       ((eq lisp-used :lw-ilisp)
         (lispworks))))
   ((eq lisp-used :acl-eli)
    ;; Franz Allegro Common Lisp using eli
    (load "fi-site-init")
    (fi:common-lisp fi:common-lisp-buffer-name
		    fi:common-lisp-directory
		    fi:common-lisp-image-name
		    fi:common-lisp-image-arguments
		    fi:common-lisp-host))
   ((eq lisp-used :corman-eshell)
    ;; Corman Common Lisp using eshell
    (cond
     ((string= "w32" window-system)
      (eshell)
      (set-buffer "*eshell*")
      (insert cormanlisp-program)
      (eshell-send-input))))))

(defun goto-match-paren (arg)
"Go to the matching parenthesis if on parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t nil)))

;;__________________________________________________________________________
;;;;    Programming - Elisp

(add-hook 'emacs-lisp-mode-hook
	  (function
	   (lambda ()
	     (interactive)
	     (require 'eldoc)
	     (turn-on-eldoc-mode)
	     (define-key emacs-lisp-mode-map [f3] 'find-function-at-point)
	     (define-key emacs-lisp-mode-map [(shift f3)] 'ffap)
             ;; Default to auto-indent on Enter
	     (define-key emacs-lisp-mode-map [(control j)] 'newline)
	     (define-key emacs-lisp-mode-map [(control m)] 'newline-and-indent))))

;;__________________________________________________________________________
;;;;    Standard Key Overrides

;; Mouse 
(global-set-key [down-mouse-2] 'imenu)
;; Disable mouse-2 event that was appending text into documents
(global-set-key [mouse-2] nil)

;; This binds word completions
(global-set-key [(control /)] 'hippie-expand)

;; Prevent accidentally killing emacs.
(global-set-key [(control x) (control c)]
		(function
		 (lambda ()
		   (interactive)
		   (if (y-or-n-p-with-timeout "Do you really want to exit Emacs ? " 4 nil)
		       (save-buffers-kill-emacs)))))

;; Match parentheses
(global-set-key [(control \])] 'goto-match-paren)

;; Common F-key shortcuts
(global-set-key [f4] 'kill-this-buffer)
(global-set-key [(control f4)] 'delete-window)
(global-set-key [(meta f4)] 'save-buffers-kill-emacs)
(global-set-key [f6] 'other-window)
(global-set-key [f7] 'delete-other-windows)

;; Eshell and Windows shell
(global-set-key [f12]
		(function
		 (lambda ()
		   (interactive)
		   (eshell))))
(global-set-key [(control f12)]
		(function
		 (lambda ()
		   (interactive)
		   (cond
		    ((string-match "windows" (symbol-name system-type))
		     (let ((explicit-shell-file-name
			    (expand-file-name (concat (getenv "EMACS_DIR") "/bin/cmdproxy.exe")))
			   (shell-file-name "cmdproxy.exe"))
		       (shell)))
		    (t (shell))))))

;;__________________________________________________________________________
;;;;    Lisp Key Overrides

(global-set-key [f1] 'common-lisp-hyperspec)
(global-set-key [(meta f1)] 'cltl2-lookup)
(global-set-key [f5] 'start-lisp)
(global-set-key [f11] 'comment-region)

(global-set-key [(control meta f5)]
		(function
                  (lambda ()
                    (interactive)
                    (cond
                      ((eq lisp-used :clisp-ilisp)
                        (setq lisp-used :acl-eli))
                      ((eq lisp-used :acl-eli)
                        (setq lisp-used :lw-ilisp))
                      ((eq lisp-used :lw-ilisp)
                        (setq lisp-used :corman-eshell))
                      (t (setq lisp-used :clisp-ilisp)))
                    (message "lisp-used: %s" lisp-used))))

;;__________________________________________________________________________
;;;;    Windows Key Overrides

;; Windows-like mouse/arrow movement & selection
(pc-selection-mode)                 
(delete-selection-mode t)           

;; C-tab swaps buffers
(global-set-key [(control tab)] 'mode-line-other-buffer)

;; Prompts for line to jump to
(global-set-key [(meta g)] 'goto-line)	

;; C-a selects all text in buffer
(global-set-key [(control a)] 'mark-whole-buffer)
  
;; C-s is reserved for search; use Alt-s to save:
(global-set-key [(meta s)] 'save-buffer)

;; C-z=Undo, C-c=Copy, C-x=Cut, C-v=Paste
(require 'cua)
(CUA-mode t)

;;__________________________________________________________________________
;;;;    Start Directory

(find-file "~/")
;; emacs.el ends here
