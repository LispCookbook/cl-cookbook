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
(defvar cltl2-prog (concat use-home "site/ilisp/extra/cltl2"))
(defvar common-lisp-hyperspec-root (concat use-home "docs/Hyperspec/"))
(defvar common-lisp-hyperspec-symbol-table (concat common-lisp-hyperspec-root "Data/Map_Sym.txt"))
(defvar hyperspec-prog (concat use-home "site/ilisp/extra/hyperspec"))
(setq acldoc-local-root (concat use-bin "acl-6.2/"))
(setq acldoc-use-local t)

;; Gnu CLISP - Inferior Lisp Mode & ILISP (switches for ANSI & no banner)
(defvar clisp-dir
  (if mswindows-p
      (concat use-bin "clisp-2.31/full/")
    "/sw/bin/"))
(defvar clisp-exe
  (if mswindows-p
      (concat clisp-dir "lisp.exe")
    (concat clisp-dir "clisp")))
(defvar clisp-hs-program
  (if mswindows-p
      (concat clisp-exe " -B " clisp-dir " -M " clisp-dir "lispinit.mem -ansi -q")
    (concat clisp-exe " -B " clisp-dir " -ansi -q")))

;; Franz Allegro Common Lisp - ELI (switches for listener & no banner)
(defvar acl-dir (concat use-bin "acl-6.2/"))
(setq fi:common-lisp-image-name
      (if mswindows-p
	  (concat acl-dir "alisp.exe")
	(concat acl-dir "alisp")))
;;(setq fi:common-lisp-image-name (concat acl-dir "allegro-ansi.exe"))
(setq fi:common-lisp-directory acl-dir)
(if mswindows-p
    (setq fi:common-lisp-image-arguments '("+B" "+cm")))
(setq fi:lisp-evals-always-compile t)
(setq fi:legacy-keybindings nil)
(setq fi:arglist-on-space t)
;;(setq fi:auto-arglist-pop-up-style '("*CL-arglist*" . nil))
(setq fi:auto-arglist-pop-up-style '(minibuffer))
(setq fi:pop-up-temp-window-behavior '(minibuffer))
;;(setq fi:pop-up-temp-window-behavior '(other . t))
;;(setq fi:pop-up-temp-window-behavior '(split . t))

;; Xanalys LispWorks - ILISP
(defvar lispworks-dir 
  (if mswindows-p
      (concat use-bin "lispworks-4.2/")
    (concat use-bin "lispworks-4.3/")))
(defvar lispworks-program 
  (if mswindows-p
      (concat lispworks-dir "lw42-console.exe")
    (concat lispworks-dir "lw43-console")))

;; Corman Common Lisp - Inferior Lisp Mode 
(defvar cormanlisp-dir (concat use-bin "corman-1.5/"))
(defvar cormanlisp-exe (concat cormanlisp-dir "clconsole.exe"))
(defvar cormanlisp-program (concat cormanlisp-exe " -image " cormanlisp-dir "cormanlisp.img"))

;; OpenMCL - ILISP
(setq openmcl-program (concat use-bin "ccl/scripts/openmcl"))

;; SBCL - ILISP
(setq sbcl-exe (concat use-bin "sbcl"))
(setq sbcl-program (concat sbcl-exe " --core " use-bin "sbcl.core"))

(defvar lisp-implementations '() "Lisp implementations installed.")

;; Setup list of Lisp implementations that have been installed
(if (file-exists-p sbcl-exe) (setq lisp-implementations (cons :sbcl-ilisp lisp-implementations)))
(if (file-exists-p openmcl-program) (setq lisp-implementations (cons :openmcl-ilisp lisp-implementations)))
(if (file-exists-p lispworks-program) (setq lisp-implementations (cons :lw-ilisp lisp-implementations)))
(if (file-exists-p cormanlisp-exe) (setq lisp-implementations (cons :corman-inf lisp-implementations)))
(if (file-exists-p clisp-exe) (setq lisp-implementations (cons :clisp-inf lisp-implementations)))
(if (file-exists-p clisp-exe) (setq lisp-implementations (cons :clisp-ilisp lisp-implementations)))
(if (file-exists-p fi:common-lisp-image-name) (setq lisp-implementations (cons :acl-eli lisp-implementations)))

;; Default Lisp to use (set to one of :acl-eli, :clisp-ilisp, :clisp-inf, :corman-inf, :lw-ilisp, :openmcl-ilisp, sbcl-ilisp)
(defvar lisp-used (elt lisp-implementations 0) "Last Lisp implementation used.")

;; Set up load path 
(setq load-path (append (list (concat use-home "")
                              (concat use-home "site")
                              (concat fi:common-lisp-directory "eli")
                              (concat use-home "site/ilisp")
                              (concat use-home "site/ecb")
                              (concat use-home "site/eieio")
                              (concat use-home "site/semantic")
                              (concat use-home "site/speedbar")
			      (concat use-home "site/w3/lisp"))
                        load-path))

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
(require 'lazy-lock)
(require 'recentf)
(require 'mouse-sel)
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
(pc-selection-mode)                 
(delete-selection-mode t)           

(defun maximize-frame (&optional frame)
"Maximize the selected FRAME."
(interactive)
(or frame
    (setq frame (selected-frame)))
(let ((pixels-per-col (/ (float (frame-pixel-width))
			 (frame-width)))
      (pixels-per-row (/ (float
			  (frame-pixel-height)) (frame-height))))
  (set-frame-size frame
		  ;; truncate or round?
		  (truncate (/
			     (x-display-pixel-width) pixels-per-col))
		  ;; reduce size to account for the toolbar
		  (- (truncate (/
				(x-display-pixel-height) pixels-per-row)) 7))
  (set-frame-position frame 0 0)))

;; Maximize frame on startup
(if (string= "w32" window-system)
    (w32-send-sys-command 61488)
  (maximize-frame))

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
(add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)

;; Font lock colorization customizations
(defun color-theme-billc ()
  "Bill Clementson's custom color theme."
  (interactive)
  (color-theme-install
   '(color-theme-billc
     ((foreground-color . "black")
      (background-color . "white")
      (mouse-color . "sienna3")
      (cursor-color . "black")
      (border-color . "Blue")
      (background-mode . light))
     (default ((t (nil))))
     (modeline ((t (:background "dark gray" :foreground "black"))))
     (modeline-buffer-id ((t (:background "dark gray" :foreground "black"))))
     (modeline-mousable ((t (:background "dark gray" :foreground "black"))))
     (modeline-mousable-minor-mode ((t (:background "dark gray" :foreground "black"))))
     (highlight ((t (:foreground "black" :background "darkseagreen2"))))
     (bold ((t (:bold t))))
     (italic ((t (:italic t))))
     (bold-italic ((t (:bold t :italic t))))
     (region ((t (:foreground "black" :background "snow3"))))
     (secondary-selection ((t (:background "paleturquoise"))))
     (underline ((t (:underline t))))
     (lazy-highlight-face ((t (:foreground "dark blue" :bold t))))
     (font-lock-comment-face ((t (:foreground "dark green" :bold t :italic t))))
     (font-lock-string-face ((t (:foreground "SlateGray4" :bold t))))
     (font-lock-keyword-face ((t (:foreground "firebrick4" :bold t))))
     (font-lock-builtin-face ((t (:bold t :foreground "black"))))
     (font-lock-function-name-face ((t (:foreground "dark blue" :bold t))))
     (font-lock-variable-name-face ((t (:foreground "black"))))
     (font-lock-type-face ((t (:foreground "blue"))))
     (font-lock-constant-face ((t (:foreground "dark blue"))))
     (font-lock-warning-face ((t (:foreground "red" :bold t))))
     (widget-documentation-face ((t (:foreground "dark green"))))
     (widget-button-face ((t (:bold t))))
     (widget-field-face ((t (:background "gray85"))))
     (widget-single-line-field-face ((t (:background "gray85"))))
     (widget-inactive-face ((t (:foreground "dim gray"))))
     (widget-button-pressed-face ((t (:foreground "red"))))
     (custom-invalid-face ((t (:foreground "yellow" :background "red"))))
     (custom-rogue-face ((t (:foreground "pink" :background "black"))))
     (custom-modified-face ((t (:foreground "white" :background "blue"))))
     (custom-set-face ((t (:foreground "blue" :background "white"))))
     (custom-changed-face ((t (:foreground "white" :background "blue"))))
     (custom-saved-face ((t (:underline t))))
     (custom-button-face ((t (nil))))
     (custom-documentation-face ((t (nil))))
     (custom-state-face ((t (:foreground "dark green"))))
     (custom-variable-tag-face ((t (:foreground "blue" :underline t))))
     (custom-variable-button-face ((t (:bold t :underline t))))
     (custom-face-tag-face ((t (:underline t))))
     (custom-group-tag-face-1 ((t (:foreground "red" :underline t))))
     (custom-group-tag-face ((t (:foreground "blue" :underline t))))
     (speedbar-button-face ((t (:foreground "green4"))))
     (speedbar-file-face ((t (:foreground "cyan4"))))
     (speedbar-directory-face ((t (:foreground "blue4"))))
     (speedbar-tag-face ((t (:foreground "brown"))))
     (speedbar-selected-face ((t (:foreground "red"))))
     (speedbar-highlight-face ((t (:background "green"))))
     (ff-paths-non-existant-file-face ((t (:foreground "NavyBlue" :bold t))))
     (show-paren-match-face ((t (:background "light blue"))))
     (show-paren-mismatch-face ((t (:foreground "white" :background "purple")))))))

(ignore-errors
  (color-theme-billc))

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

;; ILISP/Inferior Lisp hook customizations
(add-hook 'lisp-mode-hook
	  (lambda ()
	    (imenu-add-to-menubar "Symbols")
	    (outline-minor-mode)
	    (make-local-variable 'outline-regexp)
	    (setq outline-regexp "^(.*")
	    (ignore-errors (semantic-default-elisp-setup))
	    (set (make-local-variable lisp-indent-function)
		 'common-lisp-indent-function)))

(add-hook 'ilisp-load-hook
	  '(lambda ()
	     (defkey-ilisp [(control c) (e)] 'eval-in-lisp)
	     (defkey-ilisp [(control c) (\;)] 'insert-balanced-comments)
	     (defkey-ilisp [(control c) (:)] 'remove-balanced-comments)
	     (defkey-ilisp [(control c) (x)] 'copy-eval-dwim-lisp)
	     ;; Set the inferior Lisp directory to the directory of
	     ;; the buffer that spawned it on the first prompt.
	     (add-hook 'ilisp-init-hook
		       '(lambda ()
			  (default-directory-lisp ilisp-last-buffer)))))

(defun my-setup-ilisp ()
  "Set up common variables used by ilisp."
  (interactive)
  (setq ilisp-*use-fsf-compliant-keybindings* t
	ilisp-*arglist-message-lisp-space-p* t
	ilisp-print-info-message-command t
	lisp-no-popper t)
  (require 'completer)
  (require 'ilisp)
  ;; Fix clisp interaction buffer (Windows)
  (modify-coding-system-alist 'process "lisp" 'unix)
  ;; All the *.d and *.lisp sources are in UTF-8 encoding.
  (modify-coding-system-alist 'file "\\.\\(d\\|lisp\\)\\'" 'utf-8))

;; ELI hook customizations
(add-hook 'fi:common-lisp-mode-hook
	  (lambda ()
	    (outline-minor-mode)
	    (make-local-variable 'outline-regexp)
	    (setq outline-regexp "^(.*")
	    (ignore-errors (semantic-default-elisp-setup))
	    (set (make-local-variable lisp-indent-function)
		 'common-lisp-indent-function)))

(add-hook 'fi:lisp-listener-mode-hook
	  (lambda ()
	    (let ((map (current-local-map)))
	      (define-key map [tab] 'comint-dynamic-complete)
	      (define-key map [(meta p)] 'fi:pop-input)
	      (define-key map [(meta n)] 'fi:push-input))))

(add-hook 'fi:lisp-mode-hook
	  (function
	   (lambda ()
	     (let ((map (current-local-map)))
	       (define-key map [(control c) (d)] 'eli-lisp-eval-or-compile-dwim)
	       (define-key map [(control c) (e)] 'eval-in-lisp)
	       (define-key map [(control c) (\;)] 'insert-balanced-comments)
	       (define-key map [(control c) (:)] 'remove-balanced-comments)
	       (define-key map [(control c) (x)] 'copy-eval-dwim-lisp)))))

(defun start-lisp ()
  "Start up the Lisp implementation that was chosen."
  (interactive)
  (cond
    ((or (eq lisp-used :clisp-ilisp)
	 (eq lisp-used :openmcl-ilisp)
	 (eq lisp-used :sbcl-ilisp)
	 (eq lisp-used :lw-ilisp))
     (my-setup-ilisp)
     (case lisp-used
       (:clisp-ilisp (clisp-hs))
       (:openmcl-ilisp (openmcl))
       (:sbcl-ilisp (sbcl))
       (:lw-ilisp (lispworks))))

    ((eq lisp-used :acl-eli)
     (load "fi-site-init")
     (fi:common-lisp fi:common-lisp-buffer-name
		     fi:common-lisp-directory
		     fi:common-lisp-image-name
		     fi:common-lisp-image-arguments
		     fi:common-lisp-host))

    ((eq lisp-used :clisp-inf)
     (run-lisp clisp-hs-program))

    ((eq lisp-used :corman-inf)
     (run-lisp cormanlisp-program))))

;;__________________________________________________________________________
;;;;    Programming - Common Lisp (functions not provided by CL modes)

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t nil)))

(defun eval-in-lisp (arg)
  "Evaluate an sexp and display the result (but not output) in minibuffer."
  (interactive "sEval (in Lisp): ")
  (message "%s" (cond
		 ((or (eq lisp-used :clisp-ilisp)
		      (eq lisp-used :openmcl-ilisp)
		      (eq lisp-used :sbcl-ilisp)
		      (eq lisp-used :lw-ilisp)
		      (eq lisp-used :plt-ilisp)
		      (eq lisp-used :acl-ilisp))
		  (let ((result (ilisp-send arg)))
		    (cond ((equal result "T")   (setf result t))
			  ((equal result "NIL") (setf result nil))
			  (t (setf result (read result))))
		    result))
		 ((eq lisp-used :acl-eli)
		  (fi:eval-in-lisp arg))
		 ((eq lisp-used :corman-inf)
		  (nil)))))

(defun eli-lisp-eval-or-compile-dwim (compilep)
  "Send the appropriate forms to the Lisp subprocess associated with
this buffer (Do What I Mean).  If a region is selected, evaluate the
region.  If the cursor is on or immediately after a ')', evaluate the
last sexp.  If the cursor is on or immediately before a '(', evaluate
the next sexp. If the cursor is inside a defun, evaluate the defun. If
the cursor is inside a top-level sexp, evaluate the top-level
sexp. Tests are done in the order specified in these comments, so if
there is any ambiguity, make certain that the cursor is either on a
parenthesis (for the eval last/next commands or not directly
before/after/on a parenthesis for the eval defun/top-level commands.
See the documentation for fi:lisp-evals-always-compile."
  (interactive (fi::decode-prefix-argument-for-eval-or-compile))
  (save-excursion
    (cond 
      ;;Region selected - evaluate region
      ((not (equal mark-active nil))
       (fi:lisp-eval-or-compile-region compilep))
      ;; At/after sexp - evaluate last sexp
      ((or (looking-at "\\s\)")
	   (save-excursion
	     (backward-char 1)
	     (looking-at "\\s\)")))
       (if (looking-at "\\s\)")
	   (forward-char 1)) 
       (fi:lisp-eval-or-compile-last-sexp compilep))
      ;; At/before sexp - evaluate next sexp
      ((or (looking-at "\\s\(")
	   (save-excursion
	     (forward-char 1)
	     (looking-at "\\s\(")))
       (if (looking-at "\\s\(")
	   (forward-list 1)) 
       (fi:lisp-eval-or-compile-last-sexp compilep))
      ;; Default - evaluate enclosing defun/sexp
      (t (fi:lisp-eval-or-compile-defun compilep)))))

(defun copy-eval-dwim-lisp ()
  "Copy from a source buffer and evaluate DWIM (Do What I Mean) in the
Lisp listener.  If a region is selected, evaluate the region.  If the
cursor is on or immediately after a ')', evaluate the last sexp.  If
the cursor is on or immediately before a '(', evaluate the next
sexp. If the cursor is inside a defun, evaluate the defun. If the
cursor is inside a top-level sexp, evaluate the top-level sexp. Tests
are done in the order specified in these comments, so if there is any
ambiguity, make certain that the cursor is either directly on a
parenthesis (for the eval last/next commands) or not directly
before/after/on a parenthesis (for the eval defun/top-level
commands)."
  (interactive)
  (save-excursion
    (cond 
     ;;Region selected - evaluate region
     ((not (equal mark-active nil))
      (cond ((and (or (equal mode-name "Lisp")
		      (equal mode-name "Info"))
		  (boundp 'ilisp-buffer)
		  (member* ilisp-buffer (buffer-list)
			   :key #'buffer-name
			   :test #'equal))
	     (eval-region-lisp (mark) (point)))
	    ((and (or (equal mode-name "Common Lisp")
		      (equal mode-name "Info"))
		  (boundp 'fi:common-lisp-buffer-name)
		  (member* fi:common-lisp-buffer-name (buffer-list)
			   :key #'buffer-name
			   :test #'equal))
	     (fi:lisp-eval-or-compile-region nil))
	    ((and (or (equal mode-name "Emacs-Lisp")
		      (equal mode-name "Emacs Lisp")
		      (equal mode-name "Info"))
		  (member* "*scratch*" (buffer-list)
			   :key #'buffer-name
			   :test #'equal))
	     (eval-region (mark) (point))))) 
     ;; At/after sexp - evaluate last sexp
     ((or (looking-at "\\s\)")
	  (save-excursion
	    (backward-char 1)
	    (looking-at "\\s\)")))
      (if (looking-at "\\s\)")
	  (forward-char 1)) 
      (copy-eval-last-sexp))
     ;; At/before sexp - evaluate next sexp
     ((or (looking-at "\\s\(")
	  (save-excursion
	    (forward-char 1)
	    (looking-at "\\s\(")))
      (if (looking-at "\\s\(")
	  (forward-list 1)) 
      (copy-eval-last-sexp))
     ;; Default - evaluate enclosing defun/sexp
     (t (end-of-defun)
	(backward-char 1)
	(copy-eval-last-sexp)))))

(defun copy-eval-last-sexp ()
  "Evaluate the last s-expression in the buffer in the Lisp listener."
  (interactive)
  (let ((end (point))
	(beg (save-excursion
	       (backward-list 1)
	       (point)))
	(edit-buffer (current-buffer))
	(lisp-buffer nil)
	(eval-command nil))
    (cond ((and (or (equal mode-name "Lisp")
		    (equal mode-name "Info"))
		(boundp 'ilisp-buffer)
		(member* ilisp-buffer (buffer-list)
			 :key #'buffer-name
			 :test #'equal)
		(get-buffer-process (get-buffer ilisp-buffer)))
	   (setq lisp-buffer (get-buffer ilisp-buffer)
		 eval-command 'return-ilisp))
	  ((and (or (equal mode-name "Common Lisp")
		    (equal mode-name "Info"))
		(boundp 'fi:common-lisp-buffer-name)
		(member* fi:common-lisp-buffer-name (buffer-list)
			 :key #'buffer-name
			 :test #'equal)
		(get-buffer-process (get-buffer fi:common-lisp-buffer-name)))
	   (setq lisp-buffer (get-buffer fi:common-lisp-buffer-name)
		 eval-command 'fi:inferior-lisp-newline))
	  ((and (or (equal mode-name "Emacs-Lisp")
		    (equal mode-name "Emacs Lisp")
		    (equal mode-name "Info"))
		(member* "*scratch*" (buffer-list)
			 :key #'buffer-name
			 :test #'equal))
	   (setq lisp-buffer "*scratch*"
		 eval-command 'eval-print-last-sexp))
	  (t nil))
    (if eval-command
	(progn
	  (pop-to-buffer lisp-buffer)
	  (end-of-buffer)
	  (other-window 1)
	  (append-to-buffer lisp-buffer beg end)
	  (pop-to-buffer lisp-buffer)
	  (funcall eval-command)
	  (other-window 1)))))

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
;;;;    Programming - Elisp

(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (interactive)
	     (require 'eldoc)
	     (turn-on-eldoc-mode)
	     (define-key emacs-lisp-mode-map [(control c) (x)] 'copy-eval-dwim-lisp)
	     ;; Default to auto-indent on Enter
	     (define-key emacs-lisp-mode-map [(control j)] 'newline)
	     (define-key emacs-lisp-mode-map [(control m)] 'newline-and-indent)))

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

;; Control which Lisp implementation is selected
(global-set-key [f5] 'start-lisp)

(global-set-key [(control meta f5)]
		'(lambda ()
		   (interactive)
		   (let ((lisp-number (+ 1 (position lisp-used lisp-implementations))))
		     (if (> lisp-number (- (length lisp-implementations) 1))
			 (setq lisp-used (elt lisp-implementations 0))
		       (setq lisp-used (elt lisp-implementations lisp-number))))
		   (message "lisp-used: %s" lisp-used)))

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

;; Close down Lisp before killing buffer
(global-set-key [f4]
		'(lambda ()
		   (interactive)
		   (cond
		    ((eq (current-buffer) (get-buffer "*scheme*"))
		     (let ((process (get-buffer "*scheme*")))
		       (comint-snapshot-last-prompt)
		       (process-send-string process "(exit)"))
		     (sleep-for .1)
		     (kill-this-buffer))
		    ((eq (current-buffer) (get-buffer "*clisp-hs*"))
		     (insert "(quit)")
		     (return-ilisp)
		     (sleep-for .1)
		     (kill-this-buffer))
		    ((eq (current-buffer) (get-buffer "*openmcl*"))
		     (insert "(quit)")
		     (return-ilisp)
		     (sleep-for .1)
		     (kill-this-buffer))
		    ((eq (current-buffer) (get-buffer "*sbcl*"))
		     (insert "(quit)")
		     (return-ilisp)
		     (sleep-for .1)
		     (kill-this-buffer))
		    ((eq (current-buffer) (get-buffer "*common-lisp*"))
		     (fi:exit-lisp))
		    (t (kill-this-buffer)))))

;; Common buffer/window control shortcuts
(global-set-key [f6] 'other-window)
(global-set-key [f7] 'delete-other-windows)
(global-set-key [(control f7)] 'ecb-toggle-ecb-windows)
(global-set-key [(meta f7)] 'ecb-toggle-layout)

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

;;__________________________________________________________________________
;;;;    Start Directory

(find-file "~/")

;; end of emacs.el