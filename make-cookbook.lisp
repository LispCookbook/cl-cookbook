
;;
;; pandoc from markdown to epub
;; typst from markdown to Typst to pdf
;;
;; Usage:
;;
;; make epub
;; make pdf
;; make epub+pdf
;;
;; Metadata is in metadata.txt
;; -> change the date

(require 'asdf)

(defparameter chapters
  (list
   "index.md"
   "license.md"
   "foreword.md"
   "getting-started.md"
   "editor-support.md"
   "emacs-ide.md"
   "vscode-alive.md"
   "lispworks.md"
   "variables.md"
   "functions.md"
   "data-structures.md"
   "strings.md"
   "numbers.md"
   "iteration.md"
   "arrays.md"
   "dates_and_times.md"
   "pattern_matching.md"
   "regexp.md"
   "io.md"
   "files.md"
   "error_handling.md"
   "packages.md"
   "macros.md"
   "clos.md"
   "type.md"
   "sockets.md"
   "os.md"
   "ffi.md"
   "dynamic-libraries.md"
   "process.md"
   "systems.md"
   ;; "win32.md" ; Excluded because: Out of date
   "debugging.md"
   "performance.md"
   "scripting.md"
   "testing.md"
   "databases.md"
   "gui.md"
   "web.md"
   "web-scraping.md"
   "websockets.md"
   ;; "misc.md" ; Excluded because: Lack of relevant content
   ;; "awesome-cl.md"
   "contributors.md"
   ))

(defparameter *typst-preamble* "typst-preamble.typ"
  "Typst declarations and settings to which we add the full typ document.")

(defparameter *sed-command* (uiop:getenv "SED_CMD"))
(defparameter *full-markdown* "full.md")
(defparameter *bookname* "common-lisp-cookbook.epub")
(defparameter *epub-command-placeholder* "pandoc -o ~a --toc metadata.txt ~a"
  "format with book name and sources file.")

(defun reset-target ()
  (uiop:run-program (format nil "echo > ~a" *full-markdown*)))

(defun full-editing ()
  "Transform markdown frontmatters to a title, etc."
  (format t "Edit the markdown...~&")
  (uiop:run-program (format nil "~a -i \"s/title:/# /g\" ~a" *sed-command* *full-markdown*))
  (uiop:run-program (format nil "~a -i \"/^---/s/---/ /g\" ~a" *sed-command* *full-markdown*))
  ;; Exclude regions that don't export correctly, like embedded videos.
  (uiop:run-program (format nil
                            "~a -i \"/<\!-- epub-exclude-start -->/,/<\!-- epub-exclude-end -->/d\" ~a"
                            *sed-command*
                            *full-markdown*))
  ;; Make internal links work in the generated EPUB.
  (uiop:run-program (format nil "~a -i -f fix-epub-links.sed ~a" *sed-command* *full-markdown*)))

(defun to-epub ()
  (format t "~&Generating ~a...~&" *bookname*)
  (uiop:run-program (format nil *epub-command-placeholder* *bookname* *full-markdown*)))

(defun sample-pdf ()
  (format t "~&Generating a very short PDF sample.~&")
  (uiop:run-program (format nil "typst compile ~a" *typst-preamble*)))

(defun to-pdf ()
  "Needs pandoc >= 3.8 with Markdown to Typst conversion,
  and the typst binary on the path."
  (format t "~&Generating the pdf with pandoc >= 3.8 and Typst...~&")

  ;; Transform our md file to .typ:
  (uiop:run-program (format nil "pandoc -o full.typ ~a" *full-markdown*)
                    :output t
                    :error-output t)

  ;; Add typst configuration:
  (uiop:run-program (format nil "cat ~a >> full-with-preamble.typ && cat full.typ >> full-with-preamble.typ" *typst-preamble*)
                    :output t
                    :error-output t)

  ;; Compile the Typst document:
  (uiop:run-program "typst compile full-with-preamble.typ" :output t :error-output t)
  ; todo utiliser min-book?
  (uiop:run-program "mv full-with-preamble.pdf common-lisp-cookbook.pdf"))

(defun build-full-source ()
  (format t "Creating the full source into ~a...~&" *full-markdown*)
  (loop for chap in chapters
     for cmd = (format nil "cat ~a >> ~a" chap *full-markdown*)
     do (uiop:run-program cmd))
  (full-editing))

(defun generate ()
  (reset-target)
  (build-full-source))
