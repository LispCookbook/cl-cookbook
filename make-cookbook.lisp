
;;
;; pandoc to epub
;; calibre from epub to pdf
;;
;; To generate the EPUB, load this file and call
;; (generate)
;;
;; Metadata is in metadata.txt

(require 'asdf)

(defparameter chapters
  (list
   "foreword.md"
   "getting-started.md"
   "editor-support.md"
   "emacs-ide.md"
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
   "process.md"
   "systems.md"
   ;; "win32.md"
   "debugging.md"
   "performance.md"
   "scripting.md"
   "testing.md"
   "databases.md"
   "gui.md"
   "web.md"
   "web-scraping.md"
   "websockets.md"
   ;; "misc.md"
   ;; "awesome-cl.md"
   "contributors.md"
   ))

(defparameter *full-markdown* "full.md")
(defparameter *bookname* "common-lisp-cookbook.epub")
(defparameter *epub-command-placeholder* "pandoc -S -o ~a --toc metadata.txt ~a"
  "format with book name and sources file.")

(defun reset-target ()
  (uiop:run-program (format nil "echo > ~a" *full-markdown*)))

(defun full-editing ()
  "Transform markdown frontmatters to a title, etc."
  (format t "Edit the markdown...~&")
  (uiop:run-program (format nil "sed -i \"s/title:/# /g\" ~a" *full-markdown*))
  (uiop:run-program (format nil "sed -i \"s/---/ /g\" ~a" *full-markdown*))
  ;; Exclude regions that don't export correctly, like embedded videos.
  (uiop:run-program (format nil "sed -i \"/<\!-- epub-exclude-start -->/,/<\!-- epub-exclude-end -->/d\" ~a" *full-markdown*))
  )

(defun to-epub ()
  (format t "~&Generating ~a...~&" *bookname*)
  (uiop:run-program (format nil *epub-command-placeholder* *bookname* *full-markdown*)))

(defun to-pdf ()
  "Needs calibre."
  (format t "~&Generating the pdf...~&")
  (uiop:run-program (format nil "ebook-convert ~a common-lisp-cookbook.pdf" *bookname*)))

(defun build-full-source ()
  (format t "Creating the full source into ~a...~&" *full-markdown*)
  (loop for chap in chapters
     for cmd = (format nil "cat ~a >> ~a" chap *full-markdown*)
     do (uiop:run-program cmd))
  (full-editing))

(defun generate ()
  (reset-target)
  (build-full-source)
  (to-epub)
  (to-pdf))

(generate)
