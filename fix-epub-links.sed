# sed script to change internal links in markdown file,
# to links pandoc can use when generating EPUB from markdown.
# Currently, the transformed links do not work in the web server,
# so the script should change a file that is not used in the web server.
# Examples of how links are transformed:
# - In error_handling.md:
#   "[debugging section](debugging.html)" to "[debugging section][Debugging]"
#   It is expected that there is a section header with the title "Debugging".
# - In data-structures.md:
#   "[strings](strings.html)" to "[strings](#strings)"
#   pandoc associates each section header with a unique key, here "strings".
#   We specify the target header represented by "key", using the syntax (#key).
#   This has to be used when the taget syntax [section header] is ambiguous.
# Usage:
#   sed -i -f fix-epub-links.sed full.md
# Note:
#   The links with format like (#numbers), are there for a reason.
#   The (#foo) format is used when the [foo] format is ambiguous.
#
# arrays.md
s/\(\[\(Arrays and vectors\)\]\)(data-structures.html)/\1[\2]/
# databases.md
s/\(\[CLOS\]\)(clos.html)/\1[Fundamentals of CLOS]/
s/\(\[clos\]\)(clos.html)/\1[Fundamentals of CLOS]/
# data-structures.md
s/\(\[pattern matching\]\)(pattern_matching.html)/\1[Pattern Matching]/
s/\(\[strings\]\)(strings.html)/\1(#strings)/
s/\(\[CLOS section\]\)(clos.html)/\1[Fundamentals of CLOS]/
# debugging.md
s/\(\[error handling\]\)(error_handling.html)/\1[Error and exception handling]/
s/\(\[testing\]\)(testing.html)/\1[Testing the code]/
# dynamic-libraries.md
s/\(\[\(foreign function interface\)\]\)(ffi.html)/\1[Foreign Function Interfaces]/
# editor-support.md
s/\(\["\(Using Emacs as an IDE\)"\]\)(emacs-ide.html)/\1[\2]/
s/\(\[\(Using VSCode with Alive\)\]\)(vscode-alive.html)/\1[\2]/
s/\(\[read our LispWorks review here\]\)(lispworks.html)/\1[LispWorks review]/
# error_handling.md
s/\(\[debugging section\]\)(debugging.html)/\1[Debugging]/
# getting-started.md
s/\(\[editor-support\]\)(editor-support.html)/\1[Editor support]/
# index.md
s/\(\[\(License\)\]\)(license.html)/\1[\2]/
s/\(\[Getting started\]\)(getting-started.html)/\1[Getting started with Common Lisp]/
s/\(\[\(Editor support\)\]\)(editor-support.html)/\1[\2]/
s/\(\[\(Using Emacs as an IDE\)\]\)(emacs-ide.html)/\1[\2]/
s/\(\[The LispWorks IDE\]\)(lispworks.html)/\1[LispWorks review]/
s/\(\[Functions\]\)(functions.html)/\1(#functions)/
s/\(\[Data Structures\]\)(data-structures.html)/\1[Data structures]/
s/\(\[Strings\]\)(strings.html)/\1(#strings)/
s/\(\[\(Regular Expressions\)\]\)(regexp.html)/\1[\2]/
s/\(\[Numbers\]\)(numbers.html)/\1(#numbers)/
s/\(\[Loops, iteration, mapping\]\)(iteration.html)/\1[Loop, iteration, mapping]/
s/\(\[Multidimensional Arrays\]\)(arrays.html)/\1[Multidimensional arrays]/
s/\(\[\(Dates and Times\)\]\)(dates_and_times.html)/\1[\2]/
s/\(\[\(Pattern Matching\)\]\)(pattern_matching.html)/\1[\2]/
s/\(\[\(Input\/Output\)\]\)(io.html)/\1[\2]/
s/\(\[\(Files and Directories\)\]\)(files.html)/\1[\2]/
s/\(\[Error and condition handling\]\)(error_handling.html)/\1[Error and exception handling]/
s/\(\[\(Packages\)\]\)(packages.html)/\1[\2]/
s/\(\[Macros and Backquote\]\)(macros.html)/\1[Macros]/
s/\(\[CLOS (the Common Lisp Object System)\]\)(clos.html)/\1[Fundamentals of CLOS]/
s/\(\[\(Type System\)\]\)(type.html)/\1[\2]/
s/\(\[Sockets\]\)(sockets.html)/\1[TCP\/UDP programming with sockets]/
s/\(\[\(Interfacing with your OS\)\]\)(os.html)/\1[\2]/
s/\(\[\(Foreign Function Interfaces\)\]\)(ffi.html)/\1[\2]/
s/\(\[\(Building Dynamic Libraries\)\]\)(dynamic-libraries.html)/\1[\2]/
s/\(\[\(Threads\)\]\)(process.html)/\1[\2]/
s/\(\[\(Defining Systems\)\]\)(systems.html)/\1[\2]/
s/\(\[\(Using the Win32 API\)\]\)(win32.html)/\1[\2]/
s/\(\[\(Debugging\)\]\)(debugging.html)/\1[\2]/
s/\(\[Performance Tuning\]\)(performance.html)/\1[Performance Tuning and Tips]/
s/\(\[Scripting. Building executables\]\)(scripting.html)/\1[Scripting. Command line arguments. Executables.]/
s/\(\[Testing and Continuous Integration\]\)(testing.html)/\1[Testing the code]/
s/\(\[Databases\]\)(databases.html)/\1[Database Access and Persistence]/
s/\(\[GUI programming\]\)(gui.html)/\1[GUI toolkits]/
s/\(\[\(Web development\)\]\)(web.html)/\1[\2]/
s/\(\[\(Web Scraping\)\]\)(web-scraping.html)/\1[\2]/
s/\(\[\(WebSockets\)\]\)(websockets.html)/\1[\2]/
s/\(\[\(Miscellaneous\)\]\)(misc.html)/\1[\2]/
# iteration.md
s/\(\[data-structures chapter\]\)(data-structures.html)/\1[Data structures]/
# scripting.md
s/\(\[error and condition handling\]\)(error_handling.html)/\1[Error and exception handling]/
# strings.md
s/\(\[regexp\]\)(regexp.html)/\1[Regular Expressions]/
# web.md
s/\(\[databases section\]\)(databases.html)/\1[Database Access and Persistence]/
