#set document(
    title: [the Common Lisp Cookbook],
    date: auto,
    author: "collective",
    keywords: ("programming", "lisp", "common lisp", "free"),
    description: [A code-first tutorial and language reference for Common Lisp],
)

#show heading.where(level: 1, outlined: true): it => {
	pagebreak()
	it
}

#image("orly-cover.png", width: 90%),

#pagebreak(
    to: "odd"
)

#set heading(numbering: "1.")
#show link: underline

#set page(numbering: "1")
#counter(page).update(1)
