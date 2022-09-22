Thanks for contributing to the Cookbook. Please follow these
guidelines. Some are only a convention, some are important for epub
generation.

## Titles

- (important) the first section in a chapter should be a subsection: `## title` (indeed, the section name in the epub is the chapter name).

- titles are like a sentence, only the first word is capitalized.

- if you refer to functions, use markdown syntax too (backtics).

## Lists

- numbered bullet lists (`1.`) should be used only at the first level. For the next nested levels, use normal non-numbered lists.


## Code formatting

- functions should generally be referenced with backtics. There is no need to capitalize them in the age of markdown: write `function` instead of FUNCTION.

## Code snippets

- use `~~~lisp` for code snippets.

- snippets must start at the beginning of a line. Don't indent the `~~~lisp` code fence.

- code snippets must be preceded and followed by a newline:

```

Here's a snippet:

~~~lisp
(defun oh ())
~~~

This snippet...
```

- to show a snippet's result, use `;; => result` on the same line if the snippet was on one line, otherwise you can do:

~~~lisp
(print :abc)
;; :abc
~~~

If the result is large, use another code block, without comments.

## EPUB gotchas

### Embedded content

We should not abuse from embedded content, such as youtube videos.

Please exclude them from the EPUB generation with these flags:

    <!-- epub-exclude-start -->
    <!-- epub-exclude-end -->

### Internal links (for the EPUB)

We must tweak the markdown links so that internal links work in the
EPUB reader. See `fix-epub-links.sed` to add your link or just ping us on GitHub.
