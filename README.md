
<br />
<p align="center">

  <h1 align="center">The Common Lisp Cookbook</h1>

  <p align="center">
    Code-first Common Lisp recipes.
	<br />
    <a href="https://lispcookbook.github.io/cl-cookbook/"><strong>Read online »</strong></a>
    <br />
    <br />
    <a href="https://github.com/LispCookbook/cl-cookbook/releases">News</a>
    ·
    <a href="https://github.com/LispCookbook/cl-cookbook/releases">EPUB and PDF</a>
    ·
    <a href="https://github.com/LispCookbook/cl-cookbook/issues?q=is%3Aissue%20state%3Aopen%20label%3A%22good%20first%20issue%22">Contribute</a>
  </p>
</p>

A Cookbook is an invaluable resource, as it shows how to do various things in a clear fashion without all the theoretical context. Sometimes you just need to look things up. While cookbooks can never replace proper documentation such as the [HyperSpec][hs] or books such as [Practical Common Lisp][pcl], every language deserves a good cookbook, Common Lisp included.

The CL Cookbook aims to tackle all sort of topics, for the beginner as for the more advanced developer.

<p align="center">
  <a href="https://lispcookbook.github.io/cl-cookbook/">
    <img src="https://lispcookbook.github.io/cl-cookbook/orly-cover.png" alt="Logo" max-width="150">
  </a>
</p>

## Contributing

Thanks for contributing to the Cookbook.

You can start by having a look at the [style guide](STYLEGUIDE.md).

When adding new content, please ensure it renders properly.

There are three ways to do this:

### Install Jekyll system-wide

The first option is to install [Jekyll][jekyll] globally and to run `jekyll serve` in a folder where this repository was checked out.

Then open `http://127.0.0.1:4000/cl-cookbook/` (the last `/` is important).

### Install system locally using Ruby gems

Another option is to install the Jekyll version of this repository locally with Ruby gems. Since bundler 1.17.3 requires Ruby 2.5 that is rather old, it is recommended to install it using `rbenv`:

1. Install [rbenv](https://github.com/rbenv/rbenv) using your package manager, or follow [these instructions](https://github.com/rbenv/rbenv#basic-github-checkout) to install it manually.
2. Install [ruby-build](https://github.com/rbenv/ruby-build#installation). If you did a manual installation in the previous step, it is recommended to install ruby-build as a rbenv plugin.
3. Run `rbenv install 2.5.0` to install Ruby 2.5.0. Run `which gem` to make sure it points to `~/.rbenv/shims/gem`.
4. Run `gem install bundler -v 2.1.4` to install bundler.
5. `cd` to the `cl-cookbook` directory and run `bundle install --path vendor/bundle` to install Jekyll locally.
6. Run `bundle exec jekyll serve` to generate the site and host it.

### Use a Docker container

Since it can be a bit troublesome to install older versions of Ruby onto newer Linux-based systems, another option is to use `docker`.

1. Build the container by executing `sudo docker build -t cl-cookbook .` in this directory.
2. Run Jekyll inside the container `sudo docker run -p 4000:4000 -v $(pwd):/cl-cookbook cl-cookbook` from this directory.
3. Open your web browser and navigate to `http://127.0.0.1:4000/cl-cookbook/`.

This command will mount the current working directory into the container and incremental builds are actived so you will be able to see your latest changes without restarting or rebuilding the container.

### Troubleshooting

It can happen that you have older version of ruby installed in the system and
bundler install will fail. To fix this, you need to update ruby. If system update
is not an option, consider installing [rbenv][rbenv].

~~~ sh
    # Check rbenv homepage for install instructions on systems other than Mac OS X
    brew install rbenv ruby-build

    # Add rbenv to bash so that it loads every time you open a terminal
    echo 'if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi' >> ~/.bash_profile
    source ~/.bash_profile

    # Install Ruby
    rbenv install 2.5.0
    rbenv global 2.5.0
    ruby -v
~~~

After this you can proceed as usual:

1. `gem install bundler`
2. `bundle install --path vendor/bundle`
3. `bundle exec jekyll serve`

Also, refer to the [CONTRIBUTING.md][contributing] file.

### Building the EPUB and the PDF

Run `make epub` to only build epub, `make pdf` to build the PDF.  Run `make epub+pdf` to build both. See `make-cookbook.lisp`.

For the epub, you need a decently recent version of [Calibre](https://calibre-ebook.com/). They provide an easy binary installation.

For the PDF, since 2025-10-01, you need [Typst](https://typst.app/) and [Pandoc >= 3.8](https://github.com/jgm/pandoc/releases/tag/3.8).

To exclude regions of text from the output (for example, embedded videos that makes no sense in a print format), use these flags:

    <!-- epub-exclude-start -->
    <!-- epub-exclude-end -->

Our build script roughly does the following:

- concatenate all markdown content into one file
- change yaml frontmatters to a markdown title
- delete the marked regions from the file
- make internal links work on the EPUB.
- for the PDF, transform the big .md file to a typst file with Pandoc, then run `typst compile` on it. This produces a quality PDF.

It uses some metadata in `metadata.txt`.

We can check the resulting EPUB with `epubcheck`.

#### Troubleshooting build on macOS

If you encounter the following error:

~~~
debugger invoked on a UIOP/RUN-PROGRAM:SUBPROCESS-ERROR in thread
#<THREAD tid=259 "main thread" RUNNING {1203FA81D3}>:
  Subprocess with command "sed -i \"s/title:/# /g\" full.md"
 exited with error code 1
~~~

It means that your `sed` program is non-GNU. Install GNU version using `homebrew`:

~~~sh
brew install gnu-sed
~~~

Now you have `gsed` in your PATH, you can build the book with the following command:

~~~sh
SED_CMD=gsed make epub
~~~

## Origins

This is a fork of the [Common Lisp Cookbook][sf], moved from SourceForge.

This project brings the Common Lisp Cookbook to this decade. Development of the original Common Lisp Cookbook in SourceForge halted in 2007. In the meantime, a lot has happened in the land of Common Lisp. Tools and implementations have been improving, and some have fallen out of favor. Most notably, Common Lisp users can now benefit from the [Quicklisp][ql] library manager.

The main goal is making the Cookbook more modern and more accessible in addition to updating and expanding the content.

[sf]: http://cl-cookbook.sourceforge.net/
[ql]: https://www.quicklisp.org/
[hs]: http://www.lispworks.com/documentation/HyperSpec/Front/X_Master.htm
[pcl]: http://www.gigamonkeys.com/book/
[jekyll]: https://jekyllrb.com/docs/installation/
[rbenv]: https://github.com/rbenv/rbenv
[contributing]: CONTRIBUTING.md
[bundler-v2]: https://stackoverflow.com/questions/54087856/cant-find-gem-bundler-0-a-with-executable-bundle-gemgemnotfoundexceptio

## Development with GNU Guix

To enter a development environment in which all the required software dependencies are made available, run `guix shell` in the project's root directory. The provided Guix [manifest file](manifest.scm) will be automatically sourced after giving [authorization](https://guix.gnu.org/manual/devel/en/html_node/Invoking-guix-shell.html) for Guix to do so.

## Support

You can support the individuals that constantly improve the Cookbook. See the Github Sponsors icon. Thanks for them!
