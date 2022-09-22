# Lisp Cookbook

A Cookbook is an invaluable resource, as it shows how to do various things in a clear fashion without all the theoretical context. Sometimes you just need to look things up. While cookbooks can never replace proper documentation such as the [HyperSpec][hs] or books such as [Practical Common Lisp][pcl], every language deserves a good cookbook, Common Lisp included.

The CL Cookbook aims to tackle all sort of topics, for the beginner as for the more advanced developer.


## Contributing

Thanks for contributing to the Cookbook.

You can start by having a look at the [style guide](STYLEGUIDE.md).

When adding new content, please ensure it renders properly.

There are two ways to do this:

The first option is to install [Jekyll][jekyll] globally and to run `jekyll serve` in a folder where this repository was checked out.

Then open `http://127.0.0.1:4000/cl-cookbook/` (the last `/` is important).

Another option is to install the Jekyll version of this repository locally with Ruby gems. Since bundler 1.17.3 requires Ruby 2.5 that is rather old, it is recommended to install it using rbenv:

1. Install [rbenv](https://github.com/rbenv/rbenv) using your package manager, or follow [these instructions](https://github.com/rbenv/rbenv#basic-github-checkout) to install it manually.
2. Install [ruby-build](https://github.com/rbenv/ruby-build#installation). If you did a manual installation in the previous step, it is recommended to install ruby-build as a rbenv plugin.
3. Run `rbenv install 2.5.0` to install Ruby 2.5.0. Run `which gem` to make sure it points to `~/.rbenv/shims/gem`.
4. Run gem install bundler -v `2.1.4` to install bundler.
5. `cd` to the `cl-cookbook` directory and run `bundle install --path vendor/bundle` to install Jekyll locally.
6. Run `bundle exec jekyll serve` to generate the site and host it.

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

Run `make epub`. See `make-cookbook.lisp`.

You need a decently recent version of [Calibre](https://calibre-ebook.com/). They provide an easy binary installation.

To exclude regions of text of the build (for example, embedded videos that makes no sense in a print format), use these flags:

    <!-- epub-exclude-start -->
    <!-- epub-exclude-end -->

Our build script roughly does the following:

- concatenate all markdown content into one file
- change yaml frontmatters to a markdown title
- delete the mark regions from the file
- make internal links work on the EPUB.

It uses some metadata in `metadata.txt`.

We can check the resulting EPUB with `epubcheck`.


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
