# Lisp Cookbook

A Cookbook is an invaluable resource, as it shows how to do various things in a clear fashion without all the theoretical context. Sometimes you just need to look things up. While cookbooks can never replace proper documentation such as the [HyperSpec][hs] or books such as [Practical Common Lisp][pcl], every language deserves a good cookbook, Common Lisp included.

The CL Cookbook aims to tackle all sort of topics, for the beginner as for the more advanced developer.


## Contributing

When adding new content, ensure it renders properly.

There are two ways to do this:

First option is to install [Jekyll][jekyll] and run `jekyll serve` in a folder where this repository was checked out.

Another option is to use github gem provided with the repo. To do this do the following:

1. Ensure that bundler is installed (`gem install bundler` should do; in case that doesn't work, try `gem install bundler -v '1.17.3'` as per [this SO page][bundler-v2].)
2. `bundle install --path vendor/bundle`
3. `bundle exec jekyll serve`

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
