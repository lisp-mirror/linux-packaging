# linux-packaging

This library lets you distribute your Lisp applications by linux
packages for the following distributions:

- Debian
- Ubuntu
- Linux Mint
- (... and really any distribution using .deb packages)
- Arch Linux
- Manjaro Linux
- (... and really any distribution using .pkg packages)
- Fedora
- CentOS
- Red Hat
- (... and really any distribution using .rpm packages)

All you need to do is declaring this in your `.asd`:

```lisp
(defsystem "foo-deb"
  :defsystem-depends-on ("linux-packaging")
  :class "linux-packaging:deb"
  :depends-on ("foo")
  :package-name "foo"
  :package-version "1.0.0"
  :build-pathname "foo"
  :entry-point "foo:main")
```

Compile with `sbcl --eval '(asdf:make :foo-deb)'`, et voila.

What's even more cool? If your application relies on shared libraries,
such as libsqlite, `linux-packaging` will automatically detect that
and add the appropriate dependencies to your package. When your
package will be installed, the appropriate dependencies will be
installed alongside it, thanks to apt/dnf/yum/pacman/etc.

What's even more cool? If your application relies on CFFI grovel or
wrapper files, such as the one built by osicat, those files will be
statically embedded into your binary.

Both of those mean that you can write any Lisp application using CFFI,
and your built packages will be installable by anyone using those
distributions.

This is similar to what [Deploy](https://shinmera.github.io/deploy/)
is doing. This library is doing great work, by the way. But the
approach taken by `linux-packaging` is slightly different.

Deploy is essentially taking this approach:

- Dump an image of your Lisp application
- Find out the C libraries
- Copy everything into a folder
- Archive that folder

And the user has to somehow figure out how to "install" that archive.

`linux-packaging` is taking another approach:

- Statically link the C libraries that it can into the image of your
  Lisp application
- Detect the C shared libraries, and which linux package is providing
  them
- Build an installable package on any linux distribution

Installing your Lisp application then becomes a procedure that every
user of your distribution already knows.

This is an approach that plays more nicely with how distributions
typically want their packages to be: relying on shared libraries as
much as possible, so that updating the shared library doesn't mean
having to rebuild your application.

As opposed to Deploy, the downsides of `linux-packaging` are:

- You need to compile sbcl manually with the
  `--with-sb-linkable-runtime` option, as this is not a default
  option.
- You need to install [fpm](https://fpm.readthedocs.io/en/latest/) to
  build the package.
- You need to build the package for the distribution you need on said
  distribution. For example, if you want to distribute a .deb for
  Debian Buster, you need to build your .deb on a Debian Buster
  machine.
  
The last point can seem daunting, but is not actually so. See the
[`.gitlab-ci.yml`](.gitlab-ci.yml) file of this repository for a
couple of examples, but the gist of it is that Docker and friends make
it easy.

## Reference

This is a reference of all the available options that you can use your
.asd file, as well as their description.

#### class

`:class` is actually a default ASDF attribute. It lets you define
another class for the system definition. You need to pick one of
those:

- `linux-packaging:rpm`: if you want to build a .rpm package
- `linux-packaging:deb`: if you want to build a .deb package
- `linux-packaging:pacman`: if you want to build a .pkg package

#### author

`:author` is another default ASDF attribute, but it is used to define
the package's author/maintainer.

#### license

`:license` is another default ASDF attribute, used to define the
package's license as well.

#### package-name

Defines the linux package name. If not specified, the system name is
used. (In the example above, it would be `foo-deb`.)

#### package-version

Defines the version of the package. If not specified,
`linux-packaging` will try looking in the `VERSION` environment
variable, or fallback to `1.0.0`.

#### additional-files

If you want to provide files in addition to the Lisp image, such as
icons, documentation, etc, you can specify an alist of this kind:

```lisp
:additional-files (("file/path/during/build" . "folder/to/install/at/"))
```

## License

MIT License.
