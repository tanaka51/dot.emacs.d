octomacs.el provides an interface for interacting with Octopress
(http://octopress.org/).

Add the following to your .emacs:

    (add-to-list 'load-path "/path/to/octomacs")
    (require 'octomacs)

Configuring Octomacs will make interaction a little nicer.
Specifically, setting `octomacs-workdir-alist'.

    M-x customize-group RET octomacs RET

If rvm.el is installed, Octomacs will attempt to use it whenever it
needs to run a command in the Octopress directory.

If ido.el is installed, Octomacs will use it for prompting of which
Octopress project to use.

Calling the interactive function `octomacs-new-post' will prompt
for which project to use (configurable from
`octomacs-workdir-alist', or can be manually specified as a path to
an Octopress instance), and for the title of the post to create.
