How to contribute
=================
If you want to contribute, please fork the [main repo][main repo].
After release 1.0, we decided to use a [nice branching workflow][workflow]
for this project.
Please take a look if you are not familiar with it, but long story short,
fork your branch from `develop`, and make sure you update `develop` before
trying to merge back into it.

Tests and cabal flags
---------------------
For this project we use the standard hunit/quickcheck tests.
If you want to submit a new feature, please try to integrate at least
*some* tests.

You can remove the `-Werror` flag for development, but please remind that
this flag will appear in the release, so we won't be able to ship your
feature if there are warnings --- the pull requests being automatically
built and tested by travis-ci.

Code style
----------
We do not impose any specific style of code, but you can improve your code
by running it through `hlint`.


Finally, feel free to submit issues.


[main repo]: http://github.com/divarvel/hammertime

[workflow]: http://nvie.com/posts/a-successful-git-branching-model
