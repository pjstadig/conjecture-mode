## Conjecture Mode

This source repository includes `conjecture-mode.el`, which
provides support for running Clojure tests (using the `conjecture.core`
framework) via nrepl.el and seeing feedback in the test buffer about
which tests failed or errored. The installation instructions above
should work for conjecture-mode as well.

Once you have a repl session active, you can run the tests in the
current buffer with <kbd>C-c C-,</kbd>. Failing tests and errors will be
highlighted using overlays. To clear the overlays, use <kbd>C-c k</kbd>.

You can jump between implementation and test files with <kbd>C-c C-t</kbd> if
your project is laid out in a way that conjecture-mode expects. Your project
root should have a `src/` directory containing files that correspond to their
namespace. It should also have a `test/` directory containing files that
correspond to their namespace, and the test namespaces should mirror the
implementation namespaces with the addition of "-test" as the suffix to the last
segment of the namespace.

So `my.project.frob` would be found in `src/my/project/frob.clj` and
its tests would be in `test/my/project/frob_test.clj` in the
`my.project.frob-test` namespace.

This test-to-source mapping can also be configured using the
`conjecture-jump-to-implementation-fn`.

## License

Copyright © 2013 Paul Stadig.

Forked from clojure-test-mode.

clojure-test-mode Copyright © 2007-2013 Jeffrey Chu, Lennart Staflin, Phil
Hagelberg, and [contributors](https://github.com/technomancy/clojure-mode/contributors).

Distributed under the GNU General Public License; type <kbd>C-h C-c</kbd> to view it.
