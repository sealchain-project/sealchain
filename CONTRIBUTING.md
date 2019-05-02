# Contributors Guide

## Bug Reports

Please [open an issue](https://github.com/sealchain-project/sealchain/issues/new)
to report about found bugs in Sealchain.

The more detailed your report, the faster it can be resolved and will ensure it
is resolved in the right way.

## Code

If you would like to contribute code to fix a bug, add a new feature, or
otherwise improve Sealchain, pull requests are most welcome. It is a good idea to
[submit an issue](https://github.com/sealchain-project/sealchain/issues/new) to
discuss the change before plowing into writing code.

Please make sure your contributions adhere to our coding guidelines:

*  Code must adhere to the [Serokell Haskell Style Guide](https://github.com/serokell/serokell-util/blob/master/serokell-style.md).
*  Code must be documented with [Haddock](https://www.haskell.org/haddock/doc/html/index.html).
*  We are using [GitFlow](http://nvie.com/posts/a-successful-git-branching-model/.)
   branching model, so pull requests need to be based on and opened against the `develop`
   branch.
*  Please refer to [this guide](https://chris.beams.io/posts/git-commit/) to write a good Git commit message.

Please note that Sealchain uses a custom prelude [Universum](https://github.com/serokell/universum)
instead of the default one.

### Development Tricks

Common tasks for development are kept in `Makefile`s, one per package and one for the whole project.
Run `make help` to get assistance on custom commands.
As an example, you can run `make ghcid-test` in the `wallet` package to get a test-running `ghcid` process running.

### Code Quality

Sealchain uses [HLint](https://github.com/ndmitchell/hlint) as a code quality tool.

You can install it using `stack install hlint` command.

To check Sealchain code run this script (from the `sealchain` root directory):

```
$ ./scripts/haskell/lint.sh
```

### Code Style

Sealchain uses [stylish-haskell](https://github.com/jaspervdj/stylish-haskell) tool to
prettify Haskell code.

Please note that there is `.stylish-haskell.yaml` in the root of the repository. This
configuration file requires `stylish-haskell` version `0.8.1.0` or newer.

You can install it using `stack install stylish-haskell` command.

We also use [`editorconfig`](https://editorconfig.org/) to maintain consistent indentation and maximum line length.
You can [download a plugin](https://editorconfig.org/#download) for almost any common editor.

## Testing

To run tests for Sealchain code use this command (from the `sealchain` root directory):

```
$ stack test
```
