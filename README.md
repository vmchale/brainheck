# Brainheck intrepreter

This is a brainh\*ck interpreter written in Haskell. It's intended to be as
abstruse as possible and as such makes use of recursion schemes, lenses, and
monadic parser combinators.

## Installation

With [nix](http://nixos.org/nix/):

```bash
 $ nix-env -i brainheck
```

With [stack](https://haskellstack.org/):

```bash
 $ stack install brainheck
```

## Examples

```bash
 $ brainheck helloworld.bf
 Hello World!
```
