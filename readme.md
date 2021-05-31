# nimtga

A simple library to read and write the [TGA image format](https://en.wikipedia.org/wiki/Truevision_TGA) written in [Nim](https://nim-lang.org/).

Some limited image editing operations (rotate, concatenate, scale) are also included although those are not specific to TGA, they just operate on a sequence of pixels.

## Use with nimble
This library can be added to a projects `.nimble` file like this:
```
requires "https://github.com/hendrikgit/nimtga#0.2.0"
```
The part after the # is a git tag correspondeing to a release. A valid git commit hash could be used instead.
