# console-img
Print an image to the console using RGB ANSI codes.

![Example result](sample/happy-dragon-result.png)

## Build
Have [Stack](https://docs.haskellstack.org/) installed.

```bash
stack build
stack install
```

## Usage
Call the `console-img` program with a single argument.
Example:

```bash
console-img my-picture.png
```

The current version does no rescaling, so if you have a large image you
must downscale it manually to fit the terminal.
For example, if you have ImageMagick:

```bash
console-img <(convert big-image.png -resize 80 png:-)
```
