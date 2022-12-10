# binky-mode

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg?style=flat)](LICENSE)
[![MELPA](http://melpa.org/packages/binky-mode-badge.svg)](http://melpa.org/#/binky-mode)

Jump between points like a rabbit !

This package provides commands to jump between points in buffers and files.
Marked position, last jump position and recent buffers are all supported in
same mechanism like `point-to-register` but with an enhanced experience.

<!-- markdown-toc start -->

**Table of Contents**

- [binky-mode](#binky-mode)
  - [Screenshot](#screenshot)
  - [Install](#install)
    - [dependencies](#dependencies)
    - [package](#package)
  - [Usage](#usage)
  - [Customization](#customization)
  - [Feature](#feature)
  - [Todo](#todo)
  - [License](#license)

<!-- markdown-toc end -->

## Screenshot

- Call `M-x binky-*` related command

![binky with preview](image/binky-preview.png)

`?` or `C-h` for popup preview mannually

`q`, `C-g` or `escape` for quit

## Install

### dependencies

- cl-lib

### package

- Manually

Clone and add to `load-path`, require the package.

- Melpa

This package is available on [MELPA][melpa].
Install with `M-x package-install` <kbd>RET</kbd> `binky-mode` from within Emacs.

## Usage

```elisp
;; Directly
(require 'binky-mode)
(binky-mode)

;; Or with `use-package`, etc
(use-package binky-mode
  :hook (after-init-hook . binky-mode))
```

- `binky-add`    : add current positon to records
- `binky-delete` : delete existed position from records
- `binky-jump`   : jump to position in records
- `binky-binky`  : one command to rule all. With `C-u` prefix, keep editing untill quit 

## Customization

A lot of options are provided. See Docstrings

## Feature

- Better UI experience than `register-to-point`, such as preview customization and jump highlight
- Integration with buffers switch, and more sorting strategies are provided now and in future

## Todo

- [x] Avoid duplicated records added
- [ ] Buffer margin indicator support
- [ ] More sorting strategy: frecency, duration

## License

See [LICENSE](LICENSE).

[melpa]: http://melpa.org/#/binky-mode
