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
  - [Feature](#feature)
  - [Todo](#todo)
  - [License](#license)

<!-- markdown-toc end -->

## Screenshot

- Call `M-x binky-binky` command with preview

![binky with preview](image/binky-preview.png)

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

Run with `M-x` `binky-add` `binky-delete` `binky-jump` `binky-binky`

## Feature

- Better UI experience than `register-to-point`, such as preview customization
  and jump highlight
- Integration with buffers switch, and more sorting strategies are provided now
  and in future
- One command `binky-binky` to rule all
  - [x] add
  - [x] jump
  - [ ] delete

## Todo

- [ ] Implement sorting strategy: frecency, duration
- [ ] Add margin indicator support for `binky-alist`
- [ ] Avoid duplicated records added

## License

See [LICENSE](LICENSE).

[melpa]: http://melpa.org/#/binky-mode
