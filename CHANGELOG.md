# Changelog
# [1.3.0](https://github.com/liuyinz/emacs.d/compare/v1.2.2...v1.3.0) (2023-08-29)


### Bug Fixes

* don not run binky-recent-alist-update-hook if no changes ([1f2ce82](https://github.com/liuyinz/emacs.d/commit/1f2ce82bd7dd8f412074b391b38592c49135d67d))
* error when scroll mouse in binky-mark-read ([28468bd](https://github.com/liuyinz/emacs.d/commit/28468bde909f8d9a9f7b26fb5e065a9c5d83fb14))
* wrong function name ([2948f53](https://github.com/liuyinz/emacs.d/commit/2948f5319209c5ce898d5cf3ea94d24ef64e584c))


### Features

* add project information for records ([b6838b6](https://github.com/liuyinz/emacs.d/commit/b6838b65321b270d5177d1af32ec4131c19e4dec))


### Performance Improvements

* only update margins on recorded buffers ([7b22515](https://github.com/liuyinz/emacs.d/commit/7b2251558f9eede39f328e51d914907c2bc59f5c))



## 1.2.2 (2023-08-11)


### Bug Fixes

* ci error on missing file ([5915e42](https://github.com/liuyinz/emacs.d/commit/5915e42b7379c4b7d5242d09ca824dc1e9ca04a6))


### Features

* add commands jumping to next/previous record in current buffer ([3fe9656](https://github.com/liuyinz/emacs.d/commit/3fe965685d00da7d87c748e9098a9bbaa8a0fb58))



## [1.2.1](https://github.com/liuyinz/binky-mode/compare/v1.2.0...v1.2.1) (2023-07-22)


### Bug Fixes

* add recent record unexpectedly into manual alist ([80c4ee4](https://github.com/liuyinz/binky-mode/commit/80c4ee4accc1c814a65381da13de5e62cbeba22e))
* put killed buffer last in preview ([58fd3db](https://github.com/liuyinz/binky-mode/commit/58fd3db021d5a85e28cd8a16f5fa2ddf5e2101dc))
* recent records error in preview ([6a8bca5](https://github.com/liuyinz/binky-mode/commit/6a8bca5fbb7e7250728376aba00fc6ac641ba66e))
* sorting records in groups according to position ([7e26345](https://github.com/liuyinz/binky-mode/commit/7e2634509bc5167fdb17bc6329ee3677c49afa22))
* using unique buffer names in preview ([2a375d6](https://github.com/liuyinz/binky-mode/commit/2a375d6c71426a8bc749d90f35b55336b638cd27))


### Features

* add option binky-preview-in-groups ([8893242](https://github.com/liuyinz/binky-mode/commit/889324260f0a0c5696819ac16d67598300491eab))



# [1.2.0](https://github.com/liuyinz/binky-mode/compare/v1.1.0...v1.2.0) (2023-07-12)


### Bug Fixes

* adjust face color for preview ([df54d32](https://github.com/liuyinz/binky-mode/commit/df54d3231c950cc074fb7dab54b129d03434d7e7))
* **binky-margin-mode:** make margin indicator no italic style ([61b2833](https://github.com/liuyinz/binky-mode/commit/61b2833d62c67aadb60d7eaeffc92291ab5ba3d3))
* **binky-mark-back:** don't record position on the same line ([d054c4c](https://github.com/liuyinz/binky-mode/commit/d054c4ca27de70988836c6816d4788894a890210))
* don't auto mark last buffer if current-buffer is minibuffer ([6aea48d](https://github.com/liuyinz/binky-mode/commit/6aea48d587b2c378c8ba3f0483b9b93120816c82))
* echo warning if mark is illegal ([ec94dae](https://github.com/liuyinz/binky-mode/commit/ec94dae9779825487c3bb2c145dc43a2369b01d2))
* error on couldn't truncate some langauga character like chinese ([3ac9925](https://github.com/liuyinz/binky-mode/commit/3ac99256cfaa8fb335ea86d1615eb21a9d09473a))
* **preview:** close preview by force when quit the command ([1e0f9ab](https://github.com/liuyinz/binky-mode/commit/1e0f9abae41fa1773ee2b1f1275ea687e4f0a045))
* set window-min-height to 1 in preview ([b30a05f](https://github.com/liuyinz/binky-mode/commit/b30a05f67a3c725f4257378de081b5d6fafde999))
* typos in docstring ([b430ad8](https://github.com/liuyinz/binky-mode/commit/b430ad8b6bcc03830cd910d0540e149f68780cf8))


### Features

* add support for relative width of preview column ([5a7eac5](https://github.com/liuyinz/binky-mode/commit/5a7eac5e7dbfe003217e563be11eb62d5c7a354a))



# [1.1.0](https://github.com/liuyinz/binky-mode/compare/v1.0.0...v1.1.0) (2022-12-22)


### Bug Fixes

* highlight existing position line when add duplicated record ([7efd1d0](https://github.com/liuyinz/binky-mode/commit/7efd1d0b74fbe3bd8e2158e8a317f7684a273af4))
* propertize mark in binky--message ([d1c4824](https://github.com/liuyinz/binky-mode/commit/d1c4824bee793cabe1515e66187cc08463813b26))
* update dependency to emacs v26.3 ([4a4a99d](https://github.com/liuyinz/binky-mode/commit/4a4a99dbc2ae6b37e88dcafa6b4a06ead7b47ab4))


### Features

* add command binky-view ([16b1532](https://github.com/liuyinz/binky-mode/commit/16b1532d025666d863322b22cdabf76530bc8b13))
* adjust prompt color with binky-* commands ([ee6d482](https://github.com/liuyinz/binky-mode/commit/ee6d482e9a27ce1721ef3b716aa76de714b7c665))
* support margin indicator for back, auto marks ([d355f2a](https://github.com/liuyinz/binky-mode/commit/d355f2ad289ff1ca5b0d5cb0a52da4a036045f47))



# 1.0.0 (2022-12-16)


### Bug Fixes

* adjust face binky-highlight-warn ([3e09212](https://github.com/liuyinz/binky-mode/commit/3e0921216ed36b3e27b005f3e85b64bd893f3952))
* adjust face style ([83132f3](https://github.com/liuyinz/binky-mode/commit/83132f34eeebd8f54d620684b6b4da2a73a6e2cf))
* binky-highlight-jump extend to t ([95a1527](https://github.com/liuyinz/binky-mode/commit/95a15274deff19ddbca59fe661c74e34973f07a9))
* binky-highlight-warn :extend to t ([82134b8](https://github.com/liuyinz/binky-mode/commit/82134b8916c9744ddc108812aafe30d3ae9364dd))
* compatiability of function pos-eol ([f10040e](https://github.com/liuyinz/binky-mode/commit/f10040ef3fb7b67f49f287b4d7401a6caa79e7be))
* defcustom type error ([dff6000](https://github.com/liuyinz/binky-mode/commit/dff6000e61c52291a2205c7acee65fe504d4ec1f))
* **doc:** typos ([2e4ad97](https://github.com/liuyinz/binky-mode/commit/2e4ad970d91c80bdefcfd977e2e33857ddb874c3))
* set binky-frequency-timer only when sort by frequency ([ad65400](https://github.com/liuyinz/binky-mode/commit/ad65400f8bb2c62dbb4ab15c2a685490b9efb226))
* some warnings ([50cc3f9](https://github.com/liuyinz/binky-mode/commit/50cc3f9bd16d4b32d71ba37f1f9e97eae83db781))
* typos ([5bf7121](https://github.com/liuyinz/binky-mode/commit/5bf7121973db5a27136e59ab30155ecb07036a22))
* wrong line number in preview ([fe07943](https://github.com/liuyinz/binky-mode/commit/fe079431e2ea8faa98b4d1389077192a5f5dc261))
* wrong position info when swap-in ([3976b3d](https://github.com/liuyinz/binky-mode/commit/3976b3daf716611e39afde2ad2d8bf9b3d91f7d0))


### Features

* add command binky-auto-toggle ([d0b5c2f](https://github.com/liuyinz/binky-mode/commit/d0b5c2f3b33482621ccc2aab712192678e7f3c34))
* add face binky-preview-clolumn-name-same ([9be6064](https://github.com/liuyinz/binky-mode/commit/9be606483454bf80b80f086afaa55a170461f0d1))
* add feature highlight for binky-add,jump,delete ([8c17632](https://github.com/liuyinz/binky-mode/commit/8c176327ba39e2f1723a34285bbc94a31e914f62))
* add option binky-mark-distance ([c473057](https://github.com/liuyinz/binky-mode/commit/c473057e00db9d16d285ddbfd53c0bae20d5d8f3))
* add option binky-mark-quit ([53dfa77](https://github.com/liuyinz/binky-mode/commit/53dfa77655bbd4c6f5764e25ac25c5d861dd5311))
* add option binky-preview-side ([d111298](https://github.com/liuyinz/binky-mode/commit/d111298291ff3a7d3ea291d65fab49f7f50f8d35))
* add option binky-record-prune and refactor ([a6def39](https://github.com/liuyinz/binky-mode/commit/a6def392a71d2b56d117a3c6ef6511a65c0a538a))
* implement command binky-binky ([2d8b654](https://github.com/liuyinz/binky-mode/commit/2d8b6546fd4cd17d0271d46c6a35c59c3b02339b))
* support buffer margin indicator ([dfc69ac](https://github.com/liuyinz/binky-mode/commit/dfc69ac6a5d451b6990b600e32ffd259f5873a51))
