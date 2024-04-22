# zngrn's emacs config

  * Sits in `~/.emacs.d`.
  * Entry point is `~/.emacs.d/init.el`.

## Installation Notes:

  * git clone: `https://github.com/zngrn/dot-emacs.git ~/.emacs.d`

## Programming Setup

### Clojure Setup

  * To use `clojure` with lsp:
    - Install manually: [clojure-lsp](https://clojure-lsp.io/installation/#manually)
  * Check clojure lsp:
    - `which clojure-lsp` should prompt a sane location

### Golang Setup

  * To use `golang` with lsp:
    - Install `gopls` manually: `go install golang.org/x/tools/gopls@latest
`.
  * Check `gopls`:
    - `~/go/bin/gopls version` should prompt a version

### TypeScript setup

  * To use `typescript` with lsp:
    - Install `npm i -g typescript-language-server`. [Reference](https://emacs-lsp.github.io/lsp-mode/page/lsp-typescript/)
