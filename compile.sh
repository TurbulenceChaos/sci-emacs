#!/bin/bash

set -e # Exit immediately if a command exits with a non-zero status

# Redirect all output to a log file
# exec >"$HOME/.emacs.d/compile.log" 2>&1

# Install eldev for emacs-jupyter
curl -fsSL https://raw.github.com/emacs-eldev/eldev/master/webinstall/eldev | sh

# Ensure eldev binaries are in the system PATH
export PATH="$HOME/.local/bin:$PATH"

# Build emacs-jupyter
packages_dir=$HOME/.emacs.d/lisp-site

# List of directories to build with make
make_dirs=(
    "jupyter"
    "magit/lisp"
    "diff-hl"
    "compat"
    "markdown-mode"
    "transient/lisp"
    "with-editor/lisp"
    "dash.el"
    "llama"
)

for package_dir in "${make_dirs[@]}"; do
    cd "$packages_dir/$package_dir"
    make clean
    make
done

# List of Emacs Lisp files to byte-compile
elisp_files=(
    "undo-fu-session/undo-fu-session.el"
    "vertico/vertico.el"
    "orderless/orderless.el"
    "org-sliced-images/org-sliced-images.el"
    "xah-wolfram-mode/xah-wolfram-mode.el"
)

for file in "${elisp_files[@]}"; do
    cd "$packages_dir/$(dirname "$file")"
    emacs -batch -L . -f batch-byte-compile "$(basename "$file")"
done

# Return to the Emacs directory
cd ~/.emacs.d
