#!/bin/bash

# Install eldev for emacs-jupyter
if [ ! -f "$HOME/.local/bin/eldev" ]; then
    curl -fsSL https://raw.github.com/emacs-eldev/eldev/master/webinstall/eldev | sh
fi

# Ensure eldev binaries are in the system PATH
export PATH="$HOME/.local/bin:$PATH"

# Directory of Emacs Lisp packages
packages_dir=$HOME/.emacs.d/site-lisp

# List of directories to build with make
make_dirs=(
    "jupyter"
    "magit/lisp"
    "diff-hl"
    "markdown-mode"
    "transient/lisp"
    "with-editor/lisp"
    "dash.el"
    "llama"
    "keycast"
)

for make_dir in "${make_dirs[@]}"; do
    cd "$packages_dir/$make_dir"
    make clean
    make
done

# List of Emacs Lisp files to byte-compile
elisp_files=(
    "undo-fu-session/undo-fu-session.el"
    "orderless/orderless.el"
    "org-sliced-images/org-sliced-images.el"
    "xah-wolfram-mode/xah-wolfram-mode.el"
    "vundo/vundo.el"
)

for elisp_file in "${elisp_files[@]}"; do
    cd "$packages_dir/$(dirname "$elisp_file")"
    emacs -batch -L . -f batch-byte-compile "$(basename "$elisp_file")"
done

# Return to the Emacs directory
cd ~/.emacs.d
