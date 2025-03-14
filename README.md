# SCI Emacs

**Author:** Turbulence 
**Email:** 211110103110@stu.just.edu.cn  

## Introduction
This is my Emacs configuration, specifically tailored for academic research.

## Installation
```bash
# Clone the SCI-emacs repository into the Emacs directory
git clone https://github.com/TurbulenceChaos/SCI-emacs.git ~/.emacs.d

# Navigate to the Emacs directory
cd ~/.emacs.d

# Initialize and update Git submodules recursively
git submodule update --init --recursive

# Compile Emacs Lisp packages
bash compile.sh

# Install ispell for spell-checking support
sudo apt install -y ispell
```

## Test
Test Org Babel functionality in [Test/Test.org](Test/Test.org).

### Test Org-babel-Tikz

![Test Org-babel-Tikz](Test/Test-org-babel-tikz.gif)

### Test Emacs-jupyter wolfram language

![Test Emacs-jupyter wolfram language](Test/Test-emacs-jupyter-wolfram-language.gif)
