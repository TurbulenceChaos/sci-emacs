# Sci-Emacs
![Sci-Emacs](Sci-Emacs.png)

**Author:** Peng Peng  \
**Email:** [211110103110@stu.just.edu.cn](mailto:211110103110@stu.just.edu.cn)  \
**GitHub:** [TurbulenceChaos/Sci-Emacs](https://github.com/TurbulenceChaos/Sci-Emacs) \
**Environment:** (emacs "30.1" in WSL2 Ubuntu 24.04)

---
## Table of Contents
- [Sci-Emacs](#sci-emacs)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Installation](#installation)
  - [Testing Setup](#testing-setup)
    - [LaTeX Tikz](#latex-tikz)
    - [Jupyter Wolfram Language](#jupyter-wolfram-language)

## Introduction
This is my personal emacs configuration, specifically aimed at academic research.

## Installation
```bash
# Clone Sci-Emacs repository into your emacs config directory
git clone https://github.com/TurbulenceChaos/Sci-Emacs.git ~/.emacs.d

cd ~/.emacs.d

# Install `ispell` for spell checking support (`flyspell-mode`)
sudo apt install ispell
```

## Testing Setup
Verify setup in [Test.org](Test/Test.org).

### LaTeX Tikz
Demo: [Test-org-babel-tikz.gif](Test/Test-org-babel-tikz.gif) 

![Test Org-babel-Tikz](Test/Test-org-babel-tikz.gif)

### Jupyter Wolfram Language
You can find my [Wolfram-terminal-image](https://github.com/TurbulenceChaos/Wolfram-terminal-image) package on GitHub, which allows wolfram images to be shown in the vscode terminal and emacs org-mode.

Demo: [Test-emacs-jupyter-wolfram-language.gif](Test/Test-emacs-jupyter-wolfram-language.gif)

![Test Emacs-jupyter wolfram language](Test/Test-emacs-jupyter-wolfram-language.gif)

