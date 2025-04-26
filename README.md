# Sci-Emacs

**Author:** Peng Peng  \
**Email:** [211110103110@stu.just.edu.cn](mailto:211110103110@stu.just.edu.cn)  \
**GitHub:** [TurbulenceChaos/Sci-Emacs](https://github.com/TurbulenceChaos/Sci-Emacs) \
**Environment:** (emacs "30.1" in WSL2 Ubuntu 24.04)

---

## Introduction
This is my personal emacs configuration, specifically aimed for academic research.

## Installation
```bash
# Clone the Sci-Emacs repository into the emacs directory
git clone https://github.com/TurbulenceChaos/Sci-Emacs.git ~/.emacs.d

cd ~/.emacs.d

# Initialize and update git submodules recursively
git submodule update --init --recursive

# Install ispell for `spell-checking` support
sudo apt install ispell
```

## Testing setup
Verify setup in [Test/Test.org](Test/Test.org).

### Latex tikz
Demo: [Test-org-babel-tikz.gif](Test/Test-org-babel-tikz.gif) 

![Test Org-babel-Tikz](Test/Test-org-babel-tikz.gif)

### Jupyter wolfram language
You can find my [Wolfram-terminal-image](https://github.com/TurbulenceChaos/Wolfram-terminal-image) package on GitHub, which allows wolfram images to be shown in the vscode terminal and emacs org-mode.

Demo: [Test-emacs-jupyter-wolfram-language.gif](Test/Test-emacs-jupyter-wolfram-language.gif)

![Test Emacs-jupyter wolfram language](Test/Test-emacs-jupyter-wolfram-language.gif)
