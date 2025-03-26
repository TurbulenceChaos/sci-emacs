# SCI Emacs

**Author:** Peng Peng  \
**Email:** [211110103110@stu.just.edu.cn](mailto:211110103110@stu.just.edu.cn)  \
**GitHub:** [TurbulenceChaos/SCI-emacs](https://github.com/TurbulenceChaos/SCI-emacs) \
**Package-Requires:** (emacs "30.1")

---

## 1. Introduction
This is my Emacs configuration, specifically tailored for academic research.

## 2. Installation
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

## 3. Testing setup
Verify Org Babel functionality by running the test script in [Test/Test.org](Test/Test.org).

### 3.1 Testing Org-babel-Tikz
Demo: [Test-org-babel-tikz.gif](Test/Test-org-babel-tikz.gif) 

![Test Org-babel-Tikz](Test/Test-org-babel-tikz.gif)

### 3.2 Testing Jupyter-Wolfram-Language
You can find my [Wolfram-Terminal-Image](https://github.com/TurbulenceChaos/Wolfram-terminal-image) package on GitHub. 
This package enhances the display of WolframScript graphics in terminal environments like VS Code and Emacs Org-mode.

Demo: [Test-emacs-jupyter-wolfram-language.gif](Test/Test-emacs-jupyter-wolfram-language.gif)

![Test Emacs-jupyter wolfram language](Test/Test-emacs-jupyter-wolfram-language.gif)
