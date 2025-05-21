;;; init-wolfram.el --- Wolfram Mathematica config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'Wolfram-terminal-image)
  (package-vc-install
   '(Wolfram-terminal-image :url "https://github.com/TurbulenceChaos/Wolfram-terminal-image.git"
			    :branch "sci-wolfram")))

(setq sci-wolfram-play nil
      sci-wolfram-player "wolframplayer.exe"
      sci-wolfram-kernel "/usr/local/Wolfram/WolframEngine/14.1/Executables/WolframKernel"
      sci-wolfram-jupyter-formula-type "image"
      org-babel-min-lines-for-block-output 100)


(provide 'init-wolfram)
;;; init-wolfram.el ends here
