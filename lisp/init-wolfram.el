;;; init-wolfram.el --- Wolfram Mathematica config
;;; Commentary:
;;; Code:

(unless (package-installed-p 'Wolfram-terminal-image)
  (let ((package-check-signature nil))
    (package-vc-install
     '(Wolfram-terminal-image :url "https://github.com/TurbulenceChaos/Wolfram-terminal-image.git"
			      :branch "sci-wolfram"))))

(setq sci-wolfram-play nil)
(setq sci-wolfram-player "wolframplayer.exe")

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
 	       `(xah-wolfram-mode . ("/usr/local/Wolfram/WolframEngine/14.1/Executables/WolframKernel" "-noinit" "-noprompt" "-nopaclet" "-noicon" "-nostartuppaclets" "-run" "Needs[\"LSPServer`\"]; LSPServer`StartServer[]"))))


(provide 'init-wolfram)
;;; init-wolfram.el ends here
