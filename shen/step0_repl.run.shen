(let IsInteractive (== (value *language*) "Elisp")
  (do
   \* Suppress all output except pr *\
   (set *hush* true)
   (load "src/utils.shen")
   (load "src/step0_repl.shen")
   \* Assume that shen was invoked as shen -l STEP.run.shen *\
   (if (not IsInteractive)
       (main)
       (println "Running in Emacs REPL"))))
