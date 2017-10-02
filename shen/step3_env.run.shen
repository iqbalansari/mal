(let IsInteractive (== (value *language*) "Elisp")
  (do
   \* Suppress all output except pr unless we are running in Emacs *\
   (set *hush* (not IsInteractive))
   (load "src/utils.shen")
   (load "src/tokenizer.shen")
   (load "src/types.shen")
   (load "src/reader.shen")
   (load "src/printer.shen")
   (load "src/env.shen")
   (load "src/step3_env.shen")
   \* Assume that shen was invoked as shen -l STEP.run.shen *\
   (if (not IsInteractive)
       (main)
       (println "Running in Emacs REPL"))))
