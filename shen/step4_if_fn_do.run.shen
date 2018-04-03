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
   (load "src/core.shen")
   (load "src/step4_if_fn_do.shen")
   \* Assume that shen was invoked as shen -l STEP.run.shen *\
   (if (not IsInteractive)
       (main)
       (println "Running in Emacs REPL"))))
