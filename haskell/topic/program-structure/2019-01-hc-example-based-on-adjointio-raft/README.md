This uses the structure of adjoint io's Raft implementation, but without Raft --- as an example to show how to structure an application.


Notes

#+begin_example

(network ->) Event -> handle -> Action (-> network)

#+end_example

- pure
  - type-level programming
    - =Transition=  : to specify valid transitions between node modes
    - =ResultState= : uses =Transition= to statically enforce valid transitions
      - existential =res= hides result mode
    - =NodeState=   : ensures "state" type matches mode
  - type classes
    - XSM (RSM)     : pluggable application state and commands to update that app state
  - custom monad stacks
    - LoggerT

- interface between pure and effect layer
  - existential
    - =XNodeState= : to pass "state" in/out of pure execution
      - existential =s= ensures that state inaccessible outside pure

- effect layer
  - type-classes
    - XSendClient : pluggable transport
    - XRecvClient : pluggable transport
    - XPersist    : pluggable persistent storage
  - custom monad stacks
    - XT
