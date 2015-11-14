private
===

Implementation of the Forth emulator.

The folder name `private` is a Racket convention.
It's a hint to not require the modules here, though there's nothing to stop any module from doing:

```
(require forth/private/stack)
```

and using the undocumented API.


Module Summary
---
- `command.rkt` Implements Forth command parsing & execution
- `stack.rkt` Implements a stack data structure
