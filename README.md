# My emacs config

This repo contains my emacs configuration. I used to use [Doom Emacs](https://github.com/doomemacs/doomemacs) and some of this repo is based on its modules.

## File locations

`init.el` and `early-init.el` are in  `./`, all other files are in `./modules/` and are named `init-<module-identifier>,el` where `<module-identifier>` is unique and describes the functionality configured in the file. All modules `provide` a feature corresponding to the name of the file minus the extension.

## Naming convention

Custom public variables are `+<module-identifier>/<variable-name>`, ex.
``` elisp
(setq +editing/evil-mc-disabled-modes '())
```

Custom private variables are `+<module-identifier>--<variable-name>`, ex.
``` elisp
(setq +editing--current-method-index 0)
```

Custom commands are `+<module-identifier>/<command-name>`, ex.
```elisp
(+terminal/run-command)
```

Custom public functions are `+<module-identifier>-<function-name>`, ex.
```elisp
(+utils-nth-wrapped)
```

Custom private functions are `+<module-identifier>--<function-name>`, ex.
```elisp
(+terminal--get-name)
```

Custom macros are `+<module-identifier>/<macro-name>!`
```elisp
(+keys/bind!)
```
