
(defun +os/is-windows-p ()
  (eq system-type 'windows-nt))

(defun +os/is-linux-p ()
  (eq system-type 'gnu/linux))

(defun +os/is-wsl-p ()
  (and (+os/is-linux-p)
       (f-exists? "/proc/sys/fs/binfmt_misc/WSLInterop")))

(when (+os/is-windows-p)
  (add-to-list 'exec-path "c:/ProgramData/chocolatey/bin/")
  (add-to-list 'exec-path "c:/ProgramData/mingw64/mingw64/bin/")
  (add-to-list 'exec-path "c:/Program Files/Git/cmd/")
  (add-to-list 'exec-path "C:/Users/b/AppData/Local/Programs/MiKTeX/miktex/bin/x64")
  (prefer-coding-system 'utf-8)
  (setq is-windows t))


(provide 'init-os)
