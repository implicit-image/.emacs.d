
(defmacro +os/per-system! (&rest args)
  (let ((val (cond ((+os/is-windows-p) (plist-get args :win))
                   ((+os/is-wsl-p) (plist-get args :wsl))
                   ((+os/is-linux-p) (plist-get args :linux)))))
    val))

(defun +os/is-windows-p ()
  (eq system-type 'windows-nt))

(defun +os/is-linux-p ()
  (eq system-type 'gnu/linux))

(defun +os/is-wsl-p ()
  (and (+os/is-linux-p)
       (file-exists-p "/proc/sys/fs/binfmt_misc/WSLInterop")))

(when (+os/is-windows-p)
  (add-to-list 'exec-path "c:/ProgramData/chocolatey/bin/")
  (add-to-list 'exec-path "c:/ProgramData/mingw64/mingw64/bin/")
  (add-to-list 'exec-path "c:/Program Files/Git/cmd/")
  (add-to-list 'exec-path "c:/Program Files/Git/usr/bin/")
  (add-to-list 'exec-path "C:/Users/b/AppData/Local/Programs/MiKTeX/miktex/bin/x64")
  (prefer-coding-system 'utf-8)
  (setq is-windows t))


(provide 'init-os)
