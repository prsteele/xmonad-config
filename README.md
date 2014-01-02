XMonad Config
==============

This repository contains the configuration files necessary to
replicate my XMonad desktop environment.

For simplicity, many .hidden files have been renamed to not have a
leading period. The table below summarizes the mapping from
in-repository names to filesystem names; I suggest symlinking the
repository files to their filesystem locations so a `git pull` updates
them automatically.

| Repo file | Filesystem name |
| --------- | --------------- |
| xmobarrc  | ~/.xmobarrc     |
| xmonad/   | ~/.xmonad/      |
| upstart/  | ~/.config/upstart |

xmobar
------

xmobar is a 'minimalistic status bar' written in Haskell. Its layout
is configured by the xmobarrc file, and is loaded up by
xmonad/xmonad.hs.

upstart
-------

We rely on Upstart to daemonize the tray utilities.