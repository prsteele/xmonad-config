XMonad Config
==============

This repository contains the configuration files necessary to
replicate my XMonad desktop environment.

For simplicity, many .hidden files have been renamed to not have a
leading period. The table below summarizes the mapping from
in-repository names to filesystem names; I suggest symlinking the
repository files to their filesystem locations so a `git pull` updates
them automatically.

| Repo file                   | Filesystem name                        |
| --------------------------- | -------------------------------------- |
| xmobarrc                    | ~/.xmobarrc                            |
| xmonad/                     | ~/.xmonad/                             |
| upstart/                    | ~/.config/upstart                      |
| applications/xmonad.desktop | /usr/share/applications/xmonad.desktop |
| xsessions/xmonad.desktop    | /usr/share/xsessions/xmonad.desktop    |
| xmonad.start                | /usr/local/bin                         |

xmobar
------

xmobar is a 'minimalistic status bar' written in Haskell. Its layout
is configured by the xmobarrc file, and is loaded up by
xmonad/xmonad.hs.

upstart
-------

We rely on Upstart to daemonize the tray utilities. IMPORTANT: You
need to ensure that the line `xmonad` is present (and not commented
out) in `/etc/upstart-xsessions` to allow Upstart to manage the
session.

Dependencies
------------

This configuration assumes you have `scrot` installed to take
screenshots, and assumes you have created the `~/Pictures/Screenshots`
directory.