-- default desktop configuration for Fedora

import System.Posix.Env (getEnv)
import Data.Maybe (maybe)

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Config.Desktop
import XMonad.Config.Gnome

import System.IO

myModMask = mod4Mask

-- main = do
--   xmproc <- spawnPipe "xmobar"
--   spawn trayerString
--   let config = withUrgencyHook NoUrgencyHook desktopConfig
--   xmonad $ config { 
--     modMask              = myModMask
--     , terminal           = "urxvt"
--     , normalBorderColor  = "#cccccc"
--     , focusedBorderColor = "#cd8b00"
--     , borderWidth        = 2
--     , manageHook         = manageDocks <+> manageHook defaultConfig
--     , layoutHook         = myLayoutHook
--     , logHook            = xmobarLog xmproc
--     } `additionalKeys`
--     [ lockScreen ]

main = do
  x <- xmobar myConfig
  xmonad x

myConfig = withUrgencyHook NoUrgencyHook desktopConfig {
  modMask              = myModMask
  , terminal           = "urxvt"
  , normalBorderColor  = "#cccccc"
  , focusedBorderColor = "#cd8b00"
  , borderWidth        = 2
  , manageHook         = manageDocks <+> manageHook defaultConfig
  , layoutHook         = myLayoutHook
  } `additionalKeys` 
           [ lockScreen ]
    
lockScreen = ((myModMask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")

myLayoutHook = avoidStruts $ layoutHook defaultConfig

xmobarLog xmproc = dynamicLogWithPP xmobarPP {
  ppOutput = hPutStrLn xmproc
  , ppTitle = xmobarColor "green" "" . shorten 50
  , ppUrgent = xmobarColor "yellow" "red" . xmobarStrip
  }
                   
trayerString = "trayer --edge top --align right --SetDockType true "
               ++ "--SetPartialStrut true --expand true --width 10 "
               ++ "--transparent true --tint 0x000000 --height 12 &"
                   
desktop "gnome" = gnomeConfig
desktop "xmonad-gnome" = gnomeConfig
desktop _ = desktopConfig
