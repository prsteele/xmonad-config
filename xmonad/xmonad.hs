import System.Posix.Env (getEnv)
import Data.Maybe (maybe)

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run (spawnPipe, safeSpawn)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Layout.Accordion
import XMonad.Util.Scratchpad
import XMonad.StackSet as W

import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders

import System.IO

main = do
  spawnDaemons
  x <- xmobar myConfig
  xmonad $ x

myModMask = mod4Mask

myConfig = withUrgencyHook NoUrgencyHook desktopConfig {
  modMask              = myModMask
  , terminal           = "urxvt"
  , normalBorderColor  = "#cccccc"
  , focusedBorderColor = "#cd8b00"
  , borderWidth        = 2
  , manageHook         = myManageHook
  , layoutHook         = myLayoutHook
  , XMonad.workspaces  = myWorkspaces
  } `additionalKeys` 
           [ lockScreen 
           , spawnIPython 
           , screenShot
           , screenShotWindow ]
    
lockScreen = ((myModMask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")

myLayoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig

-- This looks like it might be useful for IMs
--myLayoutHook = Accordion

xmobarLog xmproc = dynamicLogWithPP xmobarPP {
  ppOutput = hPutStrLn xmproc
  , ppTitle = xmobarColor "green" "" . shorten 100
  , ppUrgent = xmobarColor "yellow" "red" . xmobarStrip
  }

myWorkspaces = ["1-main",
                "2-web",
                "3-comms",
                "4-music",
                "5-games",
                "6",
                "7",
                "8",
                "9-min"]
               
myManageHook = composeAll 
               [ className =? "dota_linux" --> doFullFloat 
               , className =? "dota_linux" --> doShift "5-games" 
               ] 
               <+> manageDocks <+> manageHook defaultConfig <+> spawnIPythonManageHook

spawnDaemons = mapM_ spawnDaemon daemons

spawnDaemon d = safeSpawn "start" [d]

daemons = ["trayer-daemon", "nm-applet-daemon", "gnome-sound-applet-daemon"]

spawnIPython = ((myModMask, xK_u), scratchpadSpawnAction myConfig)

spawnIPythonManageHook = scratchpadManageHook (W.RationalRect 0.2 0.3 0.6 0.4)

-- Take a screenshot when printscreen is pressed, then move the image
-- to ~/Pictures/Screenshots.
screenShot       = ((0, xK_Print), 
                    spawn "scrot -e 'mv $f ~/Pictures/Screenshots/'")

-- Take a screenshot of just the focused window when ctrl +
-- printscreen is pressed, then move the image to
-- ~/Pictures/Screenshots
screenShotWindow = ((controlMask, xK_Print), 
                    spawn "scrot -u -e 'mv $f ~/Pictures/Screenshots/'")