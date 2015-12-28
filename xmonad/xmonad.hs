import System.Posix.Env (getEnv)
import Data.Maybe (maybe)

import System.IO
import XMonad
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.Accordion
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe, safeSpawn)
import XMonad.Util.Scratchpad

myLayoutHook = avoidStruts $ layoutHook defaultConfig

       
lockScreen = ((myModMask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")

-- Handle fullscreen events properly (e.g. making fullscreen flash work in Firefox)
myEventHook = handleEventHook defaultConfig <+> fullscreenEventHook

xmobarLog xmproc = dynamicLogWithPP xmobarPP {
  ppOutput = hPutStrLn xmproc
  , ppTitle = xmobarColor "green" "" . shorten 50
  , ppUrgent = xmobarColor "yellow" "red" . xmobarStrip
  }

workspace1 = "1-main"
workspace2 = "2-web"
workspace3 = "3-comms"
workspace4 = "4-music"
workspace5 = "5-games"
workspace6 = "6"
workspace7 = "7"
workspace8 = "8"
workspace9 = "9-min"

myWorkspaces = [ workspace1
               , workspace2
               , workspace3
               , workspace4
               , workspace5
               , workspace6
               , workspace7
               , workspace8
               , workspace9
               ]

-- myManageHook = composeAll [ fullscreenFix
--                           , manageDocks
--                           , manageHook defaultConfig
--                           , spawnIPythonManageHook
--                           ]

-- Float programs to the correct workspaces
myManageHook = (composeAll . concat $
                 [ [resource   =? r --> doIgnore           | r <- myIgnores]
                 , [className  =? c --> doShift workspace2 | c <- myWebs]
                 , [className  =? c --> doShift workspace3 | c <- myComms]
                 , [className  =? c --> doShift workspace4 | c <- myMusic]
                 , [className  =? c --> doShift workspace5 | c <- myGames]
                 , [fullscreenFix]
                 , [manageDocks]
                 --, [manageHook defaultConfig]
                 , [spawnIPythonManageHook]
                 ])
  where
    myIgnores = [ "xmobar"
                , "trayer"
                ]

    myWebs    = ["firefox"]

    myComms   = []
    
    myMusic   = ["spotify", "Rhythmbox"]

    myGames   = [ "frozensynapse"
                , "battle.net"
                , "Heathstone"
                ]

fullscreenFix = isFullscreen    --> doF W.focusDown <+> doFullFloat

myIgnores = ["trayer", "xmobar"]

-- fullscreenFix = composeOne [ isFullscreen -?> doFullFloat
--                            ]

spawnDaemons = mapM_ spawnDaemon daemons

spawnDaemon d = safeSpawn "start" [d]

daemons = ["trayer-daemon", "nm-applet-daemon", "gnome-sound-applet-daemon"]

spawnIPython = ((myModMask, xK_u), scratchpadSpawnAction myConfig)

spawnIPythonManageHook = scratchpadManageHook (W.RationalRect 0.2 0.3 0.6 0.4)

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
  , borderWidth        = 0
  , manageHook         = myManageHook
  , layoutHook         = myLayoutHook
  , handleEventHook    = myEventHook
  , XMonad.workspaces  = myWorkspaces
  } `additionalKeys` 
           [ lockScreen 
           , spawnIPython ]
