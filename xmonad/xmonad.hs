import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.Scratchpad

import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders

-- The various workspaces I use
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

workspace1 = "1-main"
workspace2 = "2-web"
workspace3 = "3-comms"
workspace4 = "4-music"
workspace5 = "5-games"
workspace6 = "6"
workspace7 = "7"
workspace8 = "8"
workspace9 = "9-min"

-- Float programs to the correct workspaces
myManageHook = (composeAll . concat $
                [ [resource   =? r --> doIgnore           | r <- myIgnores]
                , [className  =? c --> doShift workspace2 | c <- myWebs]
                , [className  =? c --> doShift workspace3 | c <- myComms]
                , [className  =? c --> doShift workspace4 | c <- myMusic]
                , [className  =? c --> doShift workspace5 | c <- myGames]
                , [manageDocks]
                , [spawnScratchpadManageHook]
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

    spawnScratchpadManageHook = scratchpadManageHook (W.RationalRect 0.2 0.3 0.6 0.4)

-- Handle dock programs such as xmobar
myLayoutHook = avoidStruts $ layoutHook defaultConfig

-- Handle fullscreen events properly (e.g. making fullscreen flash work in Firefox)
myEventHook = handleEventHook defaultConfig <+> fullscreenEventHook

-- Spawn useful daemons
spawnDaemons = mapM_ spawnDaemon daemons
  where
    spawnDaemon d = safeSpawn "start" [d]
    daemons = ["trayer-daemon", "nm-applet-daemon", "gnome-sound-applet-daemon"]

-- Bind mod-u to pull up a scratchpad window
spawnScratchpad = ((myModMask, xK_u), scratchpadSpawnAction myConfig)


-- Bind mod-z to lock the screen
lockScreen = ((myModMask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")

-- Use the super key as the mod key
myModMask = mod4Mask

-- Top-level configuration
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
           , spawnScratchpad ]

main = do
  spawnDaemons
  x <- xmobar myConfig
  xmonad $ x

