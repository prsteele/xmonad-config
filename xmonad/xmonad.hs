import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.Scratchpad

-- Use the super key as the mod key
myModMask = mod4Mask

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

-- Layouts
tall = Tall 1 (3 / 100) (1 / 2)
myLayoutHook = avoidStruts (tall ||| Mirror tall ||| Full ||| simpleTabbed)

-- Handle fullscreen events properly (e.g. making fullscreen flash
-- work in Firefox)
myEventHook = handleEventHook def <+> fullscreenEventHook

-- Spawn useful daemons
spawnDaemons = mapM_ spawnDaemon daemons
  where
    spawnDaemon d = safeSpawn "start" [d]
    daemons = ["trayer-daemon", "nm-applet-daemon"]

-- Keybindings
myKeys = 
  [ -- Pull up a floating terminal window
    ((myModMask, xK_u), scratchpadSpawnAction myConfig)
    -- Lock the screen
  , ((myModMask .|. shiftMask, xK_z), spawn "slock")
    -- Raise the volumne
  , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 1 \"+1%\"")
    -- Lower the volume
  , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 1 \"-1%\"")
    -- Toggle the mute status
  , ((0, xF86XK_AudioMute),        spawn "pactl set-sink-mute 1 toggle")
  ]

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
  } `additionalKeys` myKeys

-- xmobar configuration
xmobarTitle = xmobarColor "#1589CE" "" . shorten 100
xmobarCurrentWorkspace = xmobarColor "#1589CE" "" . wrap "[" "]"
xmobarVisibleWorkspace = xmobarColor "#9B991F" ""
xmobarHiddenWorkspace = xmobarColor "#9B991F" ""
xmobarUrgent = xmobarColor "red" "yellow"

myLogHook = xmobarPP {
  ppTitle = xmobarTitle
  , ppCurrent = xmobarCurrentWorkspace
  , ppHidden = xmobarHiddenWorkspace
  , ppVisible = xmobarVisibleWorkspace
  , ppUrgent = xmobarUrgent
  , ppSep = " : "
  }

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask=modm} = (modm, xK_b)

myStatusBar = statusBar "xmobar" myLogHook toggleStrutsKey myConfig

main = do
  spawnDaemons
  myStatusBar >>= xmonad


