-- Base
import XMonad hiding ((|||))
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.MouseResize
import XMonad.Actions.Navigation2D

-- Other modules
import qualified Colors as C

-- Data

import Data.List (sort)
import qualified Data.Map as M
import Data.Maybe (fromJust)

-- Hooks
import XMonad.Hooks.EwmhDesktops (ewmh) -- ewmhFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import XMonad.Hooks.ManageHelpers (composeOne, doFullFloat, isDialog, isFullscreen, (-?>))

-- Layouts
import XMonad.Layout.Reflect (reflectVert)
import XMonad.Layout.Tabbed

-- Layouts modifiers
import qualified XMonad.Layout.LayoutCombinators as LC
import XMonad.Layout.MultiToggle as MT (Toggle (..), mkToggle, single)
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.StackTile
import XMonad.Layout.WindowArranger (windowArrange)

-- Utilities
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Util.SpawnOnce

main :: IO ()
main =
    xmonad
        . ewmh
--        . ewmhFullscreen
        . docks
        . navFunction
        $ def
            { manageHook = myManageHook <+> manageHook def
            , modMask = myModMask
            , terminal = myTerminal
            , startupHook = myStartupHook
            , handleEventHook = handleEventHook def
            , layoutHook = myLayoutHook
            , logHook = logHook def
            , workspaces = myWorkspaces
            , borderWidth = myBorderWidth
            , clickJustFocuses = False
            , focusFollowsMouse = False
            , normalBorderColor = C.color7
            , focusedBorderColor = C.color2
            }
            `removeKeysP` removeKeys'
            `additionalKeysP` myKeys

myModMask :: KeyMask
myModMask = mod4Mask -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "kitty" -- Sets default terminal

myBrowser :: String
myBrowser = "firefox-devedition" -- Sets qutebrowser as browser

myEditor :: String
myEditor = "emacs" -- Sets emacs as editor

lockScreen :: String
lockScreen = "i3lock-color -k -u -c 00000000 --date-str='%A, %B %Y' --time-color=ff005acc --date-color=ff005acc --time-size=60 --date-size=20"

polybar :: String
polybar = "sh /home/skykanin/dotfiles/dots/polybar/launch.sh"

myBorderWidth :: Dimension
myBorderWidth = 3 -- Sets border width for windows

mySpacing :: Int
mySpacing = 4

-- Modify XConfig to enable 2D navigation
navFunction :: XConfig a -> XConfig a
navFunction = navigation2DP navConfig ("k", "h", "j", "l") [("M-", windowGo), ("M-S-", windowSwap)] False
 where
   navConfig = def { defaultTiledNavigation = centerNavigation }

tab =
    renamed [Replace "tab"]
        . lessBorders Screen
        . smartSpacingWithEdge mySpacing
        $ tabbed shrinkText theme
  where
    theme =
        def
            { activeColor = C.color2
            , inactiveColor = C.color6
            , urgentColor = C.color3
            , activeTextColor = C.color8
            , inactiveTextColor = C.color8
            , urgentTextColor = C.color3
            , activeBorderWidth = 0
            , inactiveBorderWidth = 0
            , urgentBorderWidth = 0
            , fontName = "xft:Iosevka Nerd Font:style=Bold:size=13"
            , decoHeight = 25
            }

tiled =
    renamed [Replace "tall"]
        . lessBorders Screen
        . smartSpacingWithEdge mySpacing
        $ Tall 1 (3 / 100) (1 / 2)

stackTile =
    renamed [Replace "stack"]
        . lessBorders Screen
        . smartSpacingWithEdge mySpacing
        . reflectVert
        $ StackTile 1 (3 / 100) (1 / 2)

-- The layout hook
myLayoutHook = avoidStruts . smartBorders . mouseResize . windowArrange . toggleFullscreen $ myLayouts
  where
    myLayouts = tiled LC.||| stackTile LC.||| tab
    toggleFullscreen = mkToggle (single NBFULL)

workspaceMap :: M.Map String String
workspaceMap =
    M.fromList
        [ ("term", "1")
        , ("web", "2")
        , ("dev", "3")
        , ("comm", "4")
        , ("music", "5")
        , ("misc", "6")
        , ("seven", "7")
        , ("eight", "8")
        , ("nine", "9")
        ]

myWorkspaces :: [String]
myWorkspaces = sort . M.elems $ workspaceMap

myStartupHook :: X ()
myStartupHook = sequence_ $ fmap spawnOnce runOnce <> fmap spawn run
  where
    runOnce =
        [ myTerminal <> " --class 'kitty-startup'"
        , myBrowser <> " --class 'ff-startup'"
        , myEditor
        , "Discord"
        , "spotifywm"
        , "qbittorrent"
        , "nm-applet"
        , "blueman-applet"
        , "flameshot"
        , "sleep 4; " <> lockScreen
        , polybar
        ]
    run =
        [ "xset s off"
        , "xset r rate 280 80"
        , "xsetroot -cursor_name left_ptr" -- set cursor
        , "wal -R"
        ]

myManageHook :: ManageHook
myManageHook =
    composeAll
        [ className =? "confirm" --> doFloat
        , className =? "file_progress" --> doFloat
        , className =? "dialog" --> doFloat
        , className =? "download" --> doFloat
        , className =? "kitty-startup" --> doShift (lookupWs "term")
        , className =? "ff-startup" --> doShift (lookupWs "web")
        , className =? myEditor --> doShift (lookupWs "dev")
        , className =? "discord" --> doShift (lookupWs "comm")
        , className =? "Spotify" --> doShift (lookupWs "music")
        , className =? "qBittorrent" --> doShift (lookupWs "misc")
        , (className =? "ff-startup" <&&> resource =? "Dialog") --> doFloat -- Float Firefox Dialog
        , (className =? "firefox-developer-edition" <&&> resource =? "Dialog") --> doFloat -- Float Firefox Dialog
        -- , isFullscreen --> doFullFloat
        , isDialog --> doFloat
        ]
  where
    lookupWs = fromJust . (`M.lookup` workspaceMap)

-- START_KEYS
myKeys :: [(String, X ())]
myKeys =
    [ ("M-C-r", spawn "xmonad --recompile") -- Recompiles xmonad
    , ("M-S-r", spawn "xmonad --restart") -- Restarts xmonad
    , ("M-S-q", kill1) -- Quit focused window
    , ("M-S-x", spawn lockScreen) -- Lock screen
    , ("M-<Return>", spawn myTerminal)
    , ("M-b", spawn myBrowser)
    , ("M-d", spawn "rofi -show run") -- Open rofi run app
    , ("M-S-s", spawn "maim -s 2>/dev/null | xclip -sel c -t image/png") -- Screen shot
    , -- Layout
      ("M-f", sendMessage $ Toggle NBFULL) -- Toggle fullscreen layout
    , ("M-e", sendMessage $ LC.JumpToLayout "tall") -- Jump to tiled layout
    , ("M-s", sendMessage $ LC.JumpToLayout "stack") -- Jump to stacked layout
    , ("M-w", sendMessage $ LC.JumpToLayout "tab") -- Jump to tabbed layout
    , ("M-<Space>", withFocused $ windows . W.sink) -- Sink floating window back into tiling
    , -- Workspaces
      ("M-.", nextScreen) -- Switch focus to next monitor
    , ("M-,", prevScreen) -- Switch focus to prev monitor
    , -- Media keys
      ("<Insert>", spawn "pamixer -i 10") -- Increase volume
    , ("<Delete>", spawn "pamixer -d 10") -- Decrease volume
    , ("C-<Insert>", spawn "playerctl play-pause") -- Play/Pause
    , ("S-<Insert>", spawn "playerctl next") -- Next song
    , ("S-<Delete>", spawn "playerctl previous") -- Previous song
    , -- Multimedia Keys
      ("<XF86AudioPlay>", spawn "playerctl play")
    , ("<XF86AudioPrev>", spawn "playerctl previous")
    , ("<XF86AudioNext>", spawn "playerctl next")
    , ("<XF86AudioPause>", spawn "playerctl pause")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
    ]

removeKeys' :: [String]
removeKeys' =
    [ "M-h"
    , "M-l"
    , "M-j"
    , "M-k"
    , "M-<Space>"
    , --, "M-S-<Space>" -- Used to recompute layout
      "M-<Return>"
    , "M-S-<Return>"
    , "M-m"
    , "M-S-m"
    ]
