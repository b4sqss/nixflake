------------------------------------------------------------------------
---IMPORTS
------------------------------------------------------------------------
import Control.Monad (join, liftM, when, (>=>)) -- For Custom Fullscreen Function

import Data.List
import qualified Data.Map as M
import Data.Maybe (isJust, maybeToList)
import Data.Monoid
import Data.Semigroup
import System.Environment (setEnv)
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)
import XMonad

import XMonad.Actions.CopyWindow (copy, copyToAll, kill1, killAllOtherCopies, wsContainingCopies)
import XMonad.Actions.CycleWS (WSType (EmptyWS, HiddenNonEmptyWS), findWorkspace, moveTo, shiftTo, toggleWS')
import XMonad.Actions.DynamicWorkspaces (withNthWorkspace)
import XMonad.Actions.FloatKeys
import XMonad.Actions.Minimize (minimizeWindow)
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotAllDown, rotAllUp)
import qualified XMonad.Actions.Search as S
import XMonad.Actions.WindowGo (raiseMaybe, runOrRaise)
import XMonad.Actions.WithAll (killAll, sinkAll)
import XMonad.Hooks.DynamicLog (PP (..), dynamicLogWithPP, shorten, wrap, xmobarColor, xmobarPP)
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man

import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts, docksEventHook, manageDocks)
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, doRectFloat, isDialog)
import XMonad.Layout.Gaps
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.MultiToggle (EOT (EOT), Toggle (..), mkToggle, single, (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Layout.Reflect (REFLECTX (..), REFLECTY (..), reflectHoriz, reflectVert)
import XMonad.Layout.Renamed (Rename (CutWordsLeft, Replace), renamed)
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger (WindowArrangerMsg (..), windowArrange)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Loggers
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare (getSortByIndex)

------------------------------------------------------------------------
---VARIABLES
------------------------------------------------------------------------
myFont :: String
myFont = "xft:JetBrains Mono:size=12"

myModMask :: KeyMask
myModMask = mod4Mask -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "kitty" -- Sets default terminal

myBrowser :: String
myBrowser = "firefox" -- Sets firefox as browser for tree select

myEditor :: String
myEditor = "emacsclient -cn" -- Sets vim as editor for tree select

myBorderWidth :: Dimension
myBorderWidth = 1 -- Sets border width for windows

myNormColor :: String
myNormColor = "#1d1f21" -- Border color of normal windows

myFocusColor :: String
myFocusColor = "#f0c674" -- Border color of focused windows

altMask :: KeyMask
altMask = mod1Mask -- Setting this for use in xprompts

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset -- Getting no. of windows on current workspace

------------------------------------------------------------------------
---WORKSPACES
------------------------------------------------------------------------
xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
    doubleLts '<' = "<<"
    doubleLts x = [x]

myWorkspaces :: [String]
myWorkspaces =
  map xmobarEscape $ ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

------------------------------------------------------------------------
---WINDOW RULES
------------------------------------------------------------------------
-- My Window Rules
myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook =
  (isDialog --> doF W.swapUp) -- Bring Dialog Window on Top of Parent Floating Window
    <+> insertPosition Below Newer -- Insert New Windows at the Bottom of Stack Area
    <+> composeAll
      [ (className =? "firefox" <&&> title =? "Library") --> doCenterFloat, -- Float Firefox Downloads Window to Centre
        (className =? "Lxappearance") --> doCenterFloat, -- Float Lxappearance to Centre
        (className =? "gnome-pomodoro" <&&> title =? "Pomodoro") --> doCenterFloat, -- Float Lxappearance to Centre
        isDialog --> doCenterFloat -- Float Dialog Windows to Centre
      ]

------------------------------------------------------------------------
---AUTOSTART
------------------------------------------------------------------------
-- My Startup Applications
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "xsetroot -cursor_name left_ptr" -- Set Cursor
  spawnOnce "xrdb ~/.Xresources"
  spawnOnce "sxhkd"
  spawnOnce "dunst"
  spawnOnce "xwallpaper --zoom ~/Pictures/wallpapers/1628151473203.jpg"

------------------------------------------------------------------------
---LAYOUTS
------------------------------------------------------------------------
-- Below implementation makes it easier to use spacingRaw module to set required spacing just by changing the value of i.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw True (Border i i i i) False (Border i i i i) True

myLayoutHook = avoidStruts $ windowArrange $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ myDefaultLayout
  where
    myDefaultLayout = smartBorders tall ||| smartBorders oneBig ||| noBorders monocle

    tall = renamed [Replace "tall"] $ limitWindows 12 $ gaps [(L, 10), (R, 10), (U, 10), (D, 10)] $ mySpacing 2 $ ResizableTall 1 (5 / 100) (1 / 2) []

    oneBig = renamed [Replace "oneBig"] $ limitWindows 6 $ gaps [(L, 10), (R, 10), (U, 10), (D, 10)] $mySpacing 2 $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (5 / 9) (8 / 12)

    monocle = renamed [Replace "monocle"] $ limitWindows 20 $ Full

    ------------------------------------------------------------------------
    ---INACTIVE WINDOW TRANSPARENCY
    ------------------------------------------------------------------------
myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
  where
    fadeAmount = 0.95

    ------------------------------------------------------------------------
    ---KEYBINDINGS
    ------------------------------------------------------------------------
    -- Function to toggle floating state on focused window.
toggleFloat w = windows
    ( \s ->
      if M.member w (W.floating s)
      then W.sink w s
      else (W.float w (W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)) s)
    )

    -- My Preferred Keybindings
myKeys :: [(String, X ())]
myKeys =
    -- Xmonad
    [ ("M-S-r", spawn "xmonad --recompile"), -- Recompiles xmonad
    ("M-q", spawn "xmonad --restart"), -- Restarts xmonad
    ("M-S-q", io exitSuccess), -- Quits xmonad

    -- Workspaces
    ("M-<Tab>", toggleWS' ["NSP"]), -- Toggle to the previous WS excluding NSP

    -- Windows
    ("M-S-c", kill1), -- Kill the currently focused client
    ("M-M1-a", killAll), -- Kill all the windows on current workspace

    -- Floating Windows
    ("M-<Delete>", withFocused $ windows . W.sink), -- Push floating window back to tile
    ("M-S-<Delete>", sinkAll), -- Push ALL floating windows back to tile

    -- Windows Navigation
    -- ("M-m", windows W.focusMaster), -- Move focus to the master window
    ("M-j", windows W.focusDown), -- Move focus to the next window
    ("M-k", windows W.focusUp), -- Move focus to the prev window
    ("M-S-j", windows W.swapDown), -- Swap the focused window with the next window
    ("M-S-k", windows W.swapUp), -- Swap the focused window with the prev window
    ("M-<Backspace>", promote), -- Moves focused window to master, all others maintain order

    -- Floating Windows Actions
    ("M-<Up>", withFocused (keysMoveWindow (0, -10))), --  Move window up
    ("M-<Down>", withFocused (keysMoveWindow (0, 10))), --  Move window down
    ("M-<Right>", withFocused (keysMoveWindow (10, 0))), --  Move window to right
    ("M-<Left>", withFocused (keysMoveWindow (-10, 0))), --  Move window to left
    ("M-S-<Up>", withFocused (keysResizeWindow (0, 10) (0, 1))), --  Increase size of window up
    ("M-S-<Down>", withFocused (keysResizeWindow (0, 10) (0, 0))), --  Increase size of window down
    ("M-S-<Right>", withFocused (keysResizeWindow (10, 0) (0, 1))), --  Increase size of window right
    ("M-S-<Left>", withFocused (keysResizeWindow (10, 0) (1, 1))), --  Increase size of window left
    ("M-C-<Up>", withFocused (keysResizeWindow (0, -10) (0, 1))), --  Decrease size of window up
    ("M-C-<Down>", withFocused (keysResizeWindow (0, -10) (0, 0))), --  Decrease size of window down
    ("M-C-<Right>", withFocused (keysResizeWindow (-10, 0) (0, 1))), --  Decrease size of window right
    ("M-C-<Left>", withFocused (keysResizeWindow (-10, 0) (1, 1))), --  Decrease size of window left

    -- Layouts
    ("M-S-<Space>", sendMessage NextLayout), -- Switch to next layout
    ("M-C-n", sendMessage $ Toggle NOBORDERS), -- Toggles noborder
    ("M-S-f", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts), -- Toggles fullscreen
    ("M-t", withFocused toggleFloat), -- Toggle a window between floating and tiling states
    ("M-h", sendMessage Shrink), -- Resize horizontally to left
    ("M-l", sendMessage Expand), -- Resize horizontally to right
    ("M-C-j", sendMessage MirrorShrink), -- Resize vertically down
    ("M-C-k", sendMessage MirrorExpand), -- Resize vertically up

    -- Bar Toggle
    ("M-b", sendMessage ToggleStruts), -- Hide Xmobar

    -- Menus
    ("M-p", spawn "dmenu_run -fn 'JetBrains Mono nerd font' -nb '#1d1f21' -nf '#969896' -sb '#f0c674' -sf '#1d1f21'"), -- rofi drun(run appications)
    ("M-m", manPrompt def), -- rofi drun(run applications)

    -- My Applications
    ("M-<Return>", spawn myTerminal), -- Terminal
    ("M-o", spawn "firefox"), -- Firefox browser
    ("M-w", spawn "brave"), -- brave browser
    ("M-a", spawn myEditor), -- Emacs text editor
    ("M-d", spawn "emacsclient -c -a 'emacs' --eval '(dired nil)'"), -- Emacs text editor
    ("<Print>", spawn "scrot  -e 'mv $f ~/Pictures/screenshots'"), -- Screenshots

    --My function keys
    ("<XF86AudioPlay>", spawn "mocp --play"),
    ("<XF86AudioPrev>", spawn "mocp --previous"),
    ("<XF86AudioNext>", spawn "mocp --next"),
    ("<XF86AudioMute>", spawn "amixer set Master toggle"),
    ("<XF86AudioRaiseVolume>", spawn "pamixer -i 5"),
    ("<XF86AudioLowerVolume>", spawn "pamixer -d 5"),
    ("<XF86AudioMute>", spawn "pamixer -t"),
    ("<XF86MonBrightnessDown>", spawn "brightnessctl set 10%-"),
    ("<XF86MonBrightnessUp>", spawn "brightnessctl set +10%")
    ]

    -- where nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
    --       nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))

    ------------------------------------------------------------------------
    ---MAIN
    ------------------------------------------------------------------------
main :: IO ()
main = do
    -- Launching xmobar
    xmproc <- spawnPipe "xmobar /home/basqs/.xmonad/xmobar.hs"
    -- Xmonad Settings
    xmonad $ ewmh $ def
      { manageHook = myManageHook <+> manageDocks,
        startupHook = myStartupHook,
        layoutHook = myLayoutHook,
        handleEventHook = docksEventHook <+> fullscreenEventHook,
        modMask = myModMask,
        terminal = myTerminal,
        focusFollowsMouse = True,
        borderWidth = myBorderWidth,
        normalBorderColor = myNormColor,
        focusedBorderColor = myFocusColor,
        logHook =
          myLogHook
          <+> dynamicLogWithPP
          xmobarPP
          { ppOutput = \x -> hPutStrLn xmproc x,
          ppCurrent = xmobarColor "#f0c674" "" . wrap "[" "]", -- Current workspace in xmobar
          ppHidden = xmobarColor "#969896" "", -- Hidden workspaces in xmobar
          ppTitle = xmobarColor "#b5bd68" "" . shorten 61, -- Title of active window in xmobar
          ppSep = "<fc=#969896> | </fc>", -- Separators in xmobar
          ppUrgent = xmobarColor "#cc6666" "" . wrap "!" "!", -- Urgent workspace
          -- ppExtras = [windowCount], -- No. of windows current workspace
          ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
        }
      }
      `additionalKeysP` myKeys
  
