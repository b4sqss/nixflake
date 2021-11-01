------------------------------------------------------------------------
---IMPORTS
------------------------------------------------------------------------
    -- Base
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W
import Control.Monad ((>=>), join, liftM, when)   -- For Custom Fullscreen Function
import System.Environment (setEnv)

    -- Data
import Data.Monoid
import Data.Maybe (isJust, maybeToList)
import Data.List
import qualified Data.Map as M
import Data.Semigroup

    -- Utilities
import XMonad.Util.Loggers
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.WorkspaceCompare(getSortByIndex)

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isDialog,  doFullFloat, doCenterFloat, doRectFloat)
import XMonad.Hooks.EwmhDesktops   -- required for xcomposite in obs to work
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.DynamicProperty

    -- Actions
import XMonad.Actions.Minimize (minimizeWindow)
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotAllDown, rotAllUp)
import XMonad.Actions.CopyWindow (kill1, copy, copyToAll, killAllOtherCopies, wsContainingCopies)
import XMonad.Actions.WindowGo (runOrRaise, raiseMaybe)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.CycleWS (toggleWS', moveTo, findWorkspace, shiftTo, WSType( HiddenNonEmptyWS, EmptyWS ))
import qualified XMonad.Actions.Search as S
import XMonad.Actions.FloatKeys
import XMonad.Actions.DynamicWorkspaces(withNthWorkspace)

    -- Layouts
import XMonad.Layout.Gaps
import XMonad.Layout.SimplestFloat
import XMonad.Layout.OneBig
import XMonad.Layout.ResizableTile

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Hooks.InsertPosition
import XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.Reflect (reflectVert, reflectHoriz, REFLECTX(..), REFLECTY(..))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))

------------------------------------------------------------------------
---VARIABLES
------------------------------------------------------------------------
myFont :: String
myFont = "xft:Iosevka:regular:size=12"

myModMask :: KeyMask
myModMask = mod4Mask                    -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "alacritty"                       -- Sets default terminal

myBrowser :: String
myBrowser = "firefox"                  -- Sets firefox as browser for tree select

myEditor :: String
myEditor = "emacsclient -c"    -- Sets vim as editor for tree select

myBorderWidth :: Dimension
myBorderWidth = 1                       -- Sets border width for windows

myNormColor :: String
myNormColor   = "#323d43"                -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#d8caac"               -- Border color of focused windows

altMask :: KeyMask
altMask = mod1Mask                      -- Setting this for use in xprompts

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset   -- Getting no. of windows on current workspace

------------------------------------------------------------------------
---SCRATCHPADS
------------------------------------------------------------------------
-- Allows to have several floating scratchpads running different applications.
-- Import Util.NamedScratchpad.  Bind a key to namedScratchpadSpawnAction.
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "pulse" spawnPulse findPulse managePulse                               -- Pavucontrol
                , NS "calculator" spawnCalculator findCalculator manageCalculator           -- Calculator
                , NS "music" spawnMusic findMusic manageMusic                               -- Music
                ]
  where
    spawnPulse  = "pavucontrol"
    findPulse   = resource =? "pavucontrol"
    managePulse = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w

    spawnCalculator  = "qalculate-gtk"
    findCalculator   = resource =? "qalculate-gtk"
    manageCalculator = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w

    spawnMusic  = "spotify"
    findMusic   = resource =? "spotify"
    manageMusic = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w

------------------------------------------------------------------------
---WORKSPACES
------------------------------------------------------------------------
xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces = map xmobarEscape
               $ ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

------------------------------------------------------------------------
---WINDOW RULES
------------------------------------------------------------------------
-- My Window Rules
myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = (isDialog --> doF W.swapUp)                       -- Bring Dialog Window on Top of Parent Floating Window
               <+> insertPosition Below Newer                    -- Insert New Windows at the Bottom of Stack Area
               <+> namedScratchpadManageHook myScratchPads       -- Adding Rules for Named Scratchpads
               <+> composeAll
               [ (className =? "firefox" <&&> title =? "Library") --> doCenterFloat                             -- Float Firefox Downloads Window to Centre
               , (className =? "Lxappearance") --> doCenterFloat                                                -- Float Lxappearance to Centre
               , isDialog --> doCenterFloat                                                                     -- Float Dialog Windows to Centre
               ]

------------------------------------------------------------------------
---AUTOSTART
------------------------------------------------------------------------
-- Firefox Fullscreen Support
setFullscreenSupport :: X ()
setFullscreenSupport = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    supp <- mapM getAtom ["_NET_WM_STATE_HIDDEN"
                         ,"_NET_WM_STATE_FULLSCREEN"
                         ,"_NET_NUMBER_OF_DESKTOPS"
                         ,"_NET_CLIENT_LIST"
                         ,"_NET_CLIENT_LIST_STACKING"
                         ,"_NET_CURRENT_DESKTOP"
                         ,"_NET_DESKTOP_NAMES"
                         ,"_NET_ACTIVE_WINDOW"
                         ,"_NET_WM_DESKTOP"
                         ,"_NET_WM_STRUT"
                         ]
    io $ changeProperty32 dpy r a c propModeReplace (fmap fromIntegral supp)

-- My Startup Applications
myStartupHook :: X ()
myStartupHook = do
          spawnOnce "xsetroot -cursor_name left_ptr &"                                -- Set Cursor
          spawnOnce "xwallpaper --zoom ~/Pictures/wallpapers/1629218036910.png &"
          spawnOnce "transmission-daemon  &"
          spawnOnce "redshift -l -19.5515:-43.5616 &"
          spawnOnce "dunst &"                                                         -- Start Dunst Notification Daemon
          setFullscreenSupport                                                        -- Adding Firefox Fullscreen Support

------------------------------------------------------------------------
---LAYOUTS
------------------------------------------------------------------------
-- Below implementation makes it easier to use spacingRaw module to set required spacing just by changing the value of i.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw True (Border i i i i) False (Border i i i i) True

myLayoutHook = avoidStruts
               $ windowArrange
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
               $ myDefaultLayout
               where
                myDefaultLayout = smartBorders tall
                                  ||| smartBorders oneBig
                                  ||| noBorders monocle

tall       = renamed [Replace "tall"]
             $ limitWindows 12
             $ gaps [(L,10), (R,10), (U,10), (D,10)]
             $ mySpacing 2
             $ ResizableTall 1 (3/100) (1/2) []

oneBig     = renamed [Replace "oneBig"]
             $ limitWindows 6
             $ gaps [(L,10), (R,10), (U,10), (D,10)]
             $ mySpacing 2
             $ Mirror
             $ mkToggle (single MIRROR)
             $ mkToggle (single REFLECTX)
             $ mkToggle (single REFLECTY)
             $ OneBig (5/9) (8/12)

monocle    = renamed [Replace "monocle"]
             $ limitWindows 20
             $ Full

------------------------------------------------------------------------
---INACTIVE WINDOW TRANSPARENCY
------------------------------------------------------------------------
myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 0.95

------------------------------------------------------------------------
---KEYBINDINGS
------------------------------------------------------------------------
-- Function to toggle floating state on focused window.
toggleFloat w = windows (\s -> if M.member w (W.floating s)
        then W.sink w s
        else (W.float w (W.RationalRect (1/6) (1/6) (2/3) (2/3)) s))


-- My Preferred Keybindings
myKeys :: [(String, X ())]
myKeys =
    -- Xmonad
        [ ("M-S-r", spawn "xmonad --recompile")            -- Recompiles xmonad
        , ("M-q", spawn "xmonad --restart")              -- Restarts xmonad
        , ("M-S-q", io exitSuccess)                        -- Quits xmonad

    -- Workspaces
        , ("M-<Tab>", toggleWS' ["NSP"])                   -- Toggle to the previous WS excluding NSP

    -- Windows
        , ("M-S-c", kill1)                                 -- Kill the currently focused client
        , ("M-M1-a", killAll)                              -- Kill all the windows on current workspace

    -- Floating Windows
        , ("M-<Delete>", withFocused $ windows . W.sink)   -- Push floating window back to tile
        , ("M-S-<Delete>", sinkAll)                        -- Push ALL floating windows back to tile

    -- Windows Navigation
        , ("M-m", windows W.focusMaster)                   -- Move focus to the master window
        , ("M-j", windows W.focusDown)                     -- Move focus to the next window
        , ("M-k", windows W.focusUp)                       -- Move focus to the prev window
        , ("M-S-m", windows W.swapMaster)                  -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown)                    -- Swap the focused window with the next window
        , ("M-S-k", windows W.swapUp)                      -- Swap the focused window with the prev window
        , ("M-<Backspace>", promote)                       -- Moves focused window to master, all others maintain order
        , ("M1-<Tab>", rotAllDown)                         -- Rotate all windows clockwise and keep focus in place
        , ("M1-S-<Tab>", rotAllUp)                         -- Rotate all windows anti-clockwise and keep focus in place

    -- Floating Windows Actions
        , ("M-<Up>", withFocused (keysMoveWindow (0,-10)))                     --  Move window up
        , ("M-<Down>", withFocused (keysMoveWindow (0,10)))                    --  Move window down
        , ("M-<Right>", withFocused (keysMoveWindow (10,0)))                   --  Move window to right
        , ("M-<Left>", withFocused (keysMoveWindow (-10,0)))                   --  Move window to left
        , ("M-S-<Up>", withFocused (keysResizeWindow (0,10) (0,1)))            --  Increase size of window up
        , ("M-S-<Down>", withFocused (keysResizeWindow (0,10) (0,0)))          --  Increase size of window down
        , ("M-S-<Right>", withFocused (keysResizeWindow (10,0) (0,1)))         --  Increase size of window right
        , ("M-S-<Left>", withFocused (keysResizeWindow (10,0) (1,1)))          --  Increase size of window left
        , ("M-C-<Up>", withFocused (keysResizeWindow (0,-10) (0,1)))           --  Decrease size of window up
        , ("M-C-<Down>", withFocused (keysResizeWindow (0,-10) (0,0)))         --  Decrease size of window down
        , ("M-C-<Right>", withFocused (keysResizeWindow (-10,0) (0,1)))        --  Decrease size of window right
        , ("M-C-<Left>", withFocused (keysResizeWindow (-10,0) (1,1)))         --  Decrease size of window left

    -- Layouts
        , ("M-C-m", sendMessage $ Toggle NBFULL)                               -- Toggle Monocle Layout
        , ("M-S-<Space>", sendMessage NextLayout)                              -- Switch to next layout
        , ("M-C-n", sendMessage $ Toggle NOBORDERS)                            -- Toggles noborder
        , ("M-C-f", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts)   -- Toggles fullscreen
        , ("M-<Space>", withFocused toggleFloat)                               -- Toggle a window between floating and tiling states
        , ("M-S-x", sendMessage $ Toggle REFLECTX)                             -- Swap master/stack positions horizontally
        , ("M-S-y", sendMessage $ Toggle REFLECTY)                             -- Swap master/stack positions vertically
        , ("M1-S-m", sendMessage $ Toggle MIRROR)                              -- Toggle layout between vertical and horizontal states
        , ("M-C-M1-<Up>", sendMessage Arrange)
        , ("M-C-M1-<Down>", sendMessage DeArrange)
        , ("M-h", sendMessage Shrink)                                          -- Resize horizontally to left
        , ("M-l", sendMessage Expand)                                          -- Resize horizontally to right
        , ("M-C-j", sendMessage MirrorShrink)                                  -- Resize vertically down
        , ("M-C-k", sendMessage MirrorExpand)                                  -- Resize vertically up

    -- Scratchpads
        , ("M1-C-p", namedScratchpadAction myScratchPads "pulse")              -- Pavucontrol
        , ("M1-C-c", namedScratchpadAction myScratchPads "calculator")         -- Qalculator-gtk
        , ("M1-C-s", namedScratchpadAction myScratchPads "music")              -- Spotify

    -- Bar Toggle
        , ("M-b", sendMessage ToggleStruts)         -- Hide Xmobar

    -- Menus
        , ("M-p", spawn "rofi -show drun")          -- rofi drun(run applications)

    -- My Applications
        , ("M-<Return>", spawn myTerminal)       -- Terminal
        , ("M-o", spawn "firefox -P Normal")                       -- Firefox browser
        , ("M-S-o", spawn "firefox -P contas")                       -- Firefox browser
        , ("M-a", spawn "emacs")                           -- Emacs text editor
        , ("M-z", spawn "zathura")                         -- Zathura pdf reader
        ]

------------------------------------------------------------------------
---SPOTIFY SCRATCHPAD SETTINGS
------------------------------------------------------------------------
myHandleEventHook :: Event -> X All
myHandleEventHook = dynamicPropertyChange "WM_NAME" (title =? "Spotify" --> floating)
                where floating = customFloating $ W.RationalRect l t w h
                             where
                                h = 0.9
                                w = 0.9
                                t = 0.95 -h
                                l = 0.95 -w

------------------------------------------------------------------------
---MAIN
------------------------------------------------------------------------
main :: IO ()
main = do
    -- Launching xmobar
    xmproc <- spawnPipe "xmobar /home/basqs/.config/nixpkgs/configs/xmonad/xmobar.hs"
    -- Xmonad Settings
    xmonad $ ewmh def
        { manageHook         = myManageHook <+> manageDocks
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , handleEventHook    = docksEventHook <+> fullscreenEventHook <+> myHandleEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , focusFollowsMouse  = True
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook            = myLogHook  <+> dynamicLogWithPP xmobarPP
                        { ppOutput      = \x -> hPutStrLn xmproc x
                        , ppCurrent     = xmobarColor "#dbbc7f" "" . wrap "[" "]"   -- Current workspace in xmobar
                        , ppHidden      = xmobarColor "#4b565c" ""                  -- Hidden workspaces in xmobar
                        , ppTitle       = xmobarColor "#a7c080" "" . shorten 61     -- Title of active window in xmobar
                        , ppSep         = "<fc=#d3c6aa> | </fc>"                    -- Separators in xmobar
                        , ppUrgent      = xmobarColor "#e67e80" "" . wrap "!" "!"   -- Urgent workspace
                        , ppExtras      = [windowCount]                             -- No. of windows current workspace
                        , ppOrder       = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        }
        } `additionalKeysP`    myKeys

---------------------------------------------------------------------------- EOF --------------------------------------------------------------------------------------
