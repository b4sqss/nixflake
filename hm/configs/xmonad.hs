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
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.WorkspaceCompare(getSortByIndex)

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isDialog,  doFullFloat, doCenterFloat, doRectFloat)
import XMonad.Hooks.EwmhDesktops   -- required for xcomposite in obs to work
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
--- import XMonad.Layout.Gaps
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ResizableTile
import XMonad.Layout.BinarySpacePartition

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

myEditor :: String
myEditor = "emacsclient -c -n"
-- myEditor = "emacsclient -c -n -e '(switch-to-buffer nil)'"    -- Sets vim as editor for tree select

myBorderWidth :: Dimension
myBorderWidth = 0                       -- Sets border width for windows

altMask :: KeyMask
altMask = mod1Mask                      -- Setting this for use in xprompts

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset   -- Getting no. of windows on current workspace

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
               <+> composeAll
               [ (className =? "firefox" <&&> title =? "Library") --> doCenterFloat                             -- Float Firefox Downloads Window to Centre
               , (className =? "mpv") --> doCenterFloat
               , (className =? "sxiv") --> doCenterFloat
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
          spawnOnce "xsetroot -cursor_name left_ptr &"
          spawnOnce "xautolock -time 15 -locker 'sh ~/.local/bin/lock.sh'"
          spawnOnce "dunst &"
          spawnOnce "picom"
          spawnOnce "xwallpaper --zoom ~/Pics/wallpapers/by_upload2_2560.jpg"
          spawnOnce "xautolock -time 15 -locker 'sh ~/.local/bin/lock.sh'"
          spawnOnce "emacs --daemon"
          spawnOnce "xrdb ~/.Xresources"
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
                myDefaultLayout = noBorders tall ||| bsp ||| noBorders monocle

tall       = renamed [Replace "tall"]
             $ limitWindows 12
             --- $ gaps [(L,10), (R,10), (U,10), (D,10)]
             $ mySpacing 2
             $ ResizableTall 1 (3/100) (1/2) []

bsp        = renamed [Replace "BSP"]
             $ mySpacing 8
             $ emptyBSP

monocle    = renamed [Replace "monocle"]
             $ limitWindows 20
             $ Full

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
        , ("M-S-f", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts)   -- Toggles fullscreen
        , ("M-t", withFocused toggleFloat)                               -- Toggle a window between floating and tiling states
        , ("M-S-x", sendMessage $ Toggle REFLECTX)                             -- Swap master/stack positions horizontally
        , ("M-S-y", sendMessage $ Toggle REFLECTY)                             -- Swap master/stack positions vertically
        , ("M1-S-m", sendMessage $ Toggle MIRROR)                              -- Toggle layout between vertical and horizontal states
        , ("M-C-M1-<Up>", sendMessage Arrange)
        , ("M-C-M1-<Down>", sendMessage DeArrange)
        , ("M-h", sendMessage Shrink)                                          -- Resize horizontally to left
        , ("M-l", sendMessage Expand)                                          -- Resize horizontally to right
        , ("M-C-j", sendMessage MirrorShrink)                                  -- Resize vertically down
        , ("M-C-k", sendMessage MirrorExpand)                                  -- Resize vertically up


    -- Bar Toggle
        , ("M-b", sendMessage ToggleStruts)         -- Hide Xmobar

    -- Menus
        , ("M-p", spawn "dmenu_run -i -p 'M-x' -fn Iosevka -sf '#002b36' -sb '#839496' -nf '#839496' -nb '#002b36'")          -- rofi drun(run applications)

    -- My Applications
        , ("M-<Return>", spawn myTerminal)       -- Terminal
        , ("M-o", spawn "firefox")                       -- Firefox browser
        , ("M-S-o", spawn "qutebrowser")                       -- Firefox browser
        , ("M-w", spawn "brave")                       -- Firefox browser
        , ("M-z", spawn "zathura")                         -- Zathura pdf reader

        -- XF86
        , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 5%-")
        , ("<XF86MonBrightnessUp>", spawn "brightnessctl set +5%")

        , ("<XF86AudioMute>", spawn "pamixer -t")
        , ("<XF86AudioLowerVolume>", spawn "pamixer -d 5")
        , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 5")

        , ("<Print>", spawn "import -window root ~/Pics/screenshots/$(date '+%Y-%d-%m-%H:%M').jpg")

        , ("M-C-5", spawn "dunstify -u CRIT 'gravando' -t 800 && ffmpeg -f x11grab -s 1920x1080 -i :1 $HOME/Docs/videos/$(date +'%d_%m_%Y_%I_%M').mp4")
        , ("M-C-6", spawn "pkill ffmpeg && dunstify -u LOW 'screencast saved'")

        , ("M-a", spawn myEditor)                           -- Emacs text editor
        , ("M-d", spawn "emacsclient -c -a 'emacs' --eval '(dired nil)'")
        , ("M-C-a", spawn "emacsclient -c -a 'emacs' --eval '(org-agenda-list nil)'")
        , ("M-S-a", spawn "emacs")                           -- Emacs text editor
        ]

------------------------------------------------------------------------
myLayoutPrinter :: String -> String
myLayoutPrinter "tall" = xmobarColor "green" "" "<icon=layout_tall.xbm/>"
myLayoutPrinter x = xmobarColor "white" "" x
------------------------------------------------------------------------
---MAIN
------------------------------------------------------------------------
main :: IO ()
main = do
    -- Launching xmobar
    xmproc <- spawnPipe "xmobar"
    -- Xmonad Settings
    xmonad $ ewmh def
        { manageHook         = myManageHook <+> manageDocks
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , handleEventHook    = docksEventHook <+> fullscreenEventHook <+> handleEventHook def
        , modMask            = myModMask
        , terminal           = myTerminal
        , focusFollowsMouse  = True
        , borderWidth        = myBorderWidth
        , logHook            = dynamicLogWithPP xmobarPP
                        { ppOutput      = \x -> hPutStrLn xmproc x
                        , ppCurrent     = xmobarColor "#839496" "" -- . wrap "[" "]"   -- Current workspace in xmobar
                        , ppHidden      = xmobarColor "#586e75" ""                  -- Hidden workspaces in xmobar
                        , ppTitle       = xmobarColor "#839496" "" . shorten 75     -- Title of active window in xmobar
                        , ppLayout      = myLayoutPrinter
                        , ppSep         = " "
                        }
        } `additionalKeysP`    myKeys
