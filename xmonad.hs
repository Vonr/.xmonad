import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

import XMonad.Actions.CopyWindow (kill1, copyToAll, killAllOtherCopies)
import XMonad.Actions.CycleWS (Direction1D(..), WSType(..))
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotAllDown)
import XMonad.Actions.WithAll (sinkAll, killAll)

import XMonad.Config.Desktop

import Data.Char (toLower)
import Data.Foldable (traverse_)
import Data.List (isInfixOf)
import Data.Monoid

import GHC.Base (Applicative (liftA2), join, liftA3)

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (docks, manageDocks, avoidStruts, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ResizableTile

import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange)
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts)
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

import XMonad.Util.EZConfig (mkKeymap, additionalMouseBindings)
import qualified XMonad.Util.Hacks as Hacks
import XMonad.Util.Run (spawnPipe)
import qualified XMonad.Util.NamedScratchpad as N
import XMonad.Util.SpawnOnce
import XMonad.Actions.DynamicWorkspaceOrder (moveTo, shiftTo, getSortByOrder)
import XMonad.Actions.WorkspaceNames (swapTo', workspaceNamesEwmh)

browser :: String
browser = "chromium --enable-features=VaapiVideoDecoder --ignore-gpu-blocklist --disable-features=UseOzonePlatform --use-gl=desktop --enable-gpu-rasterization --enable-zero-copy"

myStartupHook :: X()
myStartupHook = do
  setWMName "LG3D"
  traverse_ spawnOnce
    [ "dbus-update-activation-environment --systemd DBUS_SESSION_BUS_ADDRESS DISPLAY XAUTHORITY" -- fix xdg-portal-desktop not working
    , "lxsession"
    , "{ lxappearance & }; sleep 0.5; killall lxappearance" -- Fix cursor
    , "feh --bg-scale ~/.local/share/wallpapers/haskell-gradient.png"
    , "conky"
    , "greenclip daemon"
    , "dunst"
    , "picom"
    ]
  spawnOnOnce "2" "discord-canary"
  spawnOnOnce "3" "whatsapp-nativefier"
  spawnOnOnce "1" browser

term :: String
term = "alacritty" -- There appears to be no good way to know the user's default terminal.

myScratchPads :: [N.NamedScratchpad]
myScratchPads =
  [ termApp "btop"           centeredLarge
  , termApp "calcurse"       centeredLarge
  , script  "set-volume"     centeredIBox
  , script  "set-brightness" centeredIBox
  , script  "xmonad_keys"    centeredLarge
  ] ++ terms 1 9
  where
    find         = (title =?)
    termApp' x y = N.NS x (term ++ " -o window.opacity=1 -t " ++ x ++ " -e " ++ y) $ find x
    termApp      = join termApp'
    script   x   = termApp' x $ "~/.xmonad/scripts/" ++ x ++ ".sh"
    centered w h = N.customFloating $ W.RationalRect x y w h
      where
        x  = (1 - w) / 2
        y  = (1 - h) / 2

    centeredLarge = centered 0.9 0.9
    centeredIBox  = centered 0.4 0.175

    spawnTerm x = term ++ " -t term" ++ show x
    findTerm  x = find $ "term" ++ show x
    term'       = flip (liftA3 N.NS (("term" ++) . show) spawnTerm findTerm) centeredLarge

    terms :: Int -> Int -> [N.NamedScratchpad]
    terms x y = fmap term' [x..y]

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
  ([ float'    title    "Emulator"
   , floatRes "firefox" "Dialog"
   , isFullscreen --> doFullFloat
   , className =~ "discord" --> doShift "2"
   , className =~ "whatsapp" --> doShift "3"
  ] ++ fmap (float' className)
    [ "confirm"
    , "file_progress"
    , "dialog"
    , "download"
    , "error"
    , "notification"
    , "pinentry-gtk-2"
    , "splash"
    , "toolbar"
    ]
  ) <> N.namedScratchpadManageHook myScratchPads <> manageSpawn
  where
    q =~ x = (\ z -> low x `isInfixOf` low z) <$> q
      where
        low = fmap toLower

    float'   x y = (x =~ y) --> doFloat
    floatRes x y = (className =~ x <&&> resource =~ y) --> doFloat

nonNSP :: WSType
nonNSP = WSIs $ pure $ (/=) "NSP" . W.tag

myKeys :: [(String, X())]
myKeys =
-- START_KEYS
  -- KB_GROUP XMonad
  [ on  "M-C-r"   $ spawn "xmonad --recompile"                -- Recompiles xmonad
  , on  "M-S-r"   $ spawn "xmonad --restart" <> myStartupHook -- Restarts xmonad
  , on  "M-C-S-r" $ io exitSuccess                            -- Recompiles and restarts xmonad
  , nsp "M-S-/"    "xmonad_keys"                              -- Get list of keybindings

  -- KB_GROUP Rofi
  , cmd "M-p"   "rofi -show drun"                                                             -- rofi
  , cmd "M-c"   "rofi -show calc -no-show-match -no-sort"                                     -- rofi-calc
  , cmd "M-q"   "rofi -show window"                                                           -- rofi-calc
  , cmd "M-e"   "rofi -show emoji"                                                            -- rofi-emoji
  , sh' "M-S-e" "rofi-nerdfont"                                                               -- rofi-nerdfont
  , sh' "M-C-e" "rofi-unicode"                                                                -- rofi-unicode
  , sh' "M-S-w" "rofi-wifi-menu"                                                              -- rofi-wifi-menu
  , sh' "M-S-q" "rofi-power-menu"                                                             -- rofi power menu
  , cmd "M-v"   "rofi -modi 'clipboard:greenclip print' -show clipboard -run-command '{cmd}'" -- greenclip

  -- KB_GROUP Useful Applications
  , cmd "M-b"         browser
  , cmd "M-<Return>"  term
  , cmd "M1-d"       "discord-canary"
  , cmd "M-S-s"      "sleep 0.3; xset -display $DISPLAY dpms force off"
  , cmd "M-f"        "pcmanfm"
  , ter "M1-f" 1.0   "lf"
  , ter "M1-v" 1.0   "nvim"
  , cmd "M-l"        "xtrlock"
  , sh' "<Print>"    "screenshot"
  , sh' "C-<Print>"  "screenshotflame"
  , sh' "S-<Print>"  "screenshotfull"

  -- KB_GROUP Window Management
  , on  "M-C-a" $  windows copyToAll    -- Copy current window to all workspaces
  , on  "M-S-c"    kill1                -- Kill the currently focused client
  , on  "M-S-a"    killAll              -- Kill all windows on current workspace
  , on  "M-C-c"    killAllOtherCopies   -- Kill all windows on current workspace
  , sh  "M--"     "xbasket" ["hide"]   -- Hide window
  , sh  "M-="     "xbasket" ["select"] -- Select hidden window

  -- KB_GROUP Volume Control
  , cmd "<XF86AudioRaiseVolume>" "pactl set-sink-volume @DEFAULT_SINK@ +1%; pactl get-sink-volume @DEFAULT_SINK@ | sed -r '{N; s/^(\\w*\\W+){4}([0-9]+%).*/\\2/}' | xargs -I '{}' notify-send -i '/usr/share/notify-osd/icons/hicolor/scalable/status/notification-audio-volume-medium.svg' -t 1000 -a 'sysnotif' -h int:value:{} 'Volume' 'Volume increased to'"
  , cmd "<XF86AudioLowerVolume>" "pactl set-sink-volume @DEFAULT_SINK@ -1%; pactl get-sink-volume @DEFAULT_SINK@ | sed -r '{N; s/^(\\w*\\W+){4}([0-9]+%).*/\\2/}' | xargs -I '{}' notify-send -i '/usr/share/notify-osd/icons/hicolor/scalable/status/notification-audio-volume-medium.svg' -t 1000 -a 'sysnotif' -h int:value:{} 'Volume' 'Volume decreased to'"
  , cmd "<XF86AudioMute>"        "pactl set-sink-mute @DEFAULT_SINK@ toggle"
  , nsp "M-S-v"                  "set-volume" -- Volume prompt

  -- KB_GROUP Brightness Control
  , cmd "<XF86MonBrightnessUp>"   "lux -a 1%; lux -G | xargs -I '{}' notify-send -a 'sysnotif' -i '/usr/share/notify-osd/icons/hicolor/scalable/status/notification-display-brightness.svg' -t 1000 'Brightness' 'Brightness increased to {}'"
  , cmd "<XF86MonBrightnessDown>" "lux -s 1%; lux -G | xargs -I '{}' notify-send -a 'sysnotif' -i '/usr/share/notify-osd/icons/hicolor/scalable/status/notification-display-brightness.svg' -t 1000 'Brightness' 'Brightness decreased to {}'"
  , nsp "M-S-b"                   "set-brightness" -- Brightness Prompt using ibox

  -- KB_GROUP Workspaces
  , on  "M-."   $ moveTo  Next nonNSP                        -- Switch to next workspace
  , on  "M-,"   $ moveTo  Prev nonNSP                        -- Switch to previous workspace
  , on  "M1-e"  $ moveTo  Next nonNSP                        -- Switch to next workspace
  , on  "M1-w"  $ moveTo  Prev nonNSP                        -- Switch to previous workspace
  , on  "M-S-." $ shiftTo Next nonNSP *> moveTo Next nonNSP  -- Shifts focused window to next ws
  , on  "M-S-," $ shiftTo Prev nonNSP *> moveTo Prev nonNSP  -- Shifts focused window to prev ws
  , on  "M-C-." $ swapTo' Next nonNSP                        -- Swaps current workspace with next workspace
  , on  "M-C-," $ swapTo' Prev nonNSP                        -- Swaps current workspace with previous workspace

  -- KB_GROUP Floating Windows
  , on  "M-t"   $ withFocused $ windows . W.sink -- Push floating window back to tile
  , on  "M-S-t"   sinkAll                        -- Push ALL floating windows to tile

  -- KB_GROUP Windows Navigation
  , on  "M-m"   $ windows W.focusMaster -- Move focus to the master window
  , on  "M-j"   $ windows W.focusDown   -- Move focus to the next window
  , on  "M-k"   $ windows W.focusUp     -- Move focus to the prev window
  , on  "M-S-j" $ windows W.swapDown    -- Swap focused window with next window
  , on  "M-S-k" $ windows W.swapUp      -- Swap focused window with prev window
  , on  "M-/"     promote               -- Moves focused window to master, others maintain order
  , on  "M-;"     rotAllDown            -- Rotate all the windows in the current stack

  -- KB_GROUP Layouts
  , on  "M-<Tab>" $ sendMessage NextLayout                                     -- Switch to next layout
  , on  "M-S-f"   $ sendMessage (MT.Toggle NBFULL) *> sendMessage ToggleStruts -- Toggles noborder/full

  -- KB_GROUP Master Pane
  , on  "M-S-<Up>"   $ sendMessage $ IncMasterN 1    -- Increase # of clients master pane
  , on  "M-S-<Down>" $ sendMessage $ IncMasterN (-1) -- Decrease # of clients master pane

  -- KB_GROUP Window Resizing
  , on  "M1-h" $ sendMessage Shrink       -- Shrink horiz window width
  , on  "M1-j" $ sendMessage MirrorShrink -- Shrink vert window width
  , on  "M1-k" $ sendMessage MirrorExpand -- Expand vert window width
  , on  "M1-l" $ sendMessage Expand       -- Expand horiz window width

  -- KB_GROUP Scratchpads
  , nsp "M-s" "btop"
  , nsp "M-d" "calcurse"
  ] ++ liftA2 (++) (uncurry termBinds) (uncurry wsBinds) (1, 9)
-- END_KEYS
  where
    on :: String -> X() -> (String, X())
    on = (,)
    cmd x y   = on  x $ spawn y
    ter :: String -> Float -> String -> (String, X())
    ter x o y = cmd x $ term ++ " -o window.opacity=" ++ show o ++ " -e " ++ y
    sh  x y z = cmd x $ "~/.xmonad/scripts/" ++ y ++ ".sh " ++ unwords z
    sh' x y   = sh x y []
    nsp key   = on key . N.namedScratchpadAction myScratchPads

    termBinds :: Int -> Int -> [(String, X())]
    termBinds x y = liftA2 nsp ("M1-" ++) ("term" ++) . show <$> [x..y]

    -- This function is intentionally cursed
    wsBinds :: Int -> Int -> [(String, X())]
    wsBinds = ((liftA2 (++)
      (liftA2 on ("M-"   ++) (windows . W.greedyView) <$>)
      (liftA2 on ("M-S-" ++) (liftA2 (*>)
        (windows . W.shift)
        (windows . W.greedyView)) <$>)
      . fmap show) .) . enumFromTo

myMouseBindings :: [((ButtonMask, Button), Window -> X ())]
myMouseBindings =
  [ on 8 $ moveTo Next nonNSP
  , on 9 $ moveTo Prev nonNSP
  , mk shiftMask 8 $ shiftTo Next nonNSP *> moveTo Next nonNSP
  , mk shiftMask 9 $ shiftTo Prev nonNSP *> moveTo Prev nonNSP
  , mk controlMask 8 $ swapTo' Next nonNSP
  , mk controlMask 9 $ swapTo' Prev nonNSP
  ]
  where
    mk mask x y = ((mask, x), const y)
    on          = mk 0

main :: IO ()
main = do
  xmproc <- spawnPipe "~/.config/xmobar/xmobar -x 0 -r ~/.config/xmobar/xmobar.hs"
  xmonad $ Hacks.javaHack . docks . withSB mySB . ewmhFullscreen . workspaceNamesEwmh . ewmh $ desktopConfig
    { manageHook         = myManageHook <> manageDocks
    , modMask            = mod4Mask
    , keys               = (`mkKeymap` myKeys)
    , terminal           = term
    , startupHook        = startupHook     def <> myStartupHook
    , handleEventHook    = handleEventHook def <> Hacks.windowedFullscreenFixEventHook
    , layoutHook         = myLayoutHook
    , workspaces         = fmap show ([1..9] :: [Int])
    , borderWidth        = myBorderWidth
    , normalBorderColor  = "#000000"
    , focusedBorderColor = "#966FD6"
    , logHook = dynamicLogWithPP $ filterOutWsPP ["NSP"] $ xmobarPP
      { ppOutput          = hPutStrLn xmproc
      , ppCurrent         = xmFG  colorWorkspace . xmBox "Bottom" colorWorkspace       . pad
      , ppVisible         = xmFG  colorWorkspace . clickable                           . pad
      , ppHidden          = xmFG  colorHidden    . xmBox "Top" colorHidden . clickable . pad
      , ppHiddenNoWindows = xmFG  colorHidden    . clickable                           . pad
      , ppUrgent          = xmFG "#cc241d"                                             . wrap' "!"
      , ppOrder           = \ (ws : _) -> [ws]
      , ppSort            = getSortByOrder
      }
    } `additionalMouseBindings` myMouseBindings
    where
      colorWorkspace = "#b16286"
      colorHidden    = "#928374"

      el :: String -> String -> [(String, String)] -> String -> String
      el e ev attr = wrap
        ("<" ++ val e ev ++ attrs attr ++ ">")
        $ "</" ++ e ++ ">"
        where
          val k [] = k
          val k v  = k  ++ "=" ++ v
          attrs [] = ""
          attrs xs = " " ++ unwords (uncurry val <$> xs)

      xmFG            = flip xmobarColor ""
      xmBox pos color = el "box" "" [("type", pos), ("width", "2"), ("mb", "2"), ("color", color)]
      wrap'           = join wrap

      mySB = statusBarProp "~/.config/xmobar/xmobar" (pure xmobarPP { ppOrder = \ (_ : l : _) -> [unwords $ drop 1 $ words l]})

      clickable ws = el "action" ("wmctrl -s " ++ show ((read ws :: Int) - 1)) [] ws

      myBorderWidth = 2
      myLayoutHook  = avoidStruts $ smartSpacing 2 $ smartBorders $ windowArrange
                      $ T.toggleLayouts floats $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) layouts
        where
          name x = renamed [Replace x]
          borderedSub = subLayout [] (smartBorders Simplest)
          tall = name "<icon=tall_16.xpm/> tall"
            $ windowNavigation
            $ borderedSub
            $ ResizableTall 1 (1/100) (1/2) []
          floats = name "<icon=floats_16.xpm/> floats" simplestFloat
          grid = name "<icon=grid_16.xpm/> grid"
            $ windowNavigation
            $ borderedSub
            $ mkToggle (single MIRROR)
            $ Grid (16/10)
          layouts = bordered tall
            ||| bordered grid
            ||| floats
            where
              bordered layout = withBorder myBorderWidth layout

