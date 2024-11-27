{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

import XMonad.Actions.CopyWindow (kill1, copyToAll, killAllOtherCopies)
import XMonad.Actions.CycleWS (Direction1D(..), WSType(..))
import XMonad.Actions.DynamicWorkspaceOrder (moveTo, shiftTo, getSortByOrder)
import XMonad.Actions.Promote (promote)
import XMonad.Actions.RotSlaves (rotAllDown)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.WorkspaceNames (swapTo', workspaceNamesEwmh, swapWithCurrent)

import Data.Bifunctor (first)
import Data.Char (toLower)
import Data.Foldable (traverse_)
import Data.List (isInfixOf)
import Data.Monoid (Endo)

import GHC.Base (Applicative (liftA2), join, liftA3)

import Text.Read (readMaybe)

import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen, setEwmhActivateHook)
import XMonad.Hooks.ManageDocks (docks, manageDocks, avoidStruts, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.StatusBar (statusBarProp, withSB)
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook (doAskUrgent, withUrgencyHook, NoUrgencyHook (NoUrgencyHook), focusUrgent)

import XMonad.Layout.GridVariants (Grid(..))
import XMonad.Layout.SimplestFloat (simplestFloat)
import XMonad.Layout.ResizableTile (MirrorResize(..), ResizableTall(..))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(..))
import XMonad.Layout.NoBorders (smartBorders, withBorder)
import XMonad.Layout.Renamed (renamed, Rename(..))
import XMonad.Layout.Simplest (Simplest(..))
import XMonad.Layout.Spacing (smartSpacing)
import XMonad.Layout.SubLayouts (subLayout)
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg (SetGeometry, DeArrange, Arrange))
import XMonad.Layout.WindowNavigation (windowNavigation)
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts)
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.EZConfig (mkKeymap, additionalMouseBindings)
import qualified XMonad.Util.Hacks as Hacks
import XMonad.Util.Run (spawnPipe)
import qualified XMonad.Util.NamedScratchpad as N
import XMonad.Util.SpawnOnce (manageSpawn, spawnOnOnce, spawnOnce)

workspaces' :: [String]
workspaces' = (:[]) <$> ['1'..'9']

browser :: String
browser = "chromium --allow-legacy-extension-manifests --enable-features=VaapiVideoDecoder --ignore-gpu-blocklist --disable-features=UseOzonePlatform --enable-gpu-rasterization --enable-zero-copy --process-per-site"

chromiumApp :: String -> String
chromiumApp = ("~/.xmonad/scripts/chromium-app.sh " ++)

startupHook' :: X()
startupHook' = do
  setDefaultCursor xC_left_ptr
  setWMName "LG3D"
  traverse_ spawn
    [ "pkill picom; picom"
    , "~/.config/xmobar/scripts/cavabar"
    ]
  traverse_ spawnOnce
    [ "dbus-update-activation-environment --systemd DBUS_SESSION_BUS_ADDRESS DISPLAY XAUTHORITY" -- fix xdg-portal-desktop not working
    , "lxsession"
    , "feh --bg-scale ~/.xmonad/wallpaper"
    , "conky"
    , "greenclip daemon"
    , "dunst"
    , "xrandr --output eDP --auto"
    , "~/.xstart"
    ]
  traverse_ (uncurry spawnOnOnce)
    [ ("1", browser)
    , ("3", chromiumApp "WhatsApp Web")
    , ("2", "vesktop --ignore-gpu-blocklist --disable-features=UseOzonePlatform --enable-features=VaapiVideoDecoder --use-gl=desktop --enable-gpu-rasterization --enable-zero-copy")
    ]

term :: String
term = "alacritty" -- There appears to be no good way to know the user's default terminal.

scratchpads' :: [N.NamedScratchpad]
scratchpads' =
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
    x = (1 - w) / 2
    y = (1 - h) / 2

  centeredLarge = centered 0.9 0.9
  centeredIBox  = centered 0.4 0.15

  spawnTerm = ((term ++ " -t term") ++) . show
  findTerm  = find . ("term" ++) . show
  term'     = flip (liftA3 N.NS (("term" ++) . show) spawnTerm findTerm) $ centered 0.6 0.6

  terms :: Int -> Int -> [N.NamedScratchpad]
  terms = (fmap term' .) . enumFromTo

manageHook' :: XMonad.Query (Data.Monoid.Endo WindowSet)
manageHook' = composeAll
  ([ float'      title           "Emulator"
   , floatRes   "firefox"        "Dialog"
   , shiftClass "Vesktop"        "2"
   , shiftTitle "WhatsApp Web"   "3"
   , isFullscreen --> doFullFloat
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
    , "ninjabrainbot-Main"
    ]
  ) <> N.namedScratchpadManageHook scratchpads' <> manageSpawn
 where
  q =~ x = (\ z -> low x `isInfixOf` low z) <$> q
   where low = fmap toLower

  float'     x y = (x =~ y) --> doFloat
  floatRes   x y = (className =~ x <&&> resource =~ y) --> doFloat
  shift      x y ws = (x =~ y) --> doShift ws
  shift'     cnd ws = foldr (<&&>) (pure False) cnd --> doShift ws
  shiftTitle = shift title
  shiftClass = shift className

nonNSP :: WSType
nonNSP = WSIs $ pure $ (/=) "NSP" . W.tag

keys' :: [(String, X())]
keys' =
-- START_KEYS
  -- KB_GROUP XMonad
  [ cmd "M-C-r"   $ xmonad ++ " --recompile"                      -- Recompiles xmonad
  , on  "M-S-r"   $ spawn (xmonad ++ " --restart") <> startupHook' -- Restarts xmonad
  , on  "M-C-S-r" $ io exitSuccess                                -- Recompiles and restarts xmonad
  , nsp "M-S-/"    "xmonad_keys"                                  -- Get list of keybindings

  -- KB_GROUP Rofi
  , cmd "M-p"   "rofi -show drun"                                                                            -- rofi
  , cmd "M-c"   "rofi -show calc -no-show-match -no-sort -calc-command \"echo -n '{result}' | xclip -se c\"" -- rofi-calc
  , cmd "M-q"   "rofi -show window"                                                                          -- rofi-calc
  , cmd "M-e"   "rofi -show emoji"                                                                           -- rofi-emoji
  , sh' "M-S-e" "rofi-nerdfont"                                                                              -- rofi-nerdfont
  , sh' "M-C-e" "rofi-unicode"                                                                               -- rofi-unicode
  , sh' "M-S-w" "rofi-wifi-menu"                                                                             -- rofi-wifi-menu
  , cmd "M-S-q" "~/.config/rofi/scripts/powermenu_t1"                                                        -- rofi power menu
  , cmd "M-v"   "rofi -modi 'clipboard:greenclip print' -show clipboard -run-command '{cmd}'"                -- greenclip

  -- KB_GROUP Useful Applications
  , cmd "M-b"         browser
  , cmd "M-<Return>"  term
  , cmd "M-S-s"      "sleep 0.3; xset -display $DISPLAY dpms force off"
  , cmd "M-f"        "pcmanfm-qt"
  , cmd "M-l"        "xtrlock"
  , cmd "M-C-l"      "sleep 0.3; xset -display $DISPLAY dpms force off; xtrlock"
  , cmd "M-a"        "screenpen"
  , sh' "<Print>"    "screenshot"
  , sh' "C-<Print>"  "screenshotflame"
  , sh' "S-<Print>"  "screenshotfull"

  -- KB_GROUP Window Management
  , on  "M-C-a" $  windows copyToAll        -- Copy current window to all workspaces
  , on  "M-S-c"    kill1                    -- Kill the currently focused client
  , on  "M-S-a"    killAll                  -- Kill all windows on current workspace
  , on  "M-C-c"    killAllOtherCopies       -- Kill all windows on current workspace
  , sh  "M--"     "xbasket" ["hide"]        -- Hide window
  , sh  "M-="     "xbasket" ["select"]      -- Select hidden window

  -- KB_GROUP Volume Control
  , cmd "<XF86AudioRaiseVolume>" "pactl set-sink-volume @DEFAULT_SINK@ +1%; pactl get-sink-volume @DEFAULT_SINK@ | sed -r '{N; s/^(\\w*\\W+){4}([0-9]+%).*/\\2/}' | xargs -I '{}' notify-send -i '/usr/share/notify-osd/icons/hicolor/scalable/status/notification-audio-volume-medium.svg' -t 1000 -a 'sysnotif' -h int:value:{} 'Volume' 'Volume increased to'"
  , cmd "<XF86AudioLowerVolume>" "pactl set-sink-volume @DEFAULT_SINK@ -1%; pactl get-sink-volume @DEFAULT_SINK@ | sed -r '{N; s/^(\\w*\\W+){4}([0-9]+%).*/\\2/}' | xargs -I '{}' notify-send -i '/usr/share/notify-osd/icons/hicolor/scalable/status/notification-audio-volume-medium.svg' -t 1000 -a 'sysnotif' -h int:value:{} 'Volume' 'Volume decreased to'"
  , cmd "<XF86AudioMute>"        "pactl set-sink-mute @DEFAULT_SINK@ toggle"
  , nsp "M-S-v"                  "set-volume" -- Volume prompt
  , cmd "M-h"                    "pactl set-sink-port alsa_output.pci-0000_05_00.6.analog-stereo analog-output-headphones" -- Force headphone output (useful when headphones not detected)

  -- KB_GROUP Brightness Control
  , cmd "<XF86MonBrightnessUp>"   "lux -a 1%; lux -G | xargs -I '{}' notify-send -a 'sysnotif' -i '/usr/share/notify-osd/icons/hicolor/scalable/status/notification-display-brightness.svg' -t 1000 'Brightness' 'Brightness increased to {}'"
  , cmd "<XF86MonBrightnessDown>" "lux -s 1%; lux -G | xargs -I '{}' notify-send -a 'sysnotif' -i '/usr/share/notify-osd/icons/hicolor/scalable/status/notification-display-brightness.svg' -t 1000 'Brightness' 'Brightness decreased to {}'"
  , nsp "M-S-b"                   "set-brightness" -- Brightness Prompt using ibox

  -- KB_GROUP Workspaces
  , on  "M-."   $ moveTo  Next nonNSP                        -- Switch to next workspace
  , on  "M-,"   $ moveTo  Prev nonNSP                        -- Switch to previous workspace
  , on  "M-S-." $ shiftTo Next nonNSP *> moveTo Next nonNSP  -- Shifts focused window to next ws
  , on  "M-S-," $ shiftTo Prev nonNSP *> moveTo Prev nonNSP  -- Shifts focused window to prev ws
  , on  "M-C-." $ swapTo' Next nonNSP                        -- Swaps current workspace with next workspace
  , on  "M-C-," $ swapTo' Prev nonNSP                        -- Swaps current workspace with previous workspace
  , on  "M-u"     focusUrgent

  -- KB_GROUP Floating Windows
  , on  "M-t"    $ withFocused $ windows . W.sink                        -- Push floating window back to tile
  , on  "M-S-t"    sinkAll                                               -- Push ALL floating windows to tile
  , on  "M1-t"   $ sendMessage Arrange
                 *> sendMessage (SetGeometry $ Rectangle (1920 `div` 2 - 160) 0 (160 * 2) 1080) -- Float and make window thin
  , on  "M1-S-t" $ sendMessage DeArrange                                 -- Stop arranging window

  -- KB_GROUP Windows Navigation
  , on  "M-m"      $ windows W.focusMaster -- Move focus to the master window
  , on  "M-j"      $ windows W.focusDown   -- Move focus to the next window
  , on  "M1-<Tab>" $ windows W.focusDown   -- Move focus to the next window
  , on  "M-k"      $ windows W.focusUp     -- Move focus to the prev window
  , on  "M-S-j"    $ windows W.swapDown    -- Swap focused window with next window
  , on  "M-S-k"    $ windows W.swapUp      -- Swap focused window with prev window
  , on  "M-/"      promote                 -- Moves focused window to master, others maintain order
  , on  "M-;"      rotAllDown              -- Rotate all the windows in the current stack

  -- KB_GROUP Layouts
  , on  "M-S-f"   $ sendMessage (MT.Toggle NBFULL) *> sendMessage ToggleStruts -- Toggles noborder/full
  , on  "<F11>"   $ sendMessage (MT.Toggle NBFULL) *> sendMessage ToggleStruts -- Toggles noborder/full
  , msg "M-<Tab>" NextLayout                                                   -- Switch to next layout

  -- KB_GROUP Master Pane
  , msg "M-S-<Up>"   $ IncMasterN 1    -- Increase # of clients master pane
  , msg "M-S-<Down>" $ IncMasterN (-1) -- Decrease # of clients master pane

  -- KB_GROUP Window Resizing
  , msg "M1-S-h" Shrink       -- Shrink horiz window width
  , msg "M1-S-j" MirrorShrink -- Shrink vert window width
  , msg "M1-S-k" MirrorExpand -- Expand vert window width
  , msg "M1-S-l" Expand       -- Expand horiz window width

  -- KB_GROUP Scratchpads
  , nsp "M-s" "btop"
  , nsp "M-d" "calcurse"
  ] ++ liftA2 (++) termBinds wsBinds workspaces'
-- END_KEYS
 where
  xmonad = "~/.xmonad/xmonad-x86_64-linux"
  on :: String -> X() -> (String, X())
  on = (,)
  cmd key c   = on key $ spawn c
  msg key     = on key . sendMessage
  ter key o c = cmd key $ term ++ " -o window.opacity=" ++ show o ++ " -e " ++ c
  ter :: String -> Float -> String -> (String, X())
  sh  key f a = cmd key $ "~/.xmonad/scripts/" ++ f ++ ".sh " ++ unwords a
  sh' key f   = sh key f []
  nsp key     = on key . N.namedScratchpadAction scratchpads'

  termBinds :: [String] -> [(String, X())]
  termBinds = fmap $ liftA2 nsp ("M1-" ++) ("term" ++)

  wsBinds :: [String] -> [(String, X())]
  wsBinds = (=<<) $ traverse (uncurry (liftA2 on) . first (++))
    [ ("M-",   windows . W.greedyView)
    , ("M-S-", liftA2 (*>)
                 (windows . W.shift)
                 (windows . W.greedyView))
    , ("M-C-", swapWithCurrent)
    ]

mouseBindings' :: [((ButtonMask, Button), Window -> X ())]
mouseBindings' =
  [ none  8 $ moveTo  Next nonNSP
  , none  9 $ moveTo  Prev nonNSP
  , shift 8 $ shiftTo Next nonNSP *> moveTo Next nonNSP
  , shift 9 $ shiftTo Prev nonNSP *> moveTo Prev nonNSP
  , ctrl  8 $ swapTo' Next nonNSP
  , ctrl  9 $ swapTo' Prev nonNSP
  ]
 where
  mk mask key f = ((mask, key), const f)
  none  = mk 0
  shift = mk shiftMask
  ctrl  = mk controlMask

main :: IO ()
main = do
  xmproc <- spawnPipe "~/.cabal/bin/xmobar -x 0 -r ~/.config/xmobar/xmobar.hs"
  xmonad $ Hacks.javaHack . withSB sb . docks .
           withUrgencyHook NoUrgencyHook .
           setEwmhActivateHook doAskUrgent .
           ewmhFullscreen . workspaceNamesEwmh .
           workspaceNamesEwmh . ewmh $ def
    { manageHook         = manageHook' <> manageDocks
    , modMask            = mod4Mask
    , keys               = (`mkKeymap` keys')
    , terminal           = term
    , startupHook        = startupHook' <> startupHook def
    , handleEventHook    = Hacks.windowedFullscreenFixEventHook <> handleEventHook def
    , layoutHook         = layoutHook'
    , workspaces         = workspaces'
    , borderWidth        = borderWidth'
    , normalBorderColor  = "#000000"
    , focusedBorderColor = "#966FD6"
    , logHook            = dynamicLogWithPP $ filterOutWsPP ["NSP"] $ xmobarPP
      { ppOutput          = hPutStrLn xmproc
      , ppCurrent         = xmFG colorWorkspace . xmBox "Bottom" colorWorkspace       . pad
      , ppVisible         = xmFG colorWorkspace . clickable                           . pad
      , ppHidden          = xmFG colorHidden    . xmBox "Top" colorHidden . clickable . pad
      , ppHiddenNoWindows = xmFG colorHidden    . clickable                           . pad
      , ppUrgent          = xmFG colorUrgent    . xmBox "Top" colorUrgent . clickable . pad
      , ppOrder           = \ (ws : _) -> [ws]
      , ppSort            = getSortByOrder
      }
    } `additionalMouseBindings` mouseBindings'
   where
    colorWorkspace = "#165495"
    colorHidden    = "#3b526a"
    colorUrgent    = "#ffa500"

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

    sb = statusBarProp "~/.cabal/bin/xmobar" (pure xmobarPP {
      ppOrder = \ (_ : l : _) -> [unwords $ drop 1 $ words l]
      })

    clickable ws = el "action" ("wmctrl -s " ++ maybe "0" (show . subtract 1) (readMaybe ws :: Maybe Int)) [] ws

    borderWidth' = 2
    layoutHook'  = avoidStruts $ smartSpacing 2 $ smartBorders $ windowArrange
                    $ T.toggleLayouts floats $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) layouts
     where
      name x = renamed [Replace x]
      xpm x = name $ "<icon=" ++ x ++ ".xpm/>"
      borderedSub = subLayout [] $ smartBorders Simplest

      tall = xpm "tall"
        $ windowNavigation
        $ borderedSub
        $ ResizableTall 1 (1/128) (1/2) []
      grid = xpm "grid"
        $ windowNavigation
        $ borderedSub
        $ mkToggle (single MIRROR)
        $ Grid (16/9)
      floats = xpm "floats" simplestFloat
      layouts = bordered tall
        ||| bordered grid
        ||| floats
       where bordered layout = withBorder borderWidth' layout
