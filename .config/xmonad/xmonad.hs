import qualified Data.Map as M
import System.Exit
import XMonad
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.NoBorders
import XMonad.Actions.RotSlaves
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.NoBorders
import XMonad.Layout.TwoPanePersistent
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Ungrab

windowCount =
  gets $
    Just
      . show
      . length
      . W.integrate'
      . W.stack
      . W.workspace
      . W.current
      . windowset

myXmobarCall :: String
myXmobarCall = "xmobar ~/.config/xmobar/xmobar.config"

myWorkspaces :: [String]
myWorkspaces = map show [1 .. 9] ++ ["0"]

workspaceKeys conf@XConfig {XMonad.modMask = modm} =
  M.fromList
    [ ((m .|. modm, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) $ [xK_1 .. xK_9] ++ [xK_0],
        (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]

-- pretty printing for xmobar
myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = " | ",
      ppTitleSanitize = xmobarStrip,
      ppTitle = xmobarColor "#cc241d" "",
      ppCurrent = xmobarColor "#cc241d" "" . wrap "<" ">",
      ppHidden = xmobarColor "#b16286" "",
      ppLayout = xmobarColor "#458588" "" . layoutSynonym,
      ppExtras = [windowCount],
      ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
    }

layoutSynonym :: String -> String
layoutSynonym "Full" = "[ ]"
layoutSynonym "Tall" = "[]|"
layoutSynonym "TwoPanePersistent" = "|{}"
layoutSynonym other = other

myKeys :: [(String, X ())]
myKeys =
  [ ("M-j", windows W.focusDown),
    ("M-k", windows W.focusUp),
    ("M-S-j", windows W.swapDown),
    ("M-S-k", windows W.swapUp),
    ("M-<Space>", sendMessage NextLayout),
    ("M-q", spawn "xmonad --restart"),
    ("M-S-q", io exitSuccess),
    ("M-t", withFocused $ windows . W.sink),
    ("M-b", sendMessage ToggleStruts),
    ("M-S-m", windows W.swapMaster),
    ("M-<Delete>", unGrab *> spawn "i3lock"),
    ("M-g", withFocused toggleBorder),
    ("M-S-x", withFocused killWindow),
    ("M-;", rotSlavesDown),
    ("M-:", rotSlavesUp),
    ("M-<Tab>", toggleRecentNonEmptyWS),
    ("M-<Return>", spawn "alacritty"),
    ("M-p", spawn "rofi -show run"),
    ("<Print>", spawn "flameshot gui"),
    ("M-f", spawn "firefox"),
    ("M-e", spawn "emacs"),
    ("<XF86AudioMute>", spawn "amixer set Master toggle"),
    ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-"),
    ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+"),
    ("<XF86AudioMicMute>", spawn "amixer set Capture toggle"),
    ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10"),
    ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10")
  ]

myMouseBindings XConfig {XMonad.modMask = modm} =
  M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster),
      ((modm, button2), \w -> focus w >> windows W.shiftMaster),
      ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    ]

myLayouts =
  noBorders Full
    ||| Tall 1 (3 / 100) (1 / 2)
    ||| TwoPanePersistent Nothing (3 / 100) (1 / 2)

myManageHook =
  composeAll
    [ className =? "discord" --> doShift "9",
      className =? "Element" --> doShift "9",
      className =? "KeePassXC" --> doShift "0"
    ]

main :: IO ()
main =
  xmonad
    -- . ewmhFullscreen -- uncomment for "normal" fullscreen
    . ewmh
    . withEasySB (statusBarProp myXmobarCall (pure myXmobarPP)) defToggleStrutsKey
    $ def
      { modMask = mod4Mask,
        normalBorderColor = "#a89984",
        focusedBorderColor = "#cc241d",
        borderWidth = 3,
        layoutHook = myLayouts,
        workspaces = myWorkspaces,
        keys = workspaceKeys,
        mouseBindings = myMouseBindings,
        manageHook = myManageHook
      }
      `additionalKeysP` myKeys
