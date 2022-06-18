import XMonad
import qualified XMonad.StackSet as W

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Loggers

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageHelpers

import XMonad.Actions.NoBorders
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.RotSlaves

import XMonad.Layout.NoBorders
import XMonad.Layout.TwoPanePersistent

myXmobarCall :: String
myXmobarCall = "xmobar ~/.config/xmobar/xmobar.config"

-- pretty printing for xmobar
myXmobarPP :: PP
myXmobarPP = def
    { ppSep = " | "
    , ppTitleSanitize = xmobarStrip
    , ppTitle = xmobarColor "#cc241d" ""
    , ppCurrent = xmobarColor "#cc241d" "" . wrap "<" ">"
    , ppHidden = xmobarColor "#b16286" ""
    , ppLayout = xmobarColor "#458588" "" . layoutSynonym
    }

layoutSynonym :: String -> String
layoutSynonym "Full" = "[ ]"
layoutSynonym "Tall" = "[]|"
layoutSynonym "TwoPanePersistent" = "|{}"
layoutSynonym other = other

myKeys :: [(String, X ())]
myKeys = [ ("M-<Delete>", unGrab *> spawn "i3lock")
         , ("M-g", withFocused toggleBorder)
         , ("M-S-x", withFocused killWindow)
         , ("M-;", rotSlavesDown)
         , ("M-:", rotSlavesUp)
         , ("M-<Tab>", toggleRecentNonEmptyWS)
         , ("M-<Return>", spawn "alacritty")
         , ("M-p", spawn "rofi -show run")
         , ("<Print>", spawn "flameshot gui")
         , ("M-f", spawn "firefox")
         , ("M-e", spawn "emacs")
         , ("<XF86AudioMute>", spawn "amixer set Master toggle")
         , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")
         , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+")
         , ("<XF86AudioMicMute>", spawn "amixer set Capture toggle")
         , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10")
         , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10")
         ]

myLayouts = noBorders Full
        ||| Tall 1 (3/100) (1/2)
        ||| TwoPanePersistent Nothing (3/100) (1/2)

main :: IO()
main = xmonad
      -- . ewmhFullscreen -- uncomment for "normal" fullscreen
      . ewmh
      . withEasySB (statusBarProp myXmobarCall (pure myXmobarPP)) defToggleStrutsKey
      $ def
     { modMask = mod4Mask
     , normalBorderColor = "#a89984"
     , focusedBorderColor = "#cc241d"
     , borderWidth = 3
     , layoutHook = myLayouts
     } `additionalKeysP` myKeys
