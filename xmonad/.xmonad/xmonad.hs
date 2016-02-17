import XMonad
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog (
    PP (..)
  , defaultPP
  , xmobarColor
  , wrap
  , shorten
  , statusBar
  )
import XMonad.Hooks.EwmhDesktops (
    ewmh
  , fullscreenEventHook
  )
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Util.EZConfig (additionalKeys)

layout = (smartBorders (tiled ||| Mirror tiled ||| Full))
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

myXmobarPP = defaultPP { ppCurrent = xmobarColor "yellow" ""
                       , ppTitle   = xmobarColor "green"  "" . shorten 40
                       , ppVisible = wrap "(" ")"
                       , ppLayout = const ""
                       , ppUrgent  = xmobarColor "red" "yellow"
                       }

toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

myXmobar = statusBar "xmobar" myXmobarPP toggleStrutsKey

myConfig =
  ewmh (desktopConfig {
      modMask = mod4Mask
    , layoutHook = layout
    , handleEventHook =
        handleEventHook desktopConfig <+> fullscreenEventHook
    })
  `additionalKeys`
  [ ((mod4Mask, xK_p), spawn "dmenu_run -fn \"Source Code Pro:size=20\"") ]

main = xmonad =<< myXmobar myConfig
