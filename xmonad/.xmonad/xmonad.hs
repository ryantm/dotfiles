import XMonad
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.Accordion
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig
import XMonad.Hooks.DynamicLog (xmobar)

layout = smartBorders (tiled ||| Mirror tiled ||| Full)
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

main = do
  xmonad =<< xmobar
    (
      defaultConfig {
         modMask = mod4Mask,
         layoutHook = layout
         }
      `additionalKeys`
      [ ((mod4Mask, xK_p), spawn "dmenu_run -fn \"Source Code Pro:size=20\"") ]
    )
