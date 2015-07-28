import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Run
import qualified XMonad.StackSet as W
import XMonad.Config.Gnome

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops -- to fullscreen Chrome

import XMonad.Actions.CycleWS
import XMonad.Actions.WithAll

import XMonad.Layout.Accordion
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.PerWorkspace

myFocusedBorderColor = "#ffa500"
myNormalBorderColor = "#073642"
-- myWorkspaces = ["main","play","skype","spotify"] ++ map show [5..9]

myLayoutHook = avoidStruts (tall ||| Mirror Accordion ||| Mirror tall ||| Accordion)
               ||| noBorders Full
  where tall = ResizableTall 1 (3/100) (1/2) []

myManageHook = composeAll [ isFullscreen --> doFullFloat ] <+> manageDocks

main = xmonad $ gnomeConfig {
  modMask = mod4Mask,
  -- workspaces = myWorkspaces,
  focusFollowsMouse = False,
  -- border
  borderWidth = 1,
  normalBorderColor = myNormalBorderColor,
  focusedBorderColor = myFocusedBorderColor,
  -- to fullscreen Chrome
  handleEventHook = fullscreenEventHook,
  manageHook = myManageHook,
  layoutHook = smartBorders (myLayoutHook)
  }

 `additionalKeysP` -- shortcuts
       [ ("M-o", spawn "google-chrome"),
         ("M-S-o", spawn "google-chrome --incognito"),
         ("M-\\", spawn "emacsclient -c"),
         -- ("M-S-\\", spawn "ec"),
         ("M-x p", spawn "gnome-terminal -e ipython3"),
         ("M-u", spawn "skype"),
         ("M-i", spawn "spotify"),
         ("C-M-<Backspace>", killAll),
         ("M-=", sendMessage MirrorExpand),
         ("M--", sendMessage MirrorShrink),
         ("M-`", toggleWS),
         ("C-M-<L>", prevWS),
         ("C-M-<R>", nextWS) ]
 `additionalKeys`
       [ ((mod1Mask, xK_Tab), windows W.focusDown), -- alt-tab
         ((mod1Mask .|. shiftMask, xK_Tab), windows W.focusUp), -- alt-shift-tab
         -- move between adjacent workspaces like unity
         ((mod1Mask .|. controlMask, xK_Left), prevWS), -- ctrl-alt-left
         ((mod1Mask .|. controlMask, xK_Right), nextWS), -- ctrl-alt-right
         ((mod1Mask .|. controlMask .|. shiftMask, xK_Left),
          shiftToPrev >> prevWS), -- ctrl-alt-shift-left
         ((mod1Mask .|. controlMask .|. shiftMask, xK_Right),
          shiftToNext >> nextWS) -- ctrl-alt-shift-left
       ]
