{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-------------------------------------------------------------------------------
--                                                                           --
-- XMonad Configuration                                                      --
--                                                                           --
----------------------------------------------------------------------------}}}
-- IMPORTS                                                                  {{{
-------------------------------------------------------------------------------

import Data.Default (def)
import qualified Data.Map as M
import System.IO (hClose)
import System.Taffybar.Hooks.PagerHints (pagerHints)

import XMonad hiding ((|||))

import XMonad.Actions.ConditionalKeys (bindOn, XCond(..))
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects (Project(..), dynamicProjects)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.MessageFeedback (tryMessage_)
import XMonad.Actions.Navigation2D
import XMonad.Actions.Promote
import XMonad.Actions.SpawnOn (manageSpawn, spawnOn)
import XMonad.Actions.WithAll (sinkAll, killAll)

import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.DecorationMadness
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.Hidden
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.PerScreen
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation

import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt

import qualified XMonad.StackSet as W

import XMonad.Util.EZConfig (mkNamedKeymap)
import XMonad.Util.NamedActions (NamedAction(..), addDescrKeys', (^++^), subtitle, addName, showKm)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe, hPutStr)
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import XMonad.Util.XSelection (getSelection)


----------------------------------------------------------------------------}}}
-- Main                                                                     {{{
-------------------------------------------------------------------------------

main :: IO ()
main =
  xmonad
    $ addDescrKeys' ((myModMask, xK_F1), showKeybindings) myKeys
    $ dynamicProjects projects
    $ docks
    $ ewmh
    $ pagerHints
    $ withNavigation2DConfig myNav2DConfig
    $ myConfig

myConfig = def
  { borderWidth        = myBorderWidth
  , clickJustFocuses   = myClickJustFocuses
  , focusFollowsMouse  = myFocusFollowsMouse
  , normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , manageHook         = myManageHook
  , handleEventHook    = myHandleEventHook
  , layoutHook         = myLayoutHook
  , logHook            = myLogHook
  , modMask            = myModMask
  , mouseBindings      = myMouseBindings
  , startupHook        = myStartupHook
  , terminal           = myTerminal
  , workspaces         = myWorkspaces
  }

myMouseBindings = mouseBindings def
myStartupHook = startupHook def

----------------------------------------------------------------------------}}}
-- theme                                                                    {{{
-------------------------------------------------------------------------------

myClickJustFocuses = True
myFocusFollowsMouse = False

-- colors
base03  = "#002b37"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"
black   = "#000000"
white   = "#ffffff"

cActive     = blue
cActiveWarn = red
cInactive   = base02
cFocus      = blue
cUnfocus    = base02

-- sizes
sGap     = 10
sTopbar  = 10
sBorder  = 0
sPrompt  = 26
sStatus  = 20

myBorderWidth        = 0
myNormalBorderColor  = black
myFocusedBorderColor = cActive

myFont = "-*-terminus-medium-*-*-*-*-160-*-*-*-*-*-*"

tTopBar = def
  { fontName = myFont
  , inactiveBorderColor = base03
  , inactiveColor = base03
  , inactiveTextColor = base03
  , activeBorderColor = cActive
  , activeTextColor = cActive
  , activeColor = cActive
  , urgentBorderColor = red
  , urgentTextColor = yellow
  , decoHeight = sTopbar
  }

tPrompt = def
  { font = myFont
  , bgColor = base03
  , fgColor = cActive
  , fgHLight = base03
  , bgHLight = cActive
  , borderColor = base03
  , promptBorderWidth = 0
  , height = sPrompt
  , position = Top
  }

tHotPrompt = tPrompt
  { bgColor = yellow
  , fgColor = base03
  }

tShowWName = def
  { swn_fade    = 0.5
  , swn_bgcolor = black
  , swn_color   = white
  }

tTab = def
  { fontName              = myFont
  , activeColor           = cActive
  , inactiveColor         = base02
  , activeBorderColor     = cActive
  , inactiveBorderColor   = base02
  , activeTextColor       = base03
  , inactiveTextColor     = base00
  }

----------------------------------------------------------------------------}}}
-- Workspaces                                                               {{{
-------------------------------------------------------------------------------

wsTERM     = "TERM"
wsWEB      = "WEB"
wsCOM      = "COM"
wsWORKTERM = "WRK:TERM"
wsWORKWEB  = "WRK:WEB"
wsMEDIA    = "MEDIA"
wsMONITOR  = "MONITOR"
wsSYSTEM   = "SYSTEM"
wsTEMP     = "TEMP"
wsNSP      = "NSP"

myWorkspaces = [ wsTERM, wsWEB, wsCOM, wsWORKTERM, wsWORKWEB, wsMEDIA, wsMONITOR, wsSYSTEM, wsTEMP, wsNSP ]

projects :: [Project]
projects =
  [ Project { projectName = wsTERM
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawnOn wsTERM myTerminal
            }
  , Project { projectName = wsWEB
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawnOn wsWEB myBrowser
            }
  , Project { projectName = wsWORKTERM
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawnOn wsWORKTERM myTerminal
            }
  , Project { projectName = wsMONITOR
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawnOn wsMONITOR glances
            }
  ]


----------------------------------------------------------------------------}}}
-- Bindings                                                                 {{{
-------------------------------------------------------------------------------

myModMask = mod4Mask

myKeys config =
    -- You can get the key code using `xev`
    ---------------------------------------------------------------------------
    -- | Actions
    ---------------------------------------------------------------------------
    keySet "Actions"
      [
      ]
    ^++^
    ---------------------------------------------------------------------------
    -- | Audio
    ---------------------------------------------------------------------------
    keySet "Audio"
      [ key "Play"                                  "<XF86AudioPlay>" $ spawn "playerctl play-pause"
      , key "Previous"                              "<XF86AudioPrev>" $ spawn "playerctl previous"
      , key "Next"                                  "<XF86AudioNext>" $ spawn "playerctl next"

      , key "Mute"                                  "<XF86AudioMute>" $ spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"
      , key "Raise Volume"                          "<XF86AudioRaiseVolume>" $ spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"
      , key "Lower Volume"                          "<XF86AudioLowerVolume>" $ spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"
      ]
    ^++^
    ---------------------------------------------------------------------------
    -- | Launchers
    ---------------------------------------------------------------------------
    keySet "Launchers"
      [ key "Launcher"                              "M-<Space>"       $ spawn myLauncher
      , key "Terminal"                              "M-<Return>"      $ spawn myTerminal
      , key "Spotify"                               "M-m"             $ namedScratchpadAction myScratchpads "spotify"
      , key "Mixer"                                 "M-v"             $ namedScratchpadAction myScratchpads "alsamixer"
      , key "Console"                               "M-c"             $ namedScratchpadAction myScratchpads "console"
      , key "VLC"                                   "M-x v"           $ namedScratchpadAction myScratchpads "vlc"
      ]
    ^++^
    ---------------------------------------------------------------------------
    -- | Layouts
    ---------------------------------------------------------------------------
    keySet "Layouts"
      [ key "Cycle all layouts"                     "M-<Tab>"         $ sendMessage NextLayout
      , key "Cycle sublayout"                       "M-C-<Tab>"       $ toSubl NextLayout
      -- , key "Reset layout"                          "M-S-<Tab>"       $ setLayout $ XMonad.layoutHook conf

      , key "Float tiled window"                    "M-y"             $ withFocused toggleFloat
      , key "Tile all floating windows"             "M-S-y"           $ sinkAll

      , key "Fullscreen"                            "M-f"             $ sendMessage $ XMonad.Layout.MultiToggle.Toggle FULL

      , key "Decrease master windows"               "M-,"             $ sendMessage $ IncMasterN (-1)
      , key "Increase master windows"               "M-."             $ sendMessage $ IncMasterN 1

      , key "Reflect/Rotate"                        "M-r"             $ tryMessageR Rotate (XMonad.Layout.MultiToggle.Toggle REFLECTX)
      ]
    ^++^
    ---------------------------------------------------------------------------
    -- | Resizing
    ---------------------------------------------------------------------------
    keySet "Resizing"
      [ key "Expand (L on BSP)"                     "M-["             $ tryMessageR (ExpandTowards L) Shrink
      , key "Expand (R on BSP)"                     "M-]"             $ tryMessageR (ExpandTowards R) Expand
      , key "Expand (U on BSP)"                     "M-S-["           $ tryMessageR (ExpandTowards U) MirrorShrink
      , key "Expand (D on BSP)"                     "M-S-]"           $ tryMessageR (ExpandTowards D) MirrorExpand

      , key "Shrink (L on BSP)"                     "M-C-["           $ tryMessageR (ShrinkFrom R) Shrink
      , key "Shrink (R on BSP)"                     "M-C-]"           $ tryMessageR (ShrinkFrom L) Expand
      , key "Shrink (U on BSP)"                     "M-C-S-["         $ tryMessageR (ShrinkFrom D) MirrorShrink
      , key "Shrink (D on BSP)"                     "M-C-S-]"         $ tryMessageR (ShrinkFrom U) MirrorExpand
      ]
    ^++^
    ---------------------------------------------------------------------------
    -- | System
    ---------------------------------------------------------------------------
    keySet "System"
      [ key "Restart XMonad"                        "M-q"             $ spawn "xmonad --restart"
      , key "Rebuild & Restart XMonad"              "M-C-q"           $ spawn "xmonad --recompile && xmonad --restart"
      , key "Lock Screen"                           "M-s"             $ spawn "sleep 1; xset s activate"
      ]
    ^++^
    ---------------------------------------------------------------------------
    -- | Workspaces and Projects
    ---------------------------------------------------------------------------
    keySet "Workspaces & Projects" (
      [ key "Previous Non-Empty Workspace"          "M-S-,"           $ prevNonEmptyWS
      , key "Next Non-Empty Workspace"              "M-S-."           $ nextNonEmptyWS
      ]
      ++ keys "View Workspace"                      "M-"     wsKeys     (withNthWorkspace W.greedyView) [0..]
      ++ keys "Move window to Workspace"            "M-C-"   wsKeys     (withNthWorkspace W.shift) [0..]
    )
    ^++^
    ---------------------------------------------------------------------------
    -- | Screens
    ---------------------------------------------------------------------------
    keySet "Screens" (
      []
      ++ keys "Navigate Screens"                    "M-"     arrowKeys  (flip screenGo True) dirs
      ++ keys "Move Window to Screen"               "M-C-"   arrowKeys  (flip windowToScreen True) dirs
      ++ keys "Swap Workspace to Screen"            "M-S-C-" arrowKeys  (flip screenSwap True) dirs
    )
    ^++^
    ---------------------------------------------------------------------------
    -- | Windows
    ---------------------------------------------------------------------------
    keySet "Windows" (
      [ key "Kill"                                  "M-<Backspace>"   $ kill
      , key "Kill All"                              "M-S-<Backspace"  $ confirmPrompt tHotPrompt "kill all" $ killAll

      , key "Promote"                               "M-b"             $ promote

      , key "Switch to window"                      "M-w"             $ spawn "rofi -show window"

      , key "Un-merge from Sublayout"               "M-g"             $ withFocused (sendMessage . UnMerge)
      , key "Merge all into Sublayout"              "M-S-g"           $ withFocused (sendMessage . MergeAll)

      , key "Focus Master"                          "M-z m"           $ windows W.focusMaster
      -- , ("M-z u"            , addName "Focus urgent"                 $ focusUrgent)

      , key "Navigate Tabs Down"                    "M-'"             $ bindOn LD [("Tabs", windows W.focusDown), ("", onGroup W.focusDown')]
      , key "Navigate Tabs Up"                      "M-;"             $ bindOn LD [("Tabs", windows W.focusUp), ("", onGroup W.focusUp')]
      , key "Swap Tab Down"                         "M-C-'"           $ windows W.swapDown
      , key "Swap Tab Up"                           "M-C-;"           $ windows W.swapUp
      ]
      ++ keys "Navigate Window"                     "M-"     dirKeys    (flip windowGo True) dirs
      ++ keys "Move Window"                         "M-C-"   dirKeys    (flip windowSwap True) dirs
      ++ keys "Merge with Sublayout"                "M-S-"   dirKeys    (sendMessage . pullGroup) dirs
   )

  where
    keySet s ks = subtitle s : mkNamedKeymap config ks
    key n k a = (k, addName n a)
    keys n m ks a args = zipWith (\k arg -> key n (m ++ k) (a arg)) ks args

    dirKeys   = ["j", "k", "h", "l"]
    dirs      = [D, U, L, R ]
    wsKeys    = map show $ [1..9] ++ [0]
    arrowKeys = ["<D>", "<U>", "<L>", "<R>"]

    withSelection cmd = getSelection >>= spawn . cmd


    zipM m nm ks as f = zipWith (\k d -> key nm (m ++ k) (f d)) ks as

    toggleFloat w = windows $ \s ->
                      if M.member w (W.floating s)
                      then W.sink w s
                      else W.float w (W.RationalRect (1/5) (1/5) (3/5) (7/10)) s

    tryMessageR x y = sequence_ [(tryMessage_ x y), refresh]

    nextNonEmptyWS = findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1
            >>= \t -> (windows . W.view $ t)
    prevNonEmptyWS = findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1
            >>= \t -> (windows . W.view $ t)
    getSortByIndexNoSP =
            fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex


showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <- spawnPipe "zenity --text-info --font=terminus"
  hPutStr h (unlines $ showKm x)
  hClose h
  return ()


----------------------------------------------------------------------------}}}
-- Applications                                                             {{{
-------------------------------------------------------------------------------

myBrowser        = "chromium" -- TODO: maybe custom script that is workspace aware
myBrowserClass   = "chromium-browser"
myTerminal       = "kitty"
myTerminalClass  = "kitty"
myLauncher       = "rofi -matching fuzzy -modi combi -show combi -combi-modi run,drun"

spotify          = "spotify"
spotifyClass     = "Spotify"
isSpotify        = className =? spotifyClass

alsamixer        = "kitty --class alsamixer --title alsamixer alsamixer"
alsamixerClass   = "alsamixer"
isAlsamixer      = className =? alsamixerClass

vlc              = "vlc"
vlcResource      = "vlc"
isVLC            = resource =? vlcResource

console          = "kitty --class console --title console"
consoleClass     = "console"
isConsole        = resource =? consoleClass

glances          = "kitty --class glances --title glances glances"
glancesClass     = "glances"
isGlances        = className =? glancesClass


myScratchpads =
  [ (NS "spotify" spotify isSpotify defaultFloating)
  , (NS "alsamixer" alsamixer isAlsamixer (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)))
  , (NS "vlc" vlc isVLC defaultFloating)
  , (NS "console" console isConsole (customFloating $ W.RationalRect 0 0 1 (1/2)))
  ]

----------------------------------------------------------------------------}}}
-- Navigation                                                               {{{
-------------------------------------------------------------------------------

myNav2DConfig = def
  { defaultTiledNavigation = centerNavigation
  , floatNavigation        = centerNavigation
  , screenNavigation       = lineNavigation
  , layoutNavigation       = [("Full", centerNavigation)]
  , unmappedWindowRect     = [("Full", singleWindowRect)]
  }

----------------------------------------------------------------------------}}}
-- Layouts                                                                  {{{
-------------------------------------------------------------------------------

myLayoutHook = showWorkspaceName
             $ fullscreenFloat
             $ fullScreenToggle
             $ mirrorToggle
             $ reflectToggle
             $ flex ||| tabs

 where

    showWorkspaceName = showWName' tShowWName

    fullScreenToggle = mkToggle (single FULL)
    mirrorToggle = mkToggle (single MIRROR)
    reflectToggle = mkToggle (single REFLECTX)

    -- Flex Layout
    flex = trimNamed 5 "Flex"
         $ avoidStruts
         $ windowNavigation
         $ addTopBar
         $ addTabs shrinkText tTab
         $ subLayout [] (Simplest ||| Accordion)
         $ ifWider smallMonitorWidth wideLayouts standardLayouts

    smallMonitorWidth = 1920

    wideLayouts = myGaps
                $ mySpacing
                $ (suffixed "Wide 3 Column" $ ThreeColMid 1 (1/20) (1/2))
              ||| (trimSuffixed 1 "Wide BSP" $ hiddenWindows emptyBSP)

    standardLayouts = myGaps
                    $ mySpacing
                    $ (suffixed "Standard 2/3" $ ResizableTall 1 (1/20) (2/3) [])
                  ||| (suffixed "Standard 1/2" $ ResizableTall 1 (1/20) (1/2) [])

    -- Tabs Layout
    tabs = named "Tabs"
         $ avoidStruts
         $ addTopBar
         $ addTabs shrinkText tTab
         $ Simplest

    named n             = renamed [(XMonad.Layout.Renamed.Replace n)]
    trimNamed w n       = renamed [(XMonad.Layout.Renamed.CutWordsLeft w),
                                   (XMonad.Layout.Renamed.PrependWords n)]
    suffixed n          = renamed [(XMonad.Layout.Renamed.AppendWords n)]
    trimSuffixed w n    = renamed [(XMonad.Layout.Renamed.CutWordsRight w),
                                   (XMonad.Layout.Renamed.AppendWords n)]

    addTopBar = noFrillsDeco shrinkText tTopBar

    myGaps    = gaps [(U, sGap), (D, sGap), (L, sGap), (R, sGap)]
    mySpacing = spacing sGap


----------------------------------------------------------------------------}}}
-- LogHook                                                                  {{{
-------------------------------------------------------------------------------

myLogHook =
  fadeWindowsLogHook myFadeHook

myFadeHook = composeAll
  [ opaque
  , isUnfocused --> opacity 0.85
  , (className =? myTerminalClass) <&&> (isUnfocused) --> opacity 0.9
  , isConsole --> opacity 0.9
  , isDialog --> opaque
  ]


----------------------------------------------------------------------------}}}
-- ManageHook                                                               }}}
-------------------------------------------------------------------------------

myManageHook :: ManageHook
myManageHook =
      manageApps
  <+> manageScratchpads
  <+> manageSpawn
  where
    manageApps = composeOne
      -- Applications
      [ isVLC                              -?> doFloat
      , isSpotify                          -?> doFloat
      -- Dialogs
      , isBrowserDialog                    -?> forceCenterFloat
      , isFileChooserDialog                -?> forceCenterFloat
      , isDialog                           -?> doCenterFloat
      , isPopup                            -?> doCenterFloat
      , isSplash                           -?> doCenterFloat
      -- Other
      , transience
      , isFullscreen                       -?> doFullFloat
      , pure True                          -?> tileBelow
      ]
    isBrowserDialog     = isDialog <&&> className =? myBrowserClass
    isFileChooserDialog = isRole =? "GtkFileChooserDialog"
    isPopup             = isRole =? "pop-up"
    isSplash            = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"
    isRole              = stringProperty "WM_WINDOW_ROLE"
    tileBelow           = insertPosition Below Newer

    manageScratchpads = namedScratchpadManageHook myScratchpads

    forceCenterFloat = doFloatDep move
      where
        move :: W.RationalRect -> W.RationalRect
        move _ = W.RationalRect x y w h

        w, h, x, y :: Rational
        w = 1/3
        h = 1/2
        x = (1-w)/2
        y = (1-h)/2


----------------------------------------------------------------------------}}}
-- HandleEventHook                                                          {{{
-------------------------------------------------------------------------------

myHandleEventHook =
      fadeWindowsEventHook
  <+> handleEventHook def
  <+> XMonad.Layout.Fullscreen.fullscreenEventHook

----------------------------------------------------------------------------}}}

