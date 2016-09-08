{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Arrow (second)
import Control.Monad.Reader
import Control.Monad.State
import Data.Bits
import Data.List ((\\))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid (getAll)
import Data.Ratio ((%))
import qualified Data.Set as S
import Data.Time (getCurrentTime)
import Distribution.PackageDescription.TH
       (PackageDescription(package), PackageIdentifier(pkgVersion),
        packageVariable)
import Graphics.X11.Xlib hiding (refreshKeyboardMapping)
import Graphics.X11.Xlib.Extras
import Language.Haskell.TH (runIO, stringE)
import Options.Applicative
import System.Environment
import System.IO
import System.Locale.SetLocale
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.NoBorders
import qualified XMonad.Config as Default
import XMonad.Config.Gnome
import XMonad.Config.Kde
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Accordion
import XMonad.Layout.ComboP
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Roledex
import XMonad.Layout.TabBarDecoration
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.StackSet (new, floating, member)
import qualified XMonad.StackSet as W
import XMonad.Util.Replace

main :: IO ()
main =
    void . join $
    execParser
        (info
             (helper <*> subparser (gnomeCmd <> kdeCmd))
             (fullDesc <>
              header
                  ("x " <> $(packageVariable (pkgVersion . package)) <> " " <>
                   $(stringE =<< runIO (show `fmap` getCurrentTime))) <>
              progDesc "XMonad Window Manager"))
  where
    gnomeCmd =
        command
            "gnome"
            (info
                 (helper <*> (run gnomeConfig <$> dummyFlag))
                 (fullDesc <> progDesc "Gnome XMonad Configuration"))
    kdeCmd =
        command
            "kde"
            (info
                 (helper <*> (run kdeConfig <$> dummyFlag))
                 (fullDesc <> progDesc "KDE XMonad Configuration"))
    dummyFlag = switch (long "dummy" <> short 'd' <> help "Flag something")
    run cfg _flag = do
        replace
        installSignalHandlers
        withArgs [] $
            xmonadNoargs
                cfg
                { modMask = mod4Mask
                , keys = myKeys <+> keys cfg
                , manageHook = myManageHook <+> manageHook cfg
                , layoutHook = myLayoutHook cfg
                , focusedBorderColor = "white"
                }
                Nothing
                Nothing
    myManageHook =
        composeAll
            [ className =? "Dia" --> doFloat
            , className =? "Gimp-2.8" --> doFloat
            , className =? "vncviewer" --> doFloat
            , className =? "VirtualBox" --> doFloat
            , className =? "Glade" --> doFloat
            , className =? "Synapse" --> doIgnore
            , isFullscreen --> doFullFloat]
    myLayoutHook cfg =
        onWorkspace
            "9"
            (withIM (1 % 7) (Title "tim.dysinger - Skype™") (Tall 1 0.5 0.5)) $
        layoutHook cfg ||| Accordion ||| Grid ||| Roledex
    myKeys XConfig{XMonad.modMask = modm'} =
        M.fromList
            [ ((modm', xK_z), toggleWS)
            , ((modm', xK_g), withFocused toggleBorder)
            , ((modm' .|. controlMask, xK_space), shellPrompt myXPConfig)]
    myXPConfig =
        def
        { autoComplete = Just 1
        , font = "xft:Fira Mono:medium:size=10:antialias=true:hinting=light"
        , position = Bottom
        }
