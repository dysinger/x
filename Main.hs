{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad.Reader (join, void)
import Data.Bits (Bits((.|.)))
import qualified Data.Map as M (fromList)
import Data.Ratio ((%))
import Data.Time (getCurrentTime)
import Distribution.PackageDescription.TH
       (PackageDescription(package), PackageIdentifier(pkgVersion),
        packageVariable)
import Graphics.X11.Xlib
       (xK_g, xK_z, xK_space, mod4Mask, controlMask)
import Language.Haskell.TH (runIO, stringE)
import Options.Applicative
       (helper, execParser, switch, subparser, short, progDesc, long,
        info, help, header, fullDesc, command, (<>))
import System.Environment (withArgs)
import XMonad
       (XConfig(XConfig, focusedBorderColor, keys, layoutHook, manageHook,
                modMask),
        Tall(Tall), Default(def), withFocused, doIgnore, doFloat,
        composeAll, className, (=?), (<+>), (-->), xmonadNoargs, (|||),
        installSignalHandlers)
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Config.Gnome (gnomeConfig)
import XMonad.Config.Kde (kdeConfig)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Layout.Accordion (Accordion(Accordion))
import XMonad.Layout.ComboP (Property(Title))
import XMonad.Layout.Grid (Grid(Grid))
import XMonad.Layout.IM (withIM)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Roledex (Roledex(Roledex))
import XMonad.Layout.TabBarDecoration (XPPosition(Bottom))
import XMonad.Prompt
       (XPConfig(autoComplete, font, height, position))
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Util.Replace (replace)

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
        , font = "xft:Fira Mono:medium:size=13:antialias=true:hinting=light"
        , height = 32
        , position = Bottom
        }
