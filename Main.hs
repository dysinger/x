{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Arrow (second)
import Control.Monad.Reader
import Control.Monad.State
import Data.Bits
import Data.Function
import Data.List ((\\))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid (getAll)
import qualified Data.Set as S
import Data.Time (getCurrentTime)
import Data.Version (showVersion)
import Data.Void
import Distribution.PackageDescription.TH
       (PackageDescription(package), PackageIdentifier(pkgVersion),
        packageVariable)
import Graphics.X11.Xinerama (compiledWithXinerama)
import Graphics.X11.Xlib hiding (refreshKeyboardMapping)
import Graphics.X11.Xlib.Extras
import Language.Haskell.TH (runIO, stringE)
import Options.Applicative
import System.Environment
import System.Exit (exitFailure)
import System.FilePath
import System.IO
import System.Info
import System.Locale.SetLocale
import System.Posix.Process (executeFile)
import qualified XMonad.Config as Default
import XMonad.Config.Gnome
import XMonad.Config.Kde
import XMonad.Core
import XMonad.Operations
import XMonad.StackSet (new, floating, member)
import qualified XMonad.StackSet as W

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
                 (helper <*> (xmonad gnomeConfig <$> dummyFlag))
                 (fullDesc <> progDesc "Gnome XMonad Configuration"))
    kdeCmd =
        command
            "kde"
            (info
                 (helper <*> (xmonad kdeConfig <$> dummyFlag))
                 (fullDesc <> progDesc "KDE XMonad Configuration"))
    dummyFlag = switch (long "dummy" <> short 'd' <> help "Flag something")
    xmonad conf _ = do
        installSignalHandlers
        sendReplace
        withArgs [] $
            xmonadNoargs
                conf
                { modMask = mod4Mask
                }
                Nothing
                Nothing

{-

The code below this line was not exported by XMonad so we had to copy it locally
here. See the LICENSE file for copyright details.

-}

sendReplace :: IO ()
sendReplace = do
    dpy <- openDisplay ""
    let dflt = defaultScreen dpy
    rootw <- rootWindow dpy dflt
    replace dpy dflt rootw

xmonadNoargs
    :: (LayoutClass l Window, Read (l Window))
    => XConfig l -> Maybe String -> Maybe String -> IO ()
xmonadNoargs initxmc serializedWinset serializedExtstate = do
    setLocale LC_ALL (Just "")
    installSignalHandlers
    let xmc =
            initxmc
            { layoutHook = Layout $ layoutHook initxmc
            }
    dpy <- openDisplay ""
    let dflt = defaultScreen dpy
    rootw <- rootWindow dpy dflt
    selectInput dpy rootw $ rootMask initxmc
    sync dpy False
    xSetErrorHandler
    xinesc <- getCleanedScreenInfo dpy
    nbc <-
        do v <- initColor dpy $ normalBorderColor xmc
           ~(Just nbc_) <- initColor dpy $ normalBorderColor Default.def
           return (fromMaybe nbc_ v)
    fbc <-
        do v <- initColor dpy $ focusedBorderColor xmc
           ~(Just fbc_) <- initColor dpy $ focusedBorderColor Default.def
           return (fromMaybe fbc_ v)
    hSetBuffering stdout NoBuffering
    let layout = layoutHook xmc
        lreads = readsLayout layout
        initialWinset =
            let padToLen n xs = take (max n (length xs)) $ xs ++ repeat ""
            in new layout (padToLen (length xinesc) (workspaces xmc)) $
               map SD xinesc
        maybeRead reads' s =
            case reads' s of
                [(x,"")] -> Just x
                _ -> Nothing
        winset =
            fromMaybe initialWinset $
            do s <- serializedWinset
               ws <- maybeRead reads s
               return . W.ensureTags layout (workspaces xmc) $
                   W.mapLayout (fromMaybe layout . maybeRead lreads) ws
        extState =
            fromMaybe M.empty $
            do dyns <- serializedExtstate
               vals <- maybeRead reads dyns
               return . M.fromList . map (second Left) $ vals
        cf =
            XConf
            { display = dpy
            , config = xmc
            , theRoot = rootw
            , normalBorder = nbc
            , focusedBorder = fbc
            , keyActions = keys xmc xmc
            , buttonActions = mouseBindings xmc xmc
            , mouseFocused = False
            , mousePosition = Nothing
            , currentEvent = Nothing
            }
        st =
            XState
            { windowset = initialWinset
            , numberlockMask = 0
            , mapped = S.empty
            , waitingUnmap = M.empty
            , dragging = Nothing
            , extensibleState = extState
            }
    allocaXEvent $
        \e ->
             runX cf st $
             do setNumlockMask
                grabKeys
                grabButtons
                io $ sync dpy False
                ws <- io $ scan dpy rootw
                windows . const . foldr W.delete winset $
                    W.allWindows winset \\ ws
                mapM_ manage (ws \\ W.allWindows winset)
                userCode $ startupHook initxmc
                forever $ prehandle =<< io (nextEvent dpy e >> getEvent e)
    return ()
  where
    prehandle e =
        let mouse = do
                guard (ev_event_type e `elem` evs)
                return (fromIntegral (ev_x_root e), fromIntegral (ev_y_root e))
        in local
               (\c ->
                     c
                     { mousePosition = mouse
                     , currentEvent = Just e
                     })
               (handleWithHook e)
    evs =
        [ keyPress
        , keyRelease
        , enterNotify
        , leaveNotify
        , buttonPress
        , buttonRelease]

handleWithHook :: Event -> X ()
handleWithHook e = do
    evHook <- asks (handleEventHook . config)
    whenX (userCodeDef True $ getAll `fmap` evHook e) (handle e)

handle :: Event -> X ()
handle (KeyEvent{ev_event_type = t,ev_state = m,ev_keycode = code})
  | t == keyPress =
      withDisplay $
      \dpy -> do
          s <- io $ keycodeToKeysym dpy code 0
          mClean <- cleanMask m
          ks <- asks keyActions
          userCodeDef () $ whenJust (M.lookup (mClean, s) ks) id
handle (MapRequestEvent{ev_window = w}) =
    withDisplay $
    \dpy -> do
        wa <- io $ getWindowAttributes dpy w
        managed <- isClient w
        when (not (wa_override_redirect wa) && not managed) $ do manage w
handle (DestroyWindowEvent{ev_window = w}) =
    whenX (isClient w) $
    do unmanage w
       modify
           (\s ->
                 s
                 { mapped = S.delete w (mapped s)
                 , waitingUnmap = M.delete w (waitingUnmap s)
                 })
handle (UnmapEvent{ev_window = w,ev_send_event = synthetic}) =
    whenX (isClient w) $
    do e <- gets (fromMaybe 0 . M.lookup w . waitingUnmap)
       if (synthetic || e == 0)
           then unmanage w
           else modify
                    (\s ->
                          s
                          { waitingUnmap = M.update mpred w (waitingUnmap s)
                          })
  where
    mpred 1 = Nothing
    mpred n = Just $ pred n
handle e@(MappingNotifyEvent{}) = do
    io $ refreshKeyboardMapping e
    when (ev_request e `elem` [mappingKeyboard, mappingModifier]) $
        do setNumlockMask
           grabKeys
handle e@(ButtonEvent{ev_event_type = t})
  | t == buttonRelease = do
      drag <- gets dragging
      case drag of
          Just (_,f) ->
              modify
                  (\s ->
                        s
                        { dragging = Nothing
                        }) >>
              f
          Nothing -> broadcastMessage e
handle e@(MotionEvent{ev_event_type = _t,ev_x = x,ev_y = y}) = do
    drag <- gets dragging
    case drag of
        Just (d,_) -> d (fromIntegral x) (fromIntegral y)
        Nothing -> broadcastMessage e
handle e@(ButtonEvent{ev_window = w,ev_event_type = t,ev_button = b})
  | t == buttonPress = do
      dpy <- asks display
      isr <- isRoot w
      m <- cleanMask $ ev_state e
      mact <- asks (M.lookup (m, b) . buttonActions)
      case mact of
          Just act
            | isr -> act $ ev_subwindow e
          _ -> do
              focus w
              ctf <- asks (clickJustFocuses . config)
              unless ctf $ io (allowEvents dpy replayPointer currentTime)
      broadcastMessage e
handle e@(CrossingEvent{ev_window = w,ev_event_type = t})
  | t == enterNotify && ev_mode e == notifyNormal =
      whenX (asks $ focusFollowsMouse . config) (focus w)
handle e@(CrossingEvent{ev_event_type = t})
  | t == leaveNotify = do
      rootw <- asks theRoot
      when (ev_window e == rootw && not (ev_same_screen e)) $ setFocusX rootw
handle e@(ConfigureRequestEvent{ev_window = w}) =
    withDisplay $
    \dpy -> do
        ws <- gets windowset
        wa <- io $ getWindowAttributes dpy w
        bw <- asks (borderWidth . config)
        if M.member w (floating ws) || not (member w ws)
            then do
                io $
                    configureWindow dpy w (ev_value_mask e) $
                    WindowChanges
                    { wc_x = ev_x e
                    , wc_y = ev_y e
                    , wc_width = ev_width e
                    , wc_height = ev_height e
                    , wc_border_width = fromIntegral bw
                    , wc_sibling = ev_above e
                    , wc_stack_mode = ev_detail e
                    }
                when (member w ws) (float w)
            else io $
                 allocaXEvent $
                 \ev -> do
                     setEventType ev configureNotify
                     setConfigureEvent
                         ev
                         w
                         w
                         (wa_x wa)
                         (wa_y wa)
                         (wa_width wa)
                         (wa_height wa)
                         (ev_border_width e)
                         none
                         (wa_override_redirect wa)
                     sendEvent dpy w False 0 ev
        io $ sync dpy False
handle (ConfigureEvent {ev_window = w}) = whenX (isRoot w) rescreen
handle event@(PropertyEvent{ev_event_type = t,ev_atom = a})
  | t == propertyNotify && a == wM_NAME =
      asks (logHook . config) >>= userCodeDef () >> broadcastMessage event
handle e@ClientMessageEvent{ev_message_type = mt} = do
    a <- getAtom "XMONAD_RESTART"
    if (mt == a)
        then restart "xmonad" True
        else broadcastMessage e
handle e = broadcastMessage e

scan :: Display -> Window -> IO [Window]
scan dpy rootw = do
    (_,_,ws) <- queryTree dpy rootw
    filterM ok ws
  where
    ok w = do
        wa <- getWindowAttributes dpy w
        a <- internAtom dpy "WM_STATE" False
        p <- getWindowProperty32 dpy a w
        let ic =
                case p of
                    Just (3:_) -> True
                    _ -> False
        return $
            not (wa_override_redirect wa) &&
            (wa_map_state wa == waIsViewable || ic)

setNumlockMask :: X ()
setNumlockMask = do
    dpy <- asks display
    ms <- io $ getModifierMapping dpy
    xs <-
        sequence
            [ do ks <- io $ keycodeToKeysym dpy kc 0
                 if ks == xK_Num_Lock
                     then return (setBit 0 (fromIntegral m))
                     else return (0 :: KeyMask)
            | (m,kcs) <- ms
            , kc <- kcs
            , kc /= 0 ]
    modify
        (\s ->
              s
              { numberlockMask = foldr (.|.) 0 xs
              })

grabKeys :: X ()
grabKeys = do
    XConf{display = dpy,theRoot = rootw} <- ask
    let grab kc m =
            io $ grabKey dpy kc m rootw True grabModeAsync grabModeAsync
        (minCode,maxCode) = displayKeycodes dpy
        allCodes = [fromIntegral minCode .. fromIntegral maxCode]
    io $ ungrabKey dpy anyKey anyModifier rootw
    ks <- asks keyActions
    syms <-
        forM allCodes $
        \code ->
             io (keycodeToKeysym dpy code 0)
    let keysymMap =
            M.fromListWith
                (++)
                (zip
                     syms
                     [ [code]
                     | code <- allCodes ])
        keysymToKeycodes sym = M.findWithDefault [] sym keysymMap
    forM_ (M.keys ks) $
        \(mask,sym) ->
             forM_ (keysymToKeycodes sym) $
             \kc ->
                  mapM_ (grab kc . (mask .|.)) =<< extraModifiers

grabButtons :: X ()
grabButtons = do
    XConf{display = dpy,theRoot = rootw} <- ask
    let grab button mask =
            io $
            grabButton
                dpy
                button
                mask
                rootw
                False
                buttonPressMask
                grabModeAsync
                grabModeSync
                none
                none
    io $ ungrabButton dpy anyButton anyModifier rootw
    ems <- extraModifiers
    ba <- asks buttonActions
    mapM_
        (\(m,b) ->
              mapM_ (grab b . (m .|.)) ems)
        (M.keys $ ba)

replace :: Display -> ScreenNumber -> Window -> IO ()
replace dpy dflt rootw = do
    wmSnAtom <- internAtom dpy ("WM_S" ++ show dflt) False
    currentWmSnOwner <- xGetSelectionOwner dpy wmSnAtom
    when (currentWmSnOwner /= 0) $
        do selectInput dpy currentWmSnOwner structureNotifyMask
           netWmSnOwner <-
               allocaSetWindowAttributes $
               \attributes -> do
                   set_override_redirect attributes True
                   set_event_mask attributes propertyChangeMask
                   let screen = defaultScreenOfDisplay dpy
                       visual = defaultVisualOfScreen screen
                       attrmask = cWOverrideRedirect .|. cWEventMask
                   createWindow
                       dpy
                       rootw
                       (-100)
                       (-100)
                       1
                       1
                       0
                       copyFromParent
                       copyFromParent
                       visual
                       attrmask
                       attributes
           xSetSelectionOwner dpy wmSnAtom netWmSnOwner currentTime
           fix $
               \again -> do
                   evt <-
                       allocaXEvent $
                       \event -> do
                           windowEvent
                               dpy
                               currentWmSnOwner
                               structureNotifyMask
                               event
                           get_EventType event
                   when (evt /= destroyNotify) again
