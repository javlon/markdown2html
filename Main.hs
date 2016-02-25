module Main where

import Data.Text.Lazy
import Graphics.UI.Gtk
import Text.Pandoc
import Help

main :: IO ()
main = do
  initGUI
  window <- windowNew
  set window [windowDefaultWidth := 800, windowDefaultHeight := 600,
            windowTitle := "Text Entry", containerBorderWidth := 10]

  vb <- vBoxNew False 10
  containerAdd window vb

  btnOpenFile   <- buttonNewWithLabel "Open File"
  btnConvertToH <- buttonNewWithLabel "Convert to Html"
  btnConvertToM <- buttonNewWithLabel "Convert to Markdown"
  btnCopy       <- buttonNewWithLabel "Copy"
  btnSave       <- buttonNewWithLabel "Save"

  startHB <- hBoxNew False 10
  pathToFile <- entryNew
  btnWorkInH <- radioButtonNewWithLabel "HTML"
  btnWorkInM <- radioButtonNewWithLabelFromWidget btnWorkInH "Markdown"
  boxPackStart startHB pathToFile PackGrow 0
  boxPackEnd startHB btnOpenFile PackNatural 0
  boxPackEnd startHB btnWorkInH PackNatural 0
  boxPackEnd startHB btnWorkInM PackNatural 0
  toggleButtonSetActive btnWorkInM True
  boxPackStart vb startHB PackNatural 0

  labelHBox <- hBoxNew False 10
  labH <- labelNewWithMnemonic "HTML"
  labM <- labelNewWithMnemonic "Markdown"
  boxPackStart labelHBox labM PackNatural 0
  boxPackEnd labelHBox labH PackNatural 0
  boxPackStart vb labelHBox PackNatural 0

  leftBuff  <- textBufferNew Nothing
  rightBuff <- textBufferNew Nothing
  let buffPair = (leftBuff, rightBuff)
  left  <- textViewNewWithBuffer leftBuff
  right <- textViewNewWithBuffer rightBuff
  scrollL <- scrolledWindowNew Nothing Nothing
  scrollR <- scrolledWindowNew Nothing Nothing
  containerAdd scrollL left
  containerAdd scrollR right
  hb <- hBoxNew True 10
  boxPackStart hb scrollL PackGrow 0
  boxPackStart hb scrollR PackGrow 0
  boxPackStart vb hb PackGrow 0

  endHB <- hBoxNew False 10
  boxPackStart endHB btnConvertToH PackNatural 0
  boxPackEnd endHB btnConvertToM PackNatural 0
  boxPackEnd endHB btnCopy PackNatural 0
  boxPackEnd endHB btnSave PackNatural 0
  boxPackStart vb endHB PackNatural 0

  onPressed btnConvertToH $ converting True  buffPair
  onPressed btnConvertToM $ converting False buffPair
  onEntryActivate pathToFile $ openFile btnWorkInM buffPair pathToFile
  onPressed btnOpenFile      $ openFile btnWorkInM buffPair pathToFile
  onPressed btnCopy $ clipboard  btnWorkInM buffPair
  onPressed btnSave $ saveAsFile btnWorkInM buffPair

  widgetShowAll window
  onDestroy window mainQuit
  mainGUI
