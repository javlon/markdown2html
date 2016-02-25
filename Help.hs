module Help (getSource, converting, clipboard, openFile, saveAsFile) where

import Codec.Binary.UTF8.String
import Control.Exception as E
import Control.Lens
import Control.Monad
import Data.ByteString.Lazy hiding (putStr, dropWhile, writeFile)
import Graphics.UI.Gtk
import Network.HTTP.Client hiding (checkStatus, responseBody)
import Network.Wreq as NW
import Text.Pandoc


-- The getSource' function takes url address and returns source from there on
-- box.
getSource' :: String -> IO String
getSource' url = do
  r <- NW.get url
  return . decode . unpack $ r ^. responseBody


-- The getSource function call getSource' function and handle
-- InvalidUrlException, StatusCodeException and other exceptions.
getSource :: String -> IO String
getSource url = getSource' url `E.catch` handler
  where
    handler (InvalidUrlException _ _)   = return "Invalid URL exception"
    handler (StatusCodeException _ _ _) = return "Status code exception!"
    handler x                           = return $ show x


-- The converting function converts source from left to right if predicate is 
-- true and vice versa if it's false.
converting :: Bool -> (TextBuffer, TextBuffer) -> IO ()
converting p (left, right) = do
  str <- textBufferGetAllText leftBuff
  let conv = if p 
             then either show (writeHtmlString def) $ readMarkdown def str
             else either show (writeMarkdown def)   $ readHtml def str
  textBufferSetText rightBuff conv
  where (leftBuff, rightBuff) = if p then (left, right) else (right, left)


-- The clipboard function save the source from left and right to clipboard if
-- predicate is false and true respectively.
clipboard :: RadioButton -> (TextBuffer, TextBuffer) -> IO ()
clipboard p (left, right) = do
  first <- toggleButtonGetActive p
  let buff = if first then right else left
  join $ liftM3 textBufferSelectRange (return buff)
            (textBufferGetStartIter buff)
            (textBufferGetEndIter buff)
  join $ liftM2 textBufferCopyClipboard (return buff) 
            (clipboardGet selectionClipboard)


-- The openFile function writes source from url to left and right if
-- predicate is true and false respectively.
openFile :: RadioButton -> (TextBuffer,TextBuffer) -> Entry -> IO ()
openFile p (left, right) url = do
  first <- toggleButtonGetActive p
  let writeHere = if first then left else right
  entryGetText url >>= getSource >>= textBufferSetText writeHere


-- The saveAsFile function runs the widget where you can save the source of
-- left and right buffer in a file if predicate is true and false respectively
saveAsFile :: RadioButton -> (TextBuffer, TextBuffer) -> IO ()
saveAsFile p (left, right) = do
  first <- toggleButtonGetActive p
  let buff = if first then right else left
      name = if first then "HTML" else "markdown"
  fchs <- fileChooserDialogNew (Just $ "Save as " ++ name) Nothing
            FileChooserActionSave
            [("Cancel", ResponseCancel),("Save", ResponseAccept)]
  fileChooserSetDoOverwriteConfirmation fchs True
  response <- dialogRun fchs
  case response of
    ResponseDeleteEvent -> widgetDestroy fchs
    ResponseCancel      -> widgetDestroy fchs
    ResponseAccept      -> do 
        nwf <- fileChooserGetFilename fchs
        case nwf of
            Nothing   -> putStr "Nothing"
            Just path -> writeFile path =<< textBufferGetAllText buff
        widgetDestroy fchs
  return ()


-- The textBufferGetAllText function takes buffer and return a sourse of buffer
-- in a box IO
textBufferGetAllText :: TextBuffer -> IO String
textBufferGetAllText buff = do
  join $ liftM4 textBufferGetText (return buff) 
            (textBufferGetStartIter buff) 
            (textBufferGetEndIter buff) (return True)
