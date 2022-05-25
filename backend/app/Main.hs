module Main where

import Control.Monad.State
import qualified Data.Foldable as List
import Data.Int (Int64, Int8)
import qualified Data.Vector as Vector
import System.Environment

data Instr
  = PushI Int64
  | PushD Double
  | PushB Bool
  | PushC Char
  | PushS String
  | Load Int64
  | Ref Int64
  | BinOp String
  | PrefixOp String
  | Print
  | Ret Int8
  | Call String Int
  | Jmp Int64
  | Jnz Int64
  | Label String
  deriving (Show, Read)

data Data
  = I Int64
  | D Double
  | B Bool
  | C Char
  | S String
  | Err String

instance Show Data where
  show (I i) = show i
  show (D d) = show d
  show (B b) = show b
  show (C c) = show c
  show (S s) = s
  show (Err s) = "Error : " ++ s

-- binApply :: (a -> a -> a) -> Data -> Data -> Data
-- binApply f (I i) (I j) = I (f i j)
-- binApply f (D i) (D j) = D (f i j)
-- binApply f (B i) (B j) = B (f i j)
-- binApply f (S i) (S j) = S (f i j)
-- binApply f (C i) (C j) = C (f i j)
-- binApply f _ _ = Err

data MachineState = MachineState
  { fp :: [Int],
    ip :: Int,
    stack :: [Data],
    stackLen :: Int,
    retAddr :: [Int],
    allLabels :: [(Int, String)],
    argsCnt :: [Int]
  }
  deriving (Show)

isLabel i = case i of
  (_, Label _) -> True
  _ -> False

extractLabelName (i, Label x) = (i, x)

getLabelAddr label = List.find (\(i, l) -> l == label)

actualIndex :: Int -> State MachineState Int
actualIndex i = do
  st <- get
  return $ stackLen st - (i + 1)

changeValAt i lst newVal =
  let (x, _ : xs) = Prelude.splitAt i lst
   in (x ++ newVal : xs)

getAt :: Int -> State MachineState Data
getAt i = do
  st <- get
  let index = stackLen st - (i + 1)
  return $ stack st !! index

-- There are quite  a few problems
--
-- - Data access is inverted due to List's nature in Haskell
-- - Remove args pushed into the stack when we return from function call
--

emulate :: Instr -> State MachineState (Maybe Data)
emulate instr = do
  st <- get
  case instr of
    PushI i -> do
      put $ st {stack = I i : stack st, stackLen = stackLen st + 1}
      return Nothing
    PushD d -> do
      put $ st {stack = D d : stack st, stackLen = stackLen st + 1}
      return Nothing
    PushB b -> do
      put $ st {stack = B b : stack st, stackLen = stackLen st + 1}
      return Nothing
    PushC c -> do
      put $ st {stack = C c : stack st, stackLen = stackLen st + 1}
      return Nothing
    PushS s -> do
      put $ st {stack = S s : stack st, stackLen = stackLen st + 1}
      return Nothing
    Load i -> do
      index <- actualIndex (head (fp st) + fromIntegral i)
      let newStack = tail (changeValAt index (stack st) (head $ stack st))
      put st {stack = newStack, stackLen = stackLen st - 1}
      return Nothing
    Ref i -> do
      elem <- getAt (head (fp st) + fromIntegral i)
      let newStack = elem : stack st
      put $ st {stack = newStack, stackLen = stackLen st + 1}
      return Nothing
    BinOp o -> do
      let rhs = (head . stack) st
      let lhs = (head . tail . stack) st
      let newSt = (tail . tail . stack) st
      let newTop =
            case (lhs, rhs) of
              (I i, I j) ->
                case o of
                  "+" -> I (i + j)
                  "-" -> I (i - j)
                  "*" -> I (i * j)
                  "/" -> I (div i j)
                  "!=" -> B (i /= j)
                  "==" -> B (i == j)
                  ">" -> B (i > j)
                  ">=" -> B (i >= j)
                  "<" -> B (i < j)
                  "<=" -> B (i <= j)
                  _ -> Err $ "Invalid Operator : " ++ o ++ " used on two int"
              (D i, D j) ->
                case o of
                  "+" -> D (i + j)
                  "-" -> D (i - j)
                  "*" -> D (i * j)
                  "/" -> D (i / j)
                  "!=" -> B (i /= j)
                  "==" -> B (i == j)
                  ">" -> B (i > j)
                  ">=" -> B (i >= j)
                  "<" -> B (i < j)
                  "<=" -> B (i <= j)
                  _ -> Err $ "Invalid Operator : " ++ o ++ " used on two double"
              (C i, C j) ->
                case o of
                  "+" -> S (show i ++ show j)
                  "!=" -> B (i /= j)
                  "==" -> B (i == j)
                  ">" -> B (i > j)
                  ">=" -> B (i >= j)
                  "<" -> B (i < j)
                  "<=" -> B (i <= j)
                  _ -> Err $ "Invalid Operator : " ++ o ++ " used on two char"
              (B i, B j) ->
                case o of
                  "!=" -> B (i /= j)
                  "==" -> B (i == j)
                  ">" -> B (i > j)
                  ">=" -> B (i >= j)
                  "<" -> B (i < j)
                  "<=" -> B (i <= j)
                  "and" -> B (i && j)
                  "or" -> B (i || j)
                  _ -> Err $ "Invalid Operator : " ++ o ++ " used on two bool"
              (S i, S j) ->
                case o of
                  "+" -> S (i ++ j)
                  "!=" -> B (i /= j)
                  "==" -> B (i == j)
                  ">" -> B (i > j)
                  ">=" -> B (i >= j)
                  "<" -> B (i < j)
                  "<=" -> B (i <= j)
                  _ -> Err $ "Invalid Operator : " ++ o ++ " used on two string"
              _ ->
                Err $
                  "Type mismatch in lhs and rhs: lhs is "
                    ++ show lhs
                    ++ " ,rhs is "
                    ++ show rhs
                    ++ " ,and op is "
                    ++ o
      put st {stack = newTop : (tail . tail . stack) st, stackLen = stackLen st - 1}
      return Nothing
    PrefixOp o -> do
      let rhs = (head . stack) st
      let newSt = (tail . stack) st
      let newTop =
            case rhs of
              I i ->
                case o of
                  "-" -> I (- i)
                  _ -> Err $ "Invalid operator : " ++ o ++ " used on int"
              D d ->
                case o of
                  "-" -> D (- d)
                  _ -> Err $ "Invalid operator : " ++ o ++ " used on double"
              B b ->
                case o of
                  "!" -> B (not b)
                  _ -> Err $ "Invalid operator : " ++ o ++ " used on bool"
              _ -> Err $ show rhs ++ " doesnt support any unary operator, Operator used : " ++ o
      put st {stack = newTop : (tail . stack) st}
      return Nothing
    Print -> do
      let top = (head . stack) st
      put $ st {stack = (tail . stack) st, stackLen = stackLen st - 1}
      return $ Just top
    Ret yes -> do
      let newFp = (tail . fp) st
      let newIp = (head . retAddr) st
      put
        st
          { ip = newIp,
            fp = newFp,
            stack =
              if yes == 1
                then (head . stack) st : drop ((head . argsCnt) st) ((tail . stack) st)
                else drop ((head . argsCnt) st) (stack st),
            argsCnt = (tail . argsCnt) st,
            stackLen = stackLen st - (head . argsCnt) st,
            retAddr = (tail . retAddr) st
          }
      return Nothing
    Call lbl cnt -> do
      case getLabelAddr lbl (allLabels st) of
        Just (ptr, _) -> do
          put $ st {retAddr = ip st : retAddr st, ip = ptr, fp = stackLen st : fp st, argsCnt = cnt : argsCnt st}
          return Nothing
        Nothing -> return Nothing
    Jmp off -> do
      put $ st {ip = ip st + fromIntegral off}
      return Nothing
    Jnz off -> do
      let top = head $ stack st
      let jump =
            case top of
              B t -> t
              D d -> d /= 0
              I i -> i /= 0
              _ -> False
      let newStack = (tail . stack) st
      let newLen = stackLen st - 1
      if jump
        then
          ( do
              put $ st {ip = ip st + fromIntegral off, stack = newStack, stackLen = newLen}
          )
        else
          ( do
              put st {stack = newStack, stackLen = newLen}
          )
      return Nothing
    Label l ->
      return Nothing

emulateAllInstrs :: Vector.Vector Instr -> MachineState -> IO ()
emulateAllInstrs instrs state = do
  case instrs Vector.!? ip state of
    Just instr -> do
      let (a, s) = runState (emulate instr) state
      let newState = s {ip = ip s + 1}
      -- putStr $ show instr
      -- putStr $ ":"
      -- putStrLn $ show state
      case a of
        Just x -> do
          -- putStr $ show x
          case x of
            Err e -> do
              print $ "Error!!!" ++ show x
              return ()
            _ -> do
              -- putStrLn "================="
              putStr $ show x
              -- putStrLn "================="
              emulateAllInstrs instrs newState
        Nothing -> do
          emulateAllInstrs instrs newState
    Nothing -> do
      return ()

main :: IO ()
main = do
  -- file <- readFile "../build/out.bytecode"
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Please Provide Arguments : <this-binary-name> <input-file>"
    x : _ -> do
      file <- readFile x
      let allLines = lines file
      let instrs = Vector.fromList $ Prelude.map (\x -> read x :: Instr) allLines
      let allLabels = Vector.toList $ Vector.map extractLabelName $ Vector.filter isLabel (Vector.indexed instrs)
      let state = MachineState {fp = [0], ip = 0, stack = [], stackLen = 0, retAddr = [], allLabels = allLabels, argsCnt = []}
      emulateAllInstrs instrs state
