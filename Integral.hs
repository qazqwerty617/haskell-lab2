{-# LANGUAGE BangPatterns #-}
-- | Лабораторна робота №2, варіант 4.
-- Паралельне обчислення визначеного інтеграла J = ∫[A,B] f(x) dx
-- із заданою точністю ε.
--
-- Компіляція:  ghc -O2 -threaded -rtsopts Integral.hs
-- Запуск:      ./Integral +RTS -N
module Main (main) where

import Control.Concurrent      (forkOn, getNumCapabilities, setNumCapabilities)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception       (evaluate)
import Control.Monad           (forM, forM_)
import GHC.Clock               (getMonotonicTime)
import GHC.Conc                (getNumProcessors)
import System.IO
import Text.Printf             (printf)
import Text.Read               (readMaybe)

------------------------------------------------------------------------
-- Підінтегральні функції

data Func = Func
  { fName      :: String
  , fun        :: Double -> Double
  , antideriv  :: Maybe (Double -> Double)   -- первісна, якщо її можна записати
  }

functions :: [Func]
functions =
  [ Func "sin(x)"                     sin                              (Just (negate . cos))
  , Func "x^2 * e^(-x)"               (\x -> x * x * exp (-x))         (Just (\x -> -(x * x + 2 * x + 2) * exp (-x)))
  , Func "1 / (1 + x^2)"              (\x -> 1 / (1 + x * x))          (Just atan)
  , Func "sqrt(x) * cos(x)^2 * ln(1+x)" (\x -> sqrt x * cos x ^ (2 :: Int) * log (1 + x)) Nothing
  ]

------------------------------------------------------------------------
-- Чисельне інтегрування

-- | Формула Сімпсона на відрізку [a, b], m - парна кількість кроків.
-- Цикл з накопичувачем і BangPatterns, щоб не плодити ліниві обчислення
simpson :: (Double -> Double) -> Double -> Double -> Int -> Double
simpson f a b m = h / 3 * (f a + f b + go 1 0)
  where
    h = (b - a) / fromIntegral m
    go !i !acc
      | i >= m    = acc
      | otherwise = go (i + 1) (acc + w * f (a + fromIntegral i * h))
      where w = if odd i then 4 else 2

-- | Паралельний Сімпсон: [a, b] ділимо на k однакових частин, кожну рахує
-- свій потік, потім частинні суми додаються. n - загальна кількість кроків,
-- кратна 2k, тому на кожен потік припадає парне число кроків
parallelSimpson :: Int -> (Double -> Double) -> Double -> Double -> Int -> IO Double
parallelSimpson k f a b n = do
  let m   = n `div` k                       -- кроків на один потік
      len = (b - a) / fromIntegral k        -- довжина частини відрізка
  boxes <- forM [0 .. k - 1] $ \j -> do
    box <- newEmptyMVar
    let lo = a + fromIntegral j * len
        hi = if j == k - 1 then b else lo + len
    -- forkOn закріплює потік за ядром j, evaluate змушує рахувати саме в цьому потоці
    _ <- forkOn j (evaluate (simpson f lo hi m) >>= putMVar box)
    pure box
  sum <$> mapM takeMVar boxes

-- | Подвоюємо кількість кроків, доки оцінка похибки за правилом Рунге
-- |I(2n) - I(n)| / 15 не стане меншою за ε. Повертає (інтеграл, n, оцінка похибки)
integrate :: Int -> (Double -> Double) -> Double -> Double -> Double -> IO (Double, Int, Double)
integrate k f a b eps = do
  let n0 = 2 * k * 8
  i0 <- parallelSimpson k f a b n0
  loop n0 i0
  where
    maxN = 2 ^ (28 :: Int)
    loop n iN = do
      let n2 = 2 * n
      i2 <- parallelSimpson k f a b n2
      let err = abs (i2 - iN) / 15
      if err < eps || n2 >= maxN
        then pure (i2, n2, err)
        else loop n2 i2

------------------------------------------------------------------------
-- Введення з клавіатури з перевіркою

ask :: Read a => String -> (a -> Bool) -> String -> IO a
ask question ok hint = do
  putStr (question ++ ": ")
  hFlush stdout
  s <- getLine
  case readMaybe s of
    Just v | ok v -> pure v
    _             -> putStrLn ("  " ++ hint) >> ask question ok hint

------------------------------------------------------------------------
-- Вимірювання часу

timeIt :: IO a -> IO (a, Double)
timeIt act = do
  t0 <- getMonotonicTime
  r  <- act
  t1 <- getMonotonicTime
  pure (r, t1 - t0)

-- | Найкращий час з кількох запусків, щоб прибрати випадкові затримки ОС
bestOf :: Int -> IO a -> IO Double
bestOf runs act = minimum <$> forM [1 .. runs] (const (snd <$> timeIt act))

benchmark :: Func -> Double -> Double -> Int -> IO ()
benchmark f a b maxThreads = do
  let n       = 2 ^ (25 :: Int) * 3     -- ~100 млн кроків, ділиться на 2k для k = 1, 2, 3, 4, 6, 8
      threads = takeWhile (<= maxThreads) [1, 2, 3, 4, 6, 8]
  printf "\nЗаміри швидкодії: фіксоване n = %d кроків, найкращий час з 3 запусків\n" n
  results <- forM threads $ \k -> do
    setNumCapabilities k
    v <- parallelSimpson k (fun f) a b n          -- "розігрів"
    t <- bestOf 3 (parallelSimpson k (fun f) a b n)
    pure (k, t, v)
  let t1 = head [t | (_, t, _) <- results]
  putStrLn "  Потоків | Час, с   | Прискорення | Ефективність | Значення інтеграла"
  putStrLn "  --------+----------+-------------+--------------+--------------------"
  forM_ results $ \(k, t, v) -> do
    let s = t1 / t
    printf "  %7d | %8.4f | %11.2f | %11.0f%% | %.15f\n" k t s (100 * s / fromIntegral k) v

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  hSetBuffering stdout NoBuffering
  cores <- getNumProcessors
  caps  <- getNumCapabilities
  printf "Логічних процесорів у системі: %d, запущено з -N%d\n\n" cores caps

  putStrLn "Оберіть підінтегральну функцію:"
  forM_ (zip [1 :: Int ..] functions) $ \(i, f) -> printf "  %d. f(x) = %s\n" i (fName f)
  choice <- ask "Номер функції" (`elem` [1 .. length functions]) "Введіть номер зі списку."
  let f = functions !! (choice - 1)

  a   <- ask "A (нижня межа)" (const True :: Double -> Bool) "Потрібне дійсне число, наприклад 0 або 1.5."
  b   <- ask "B (верхня межа)" (> a) "B має бути дійсним числом, більшим за A."
  eps <- ask "Точність ε" (\e -> e > 0 && e < 1 :: Bool) "Точність має бути в межах 0 < ε < 1, наприклад 1e-10."
  k   <- ask "Кількість потоків k" (\x -> x >= 1 && x <= 64) "Потрібне ціле число від 1 до 64."

  setNumCapabilities k
  ((val, n, err), t) <- timeIt (integrate k (fun f) a b eps)
  printf "\nJ = ∫[%g; %g] %s dx\n" a b (fName f)
  printf "  Результат:          %.15f\n" val
  printf "  Кроків сітки n:     %d\n" n
  printf "  Оцінка похибки:     %.3e (за правилом Рунге)\n" err
  case antideriv f of
    Just g  -> printf "  Точне значення:     %.15f\n  Фактична похибка:   %.3e\n"
                      (g b - g a) (abs (val - (g b - g a)))
    Nothing -> putStrLn "  Точне значення:     первісна не виражається через елементарні функції"
  printf "  Час обчислення:     %.4f с на %d потоках\n" t k

  benchmark f a b (min 8 cores)
