-- All Rights Reserved.
--
--    Licensed under the Apache License, Version 2.0 (the "License");
--    you may not use this file except in compliance with the License.
--    You may obtain a copy of the License at
--
--        http://www.apache.org/licenses/LICENSE-2.0
--
--    Unless required by applicable law or agreed to in writing, software
--    distributed under the License is distributed on an "AS IS" BASIS,
--    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--    See the License for the specific language governing permissions and
--    limitations under the License.

-- | The sole datatype that the core deals with.
module Test.DarkMatter.Data.ProcLib
       ( specs
       ) where

import Debug.Trace
import Data.Maybe
import Test.Hspec
import Test.QuickCheck hiding (sample)
import DarkMatter.Data.Proc
import DarkMatter.Data.ProcLib

newtype Op a = Op (a -> a -> a)

mean_spec :: Spec
mean_spec = do
    it "numerical stability"
      (forAll samples0 $ not . isInfinite . fst . run_ mean)

    it "against reference implementation"
      (forAll samples1 (\xs -> let m1 = fst (run_ mean xs)
                                   m0 = refImpl xs
                               in abs (m1-m0) < 0.01))
  where samples0 = listOf1 (elements [1.7976931348623158e+308])

        samples1 = listOf1 (choose (0, 1) :: Gen Double)

        refImpl xs = sum xs / fromIntegral (length xs)

count_spec :: Spec
count_spec = 
    it "counting elements"
      (forAll samples (\xs -> let l0 = length xs
                                  l1 = fst (run_ count xs)
                              in l0 == l1))
  where samples = listOf1 (elements [()])

binary_spec :: Spec
binary_spec =
    it "against reference implementation"
      (forAll samples (\(Op f, xs) -> let v0 = foldr1 f xs
                                          v1 = fst (run_ (binary f) xs)
                                      in v0 == v1))
  where samples = do { values <- listOf1 (arbitrary :: Gen Integer)
                     ; op     <- elements [Op max, Op min, Op (+), Op (*)]
                     ; return (op, values)
                     }

select_spec :: Spec
select_spec =
    it "against reference implementation"
      (forAll samples (\xs -> let f  = (> 0)
                                  v0 = reverse $ filter f xs
                                  v1 = catMaybes (fst $ run (select f) xs)
                              in v0 == v1))
  where samples = arbitrary :: Gen [Int]

window_spec :: Spec
window_spec =
    it "honor the buffer size"
      (forAll samples ((all (==10)) . catMaybes . fst . run (window 10 count)))
  where samples = arbitrary :: Gen [()]

takeProc_spec :: Spec
takeProc_spec =
    it "against reference implementation"
      (forAll samples (\n -> let v0 = fst $ runWhile isJust (takeProc n) [1..]
                             in length v0 == n))
  where samples = elements [0..100]

dropProc_spec :: Spec
dropProc_spec =
    it "against reference implementation"
      (forAll samples (\n -> let v0 = fst $ runWhile isNothing (dropProc n) [1..]
                             in length v0 == n))
  where samples = elements [0..100]

sample_spec :: Spec
sample_spec =
    it "honor the documented frequency"
      (forAll samples (\(n, m) -> let vs = catMaybes $ fst $ run (sample n m) [1..100]
                                      ex = n * (100 `div` m) + min n (100 `mod` m)
                                  in length vs == ex))
  where samples = arbitrary `suchThat` (\(n, m) -> n <= m && n > 0)

sma_spec :: Spec
sma_spec = do
    it "numerical stability"
      (forAll samples0 (\(n, xs) -> let v0 = catMaybes $ fst $ run (sma n) xs
                                    in all (not . isInfinite) v0))

    it "reference implemenetation"
      (forAll samples1 (\(n, xs) -> let v0 = catMaybes $ reverse $ fst $ run (sma n) xs
                                        v1 = refImpl n xs
                                        vs = zipWith (-) v0 v1
                                    in all ((< 0.01) . abs) vs))
  where samples0 = do { vals <- listOf1 (elements [1.7976931348623158e+308])
                      ; size <- elements [1..100]
                      ; return (size, vals)
                      }

        samples1 = do { vals <- listOf1 (choose (0, 1) :: Gen Double)
                      ; size <- elements [1..100]
                      ; return (size, vals)
                      }
        
        refImpl n xs
          | length xs >= n = sum (take n xs) / (fromIntegral n) : refImpl n (tail xs)
          | otherwise      = []


specs :: Spec
specs = describe "DarkMatter.Data.ProcLib" $ do
    describe "mean" mean_spec
    describe "sma" sma_spec
    describe "count" count_spec
    describe "binary" binary_spec
    describe "select" select_spec
    describe "window" window_spec
    describe "takeProc" takeProc_spec
    describe "dropProc" dropProc_spec
    describe "sample" sample_spec

instance Show (Op a) where
  show _ = "<<function>>"