{-# LANGUAGE BangPatterns #-}
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

module DarkMatter.Data.Timeline
       ( Timeline()
       , empty
       , publish
       , publishMany
       , tKeys
       , tLookup
       , ttl
       , gc
       ) where

import           Data.List (foldl')
import qualified Data.Map as M
import           Data.Hashable
import           DarkMatter.Data.Time
import qualified DarkMatter.Data.Metric as M
import           DarkMatter.Data.Event

-- | The wall is capable of accepting all kinds of events. I think
-- this makes the use of it easier and gives more flexibility.
newtype Timeline k = Timeline { history :: M.Map k Event }

-- | Controls the global sync rate. The minimum rate at wich we
-- produce events
clock :: Time
clock = mktime 60 0

-- | The expiration time of events in timeline. If no event arrives
-- before this frequency, it is marked as invalid and not used.
ttl :: Time
ttl = clock `mul` 60

empty :: Timeline k
empty = Timeline M.empty

elapsed :: Event -> Event -> Maybe Time
elapsed e0 e1
  | older     = Nothing
  | otherwise = Just (diff (time e1) (time e0))
    where older = time e0 >= time e1

tKeys :: (Hashable k, Ord k) => Timeline k -> [k]
tKeys w = M.keys (history w)

tLookup :: (Hashable k, Ord k) => Timeline k -> k -> Maybe Event
tLookup w = flip M.lookup (history w)

tUpdate :: (Hashable k, Ord k) => Timeline k -> (Event -> Maybe Event) -> k -> Timeline k
tUpdate w f k = w { history = M.update f k (history w) }

tInsert :: (Hashable k, Ord k) => Timeline k -> k -> Event -> Timeline k
tInsert w k e = w { history = M.insert k e (history w) }

timeline :: (Hashable k, Ord k) => Timeline k -> (Event -> Event -> Event) -> k -> Event -> (Maybe (Either Event Event), Timeline k)
timeline w f k e1 = case (tLookup w k)
                    of Nothing
                         -> (Nothing, tInsert w k e1)
                       Just e0
                         -> replace (elapsed e0 e1) e0
  where replace Nothing _ = (Nothing, w)
        replace (Just t) e0
          | t >= ttl   = (Nothing, tInsert w k e1)
          | t >= clock = (Just (Right e0), tInsert w k e1)
          | otherwise  = let e = e0 `f` e1
                         in (Just (Left e), tInsert w k e)

gc :: (Hashable k, Ord k) => Time -> Timeline k -> Timeline k
gc now w0 = foldl' adjust w0 (tKeys w0)
  where adjust w k = tUpdate w maybeExpire k

        maybeExpire e
          | t >= ttl   = Nothing
          | otherwise  = Just e
            where t = diff now (time e)


combineSum :: Event -> Event -> Event
combineSum e0 e1 = event (time e0) (val e0 + val e1)

combineFst :: Event -> Event -> Event
combineFst = const

combineSnd :: Event -> Event -> Event
combineSnd = flip const

publish :: (Hashable k, Ord k) => Timeline k -> M.Metric k -> (Timeline k, Maybe (k, Event))
publish w m
  | M.isGauge m    = strict $ gauge w (M.key m) e
  | M.isDerive m   = strict $ derive w (M.key m) e
  | M.isCounter m  = strict $ counter w (M.key m) e
  | M.isAbsolute m = strict $ absolute w (M.key m) e
  | otherwise      = error "the impossible happened!"
    where e = event (M.time m) (M.val m)

          strict (a, b) = a `seq` (a, b)

publishMany :: (Hashable k, Ord k) => Timeline k -> [M.Metric k] -> (Timeline k, [(k, Event)])
publishMany w0 = go [] w0
  where go acc w []     = let r = reverse acc
                          in (w, r)
        go acc w (m:ms) = let (w1, me) = publish w m
                          in case me
                             of Nothing -> go acc w1 ms
                                Just e  -> go (e:acc) w1 ms

-- | We simply store the most recent value in the timeline. In the
-- future we want to use this to retrieve an instant snapshot of the
-- whole thing. Regardless, the new event is always returned as-is.
gauge :: (Hashable k, Ord k) => Timeline k -> k -> Event -> (Timeline k, Maybe (k, Event))
gauge w k e1 = let (_, w1) = timeline w combineSnd k e1
               in (w1, Just (k, e1))

-- | Compute the `(e1 - e0) / time_delta` of two events. Older events
-- are ignored or events that are arriving faster than the global
-- clock are simply discarded.
derive :: (Hashable k, Ord k) => Timeline k -> k -> Event -> (Timeline k, Maybe (k, Event))
derive w k e1 = case (timeline w combineFst k e1)
                of (Nothing, w1)
                     -> (w1, Nothing)
                   (Just (Left _), w1)
                     -> (w1, Nothing)
                   (Just (Right e0), w1)
                     -> (w1, Just (k, f e0))
  where f e0 = let e = toDouble (diff (time e1) (time e0))
                   t = time e1
                   v = (val e1 - val e0) / e
               in event t v

-- | Same as derive but act differently when the counter wrap (e0 >
-- e1).
counter :: (Hashable k, Ord k) => Timeline k -> k -> Event -> (Timeline k, Maybe (k, Event))
counter w k e1 = case (timeline w combineFst k e1)
                 of (Nothing, w1)
                      -> (w1, Nothing)
                    (Just (Left _), w1)
                      -> (w1, Nothing)
                    (Just (Right e0), w1)
                      -> (w1, Just (k, f e0))
  where f e0 = let l = if (val e0 < 2**32) then 2**32 else 2**64
                   e = toDouble (diff (time e1) (time e0))
                   t = time e1
                   v = if (val e1 < val e0)
                       then (l - (val e0) + (val e1)) / e
                       else (val e1 - val e0) / e
               in event t v

absolute :: (Hashable k, Ord k) => Timeline k -> k -> Event -> (Timeline k, Maybe (k, Event))
absolute w k e1 = case (timeline w combineSum k e1)
                  of (Nothing, w1)
                       -> (w1, Nothing)
                     (Just (Left _), w1)
                       -> (w1, Nothing)
                     (Just (Right e0), w1)
                       -> (w1, Just (k, f e0))
  where f e0 = let t = time e0 `add` clock
                   v = (val e0) / (toDouble clock)
               in event t v
