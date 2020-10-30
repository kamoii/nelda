{-# LANGUAGE TypeFamilies #-}
module Data.CoalesceMaybe where

-- | CoalesceMaybe nested nullable column into a single level of nesting.
type family CoalesceMaybe a where
    CoalesceMaybe (Maybe (Maybe a)) = CoalesceMaybe (Maybe a)
    CoalesceMaybe a                 = a
