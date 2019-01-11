-- | An undo-redo history that keeps a copy of every state.

module TaskMachine.History
  ( History
  , history
  , step
  , current
  , modify
  , undo
  , maybeUndo
  , redo
  , maybeRedo
  ) where

data Step a b = Step a b

-- | Represents the history (only one branch) of some type @a@.
--
-- Contains backwards (ctrl+z) as well as forwards (ctrl+y or ctrl+shift+z) history,
-- as well as the current state.
data History a b = History [Step a b] a [Step a b]

-- | Create a new 'History' from a single state.
history :: a -> History a b
history a = History [] a []

-- | Add a new step to the history.
--
-- Any forwards history will be overwritten, as this action starts a new
-- branch in the history tree.
step :: a -> b -> History a b -> History a b
step a (History xs y _) = History (Step y b : xs)  a []

-- | Read the current state of the history.
current :: History a -> b -> a
current (History _ a _) = a

{-
-- | Modify the current state, adding a step in the process.
--
-- @'modify' f h = 'step' (f $ 'current' h) h@
modify :: (a -> a) -> History a -> History a -- not a functor!
modify f h = step (f $ current h) h
-}

-- | Jump to the previous state, remembering the future for later redos.
--
-- If there is no previous state, do nothing.
undo :: History a b -> Maybe (History a b, b)
undo (History (Step x b : xs) y zs) = Just (History xs x (Step y b : zs), b)
undo _                              = Nothing

{-
-- | Like 'undo', but returns 'Nothing' if there was no previous state.
maybeUndo :: History a -> Maybe (History a)
maybeUndo (History (x:xs) y zs) = Just $ History xs x (y:zs)
maybeUndo _                     = Nothing
-}

-- | Jump to the next state, remembering the past for later undos.
--
-- If there is no next state, do nothing.
redo :: History a b -> Maybe (History a b, b)
redo (History xs y (Step z b : zs)) = Just (History (Step y b : xs) z zs, b)
redo _                              = Nothing

{-
-- | Like 'redo', but returns 'Nothing' if there was no next state.
maybeRedo :: History a -> Maybe (History a)
maybeRedo (History xs y (z:zs)) = Just $ History (y:xs) z zs
maybeRedo _                     = Nothing
-}
