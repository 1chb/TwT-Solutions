Exercise 1.2-i

> Either Bool (Bool, Maybe Bool)  -> Bool
  (       2 + (  2 * (1 + 2)   )) ->  2
          8 -> 2; 2^8 = 256

Exercise 1.4-i

The Prelude functions curry and uncurry have these types. Using these
and prove the algebraic equality using the Curry - Howard Isomorphism,
prove that (uncurry . curry) == id and (curry . uncurry) == id, see
page 10, e.g (to . from) == id:

Here are the definitions of all involved functions, rewritten in
(partly) the lambda equivalents:

> curry   = λf x y  ➝  f (x, y)
> uncurry = λg p    ➝  g (fst p) (snd p)
> fst     = λ(a, b) ➝ a
> snd     = λ(a, b) ➝ b
> f . g   = λh      ➝ f (g h)
> id      = λx      ➝ x

Show that (curry . uncurry) == id, starting with (curry . uncurry) and
try to find a way to id. Denote each step with a comment to show why
it is eligible.

    curry . uncurry                                       -- def of (.) where f=curry and g=uncurry 
⟹ λh ➝ curry(uncurry h)                                  -- def of curry
⟹ λh ➝ (λf x y ➝ f (x, y)) (uncurry h)                   -- application f = (uncurry h)
⟹ λh ➝ (λx y ➝ (uncurry h) (x, y))                       -- def of uncurry
⟹ λh ➝ (λx y ➝ ((λg p ➝ g (fst p) (snd p)) h) (x, y))    -- application g = h
⟹ λh ➝ (λx y ➝ (λp ➝ h (fst p) (snd p)) (x, y))          -- application p = (x, y)
⟹ λh ➝ (λx y ➝ (h (fst (x, y)) (snd (x, y))))            -- def of fst
⟹ λh ➝ (λx y ➝ (h ((λ(a, b) ➝ a) (x, y)) (snd (x, y))))  -- application (a, b) = (x, y)
⟹ λh ➝ (λx y ➝ (h x (snd (x, y))))                       -- def of snd
⟹ λh ➝ (λx y ➝ (h x ((λ(a, b) ➝ b) (x, y))))             -- application (a, b) = (x, y)
⟹ λh ➝ (λx y ➝ (h x y))                                  -- syntactic sugar: λx ➝ λy ➝ ... == λx y ➝ ...
⟹ λh x y ➝ h x y                                         -- η-reduction
⟹ λh x ➝ h x                                             -- η-reduction
⟹ λh ➝ h                                                 -- undef of id
⟹ id                                                     -- QED.

    uncurry . curry
⟹ λh ➝ uncurry (curry h)
⟹ λh ➝ (λg p ➝ g (fst p) (snd p)) (curry h)
⟹ λh ➝ (λp ➝ (curry h) (fst p) (snd p))
⟹ λh ➝ (λp ➝ ((λf x y ➝ f (x, y)) h) (fst p) (snd p))
⟹ λh ➝ (λp ➝ (λx y ➝ h (x, y)) (fst p) (snd p))
⟹ λh ➝ (λp ➝ h (fst p, snd p))
⟹ λh ➝ (λp ➝ h p)
⟹ λh ➝ h
⟹ id


Exercise 1.4-11

Prove that a^b*a^c = a^(b+c) (using CHI):

> |(b -> a, c -> a)|  -- a^b*a^c
> |Either b c -> a|   -- a^(b+c)

> to :: (b -> a, c -> a) -> (Either b c -> a)
> to (f, g) x = case x of {Left b -> f b; Right c -> g c}

> from :: (Either b c -> a) -> (b -> a, c -> a)
> from e = (\b -> e $ Left b, \c -> e $ Right c)

> f . g = λh ➝ f(g h)
> to    = λ(f, g) x ➝ case x of {Left b ➝ f b; Right c ➝ g c}
> from  = \e ➝ (\b ➝ e $ Left b, \c ➝ e $ Right c)

    to . from
⟹ λh ➝ to (from h)
⟹ λh ➝ to ((\e ➝ (\b ➝ e $ Left b, \c ➝ e $ Right c)) h)
⟹ λh ➝ to (\b ➝ h $ Left b, \c ➝ h $ Right c)
⟹ λh ➝ (λ(f, g) x ➝ case x of {Left b ➝ f b; Right c ➝ g c}) (\b ➝ h $ Left b, \c ➝ h $ Right c)
⟹ λh x ➝ case x of {Left b ➝ (\b ➝ h $ Left b) b; Right c ➝ (\c ➝ h $ Right c) c}
⟹ λh x ➝ case x of {Left b ➝ h x; Right c ➝ h x}
⟹ λh x ➝ h x
⟹ λh ➝ h
⟹ id

    from . to
⟹ λ(f, g) ➝ from (to (f, g))
⟹ λ(f, g) ➝ (\e ➝ (\b ➝ e $ Left b, \c ➝ e $ Right c)) (to (f, g))
⟹ λ(f, g) ➝ (\b ➝ to (f, g) $ Left b, \c ➝ to (f, g) $ Right c)
⟹ λ(f, g) ➝  (\b ➝ (λ(f, g) x ➝ case x of {Left b ➝ f b; Right c ➝ g c}) (f, g) $ Left b
             , \c ➝ (λ(f, g) x ➝ case x of {Left b ➝ f b; Right c ➝ g c}) (f, g) $ Right c)
⟹ λ(f, g) ➝  (\b ➝ (λx ➝ case x of {Left b ➝ f b; Right c ➝ g c}) $ Left b
             , \c ➝ (λx ➝ case x of {Left b ➝ f b; Right c ➝ g c}) $ Right c)
⟹ λ(f, g) ➝  (\b ➝ (case Left b of {Left b ➝ f b; Right c ➝ g c})
             , \c ➝ (case Right c of {Left b ➝ f b; Right c ➝ g c}))
⟹ λ(f, g) ➝  (\b ➝ f b, \c ➝ g c)
⟹ λ(f, g) ➝  (f, g)
⟹ id


Exercise 1.4-iii
Prove (a*b)^c = a^c*b^c

> |c -> (a, b)|      -- (a*b)^c
> |(c -> a, c -> b)| -- a^c*b^c

> to :: (c -> (a, b)) -> (c -> a, c -> b)
> to f = (fst . f, snd . f)

> from :: (c -> a, c -> b) -> (c -> (a, b))
> from (f, g) = \c -> (f c, g c)

> to = λf ➝ (fst . f, snd . f)
> from = λ(f, g) c ➝ (f c, g c)
> f . g = λh ➝ f(g h)
> fst     = λ(a, b) ➝ a
> snd     = λ(a, b) ➝ b

    to . from
⟹ λ(f, g) ➝ to (from (f, g))
⟹ λ(f, g) ➝ (λf ➝ (fst . f, snd . f)) (from (f, g))
⟹ λ(f, g) ➝ (fst . from (f, g), snd . from (f, g))
⟹ λ(f, g) ➝ (fst . (λ(f, g) c ➝ (f c, g c)) (f, g), snd . (λ(f, g) c ➝ (f c, g c)) (f, g))
⟹ λ(f, g) ➝ (fst . (λc ➝ (f c, g c)), snd . (λc ➝ (f c, g c)))
⟹ λ(f, g) ➝ (λh ➝ fst ((λc ➝ (f c, g c)) h), λh ➝ snd ((λc ➝ (f c, g c)) h))
⟹ λ(f, g) ➝ (λh ➝ fst (f h, g h), λh ➝ snd (f h, g h))
⟹ λ(f, g) ➝ (λh ➝ (λ(a, b) ➝ a) (f h, g h), λh ➝ (λ(a, b) ➝ a) (f h, g h))
⟹ λ(f, g) ➝ (λh ➝ f h, λh ➝ g h)
⟹ λ(f, g) ➝ (f, g)
⟹ id

    from . to
⟹ λh ➝ from (to h)
⟹ λh ➝ from ((λf ➝ (fst . f, snd . f)) h)
⟹ λh ➝ from (fst . h, snd . h)
⟹ λh ➝ (λ(f, g) c ➝ (f c, g c)) (fst . h, snd . h)
⟹ λh ➝ (λc ➝ ((fst . h) c, (snd . h) c))
⟹ λh ➝ (λc ➝ (fst (h c), snd (h c)))  -- lemma: (fst x, snd x) == x, where x :: (a, b)
⟹ λh ➝ (λc ➝ h c)
⟹ λh ➝ h
⟹ id
