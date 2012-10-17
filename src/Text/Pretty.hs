
{-# LANGUAGE BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Prettify
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Hans Hoglund <hans@hanshoglund.se>
-- Stability   :  stable
-- Portability :  portable
--
-- This library was based on /The Design of a Pretty-printing Library/ by Jeuring and 
-- Meijer, heavily modified by Simon Peyton Jones (December 1996) and largely 
-- rewritten by Hans Hoglund (October 2012).
--
-----------------------------------------------------------------------------
module Text.Pretty (

        -- * The Pretty typeclass
        Pretty(..),

        -- * The Printer type
        Printer,

        -- * Construction

        -- ** Primitive types
        char, string, sizedText, zeroWidthText,
        int, integer, float, double, rational,

        -- ** Combinators
        empty,
        (<->), (<+>), hcat, hsep,
        (</>), (<//>), vcat,
        sep, cat,
        fsep, fcat,

        -- ** Wrapping and punctuation
        wrap, parens, brackets, braces, quotes, doubleQuotes,
        nest,
        hang,
        sepBy, initBy, termBy,
        sepByS, initByS, termByS,

        -- * Predicates on printers
        isEmpty,

        -- * Rendering printers
        runPrinter,
        Mode(..),
        Style(..),
        style,
        runPrinterStyle
    ) where

import Data.Semigroup
import Data.Ratio ( Ratio, numerator, denominator )
import Data.String ( IsString(fromString) )

-- ---------------------------------------------------------------------------
-- The Printer calculus

-- The Printer combinators satisfy the following laws:

{-
Laws for </>
~~~~~~~~~~~
<a1>    (x </> y) </> z   = x </> (y </> z)
<a2>    empty </> x      = x
<a3>    x </> empty      = x

        ...ditto <//>...

Laws for <>
~~~~~~~~~~~
<b1>    (x <> y) <> z   = x <> (y <> z)
<b2>    empty <> x      = empty
<b3>    x <> empty      = x

        ...ditto <+>...

Laws for string
~~~~~~~~~~~~~
<t1>    string s <> string t        = string (s++t)
<t2>    string "" <> x            = x, if x non-empty

** because of law n6, t2 only holds if x doesn't
** start with `nest'.


Laws for nest
~~~~~~~~~~~~~
<n1>    nest 0 x                = x
<n2>    nest k (nest k' x)      = nest (k+k') x
<n3>    nest k (x <> y)         = nest k x <> nest k y
<n4>    nest k (x </> y)         = nest k x </> nest k y
<n5>    nest k empty            = empty
<n6>    x <> nest k y           = x <> y, if x non-empty

** Note the side condition on <n6>!  It is this that
** makes it OK for empty to be a left unit for <>.

Miscellaneous
~~~~~~~~~~~~~
<m1>    (string s <> x) </> y = string s <> ((string "" <> x) </>
                                         nest (-length s) y)

<m2>    (x </> y) <> z = x </> (y <> z)
        if y non-empty


Laws for list versions
~~~~~~~~~~~~~~~~~~~~~~
<l1>    sep (ps++[empty]++qs)   = sep (ps ++ qs)
        ...ditto hsep, hcat, vcat, fill...

<l2>    nest k (sep ps) = sep (map (nest k) ps)
        ...ditto hsep, hcat, vcat, fill...

Laws for oneLiner
~~~~~~~~~~~~~~~~~
<o1>    oneLiner (nest k p) = nest k (oneLiner p)
<o2>    oneLiner (x <> y)   = oneLiner x <> oneLiner y

You might think that the following verion of <m1> would
be neater:

<3 NO>  (string s <> x) </> y = string s <> ((empty <> x)) </>
                                         nest (-length s) y)

But it doesn't work, for if x=empty, we would have

        string s </> y = string s <> (empty </> nest (-length s) y)
                    = string s <> nest (-length s) y
-}

-- ---------------------------------------------------------------------------
-- Operator fixity

infixl 6 <->
infixl 6 <+>
infixl 5 </>, <//>


-- ---------------------------------------------------------------------------
-- Internal
list :: b -> (a -> [a] -> b) -> [a] -> b
list z f [] = z
list z f (x:xs) = x `f` xs

-- ---------------------------------------------------------------------------
-- Pretty

-- |
-- Class of types that can be pretty-printed.                                                    
--
-- The Pretty class is similar to 'Show', but converts values to 'Printer's instead 
-- of 'Strings'. A printer is essentially a string with some extra structural information
-- such as length and identation.
--
-- Note that the instances for primitive types, lists and tuples all satisfy
--
-- > (show . pretty) x == show x
--
class Pretty a where

    -- | Return a printer for the given value.
    pretty :: a -> Printer

    -- | The method prettyList is provided to allow the programmer to give a
    --   specialised way of printing lists of values. For example, this is used by
    --   the predefined Pretty instance of the Char type, where values of type String
    --   should be shown in double quotes, rather than between square brackets.    
    prettyList :: [a] -> Printer
    prettyList = brackets . sepBy (char ',') . map pretty

int      :: Int      -> Printer
integer  :: Integer  -> Printer
float    :: Float    -> Printer
double   :: Double   -> Printer
rational :: Rational -> Printer
char'    :: Char     -> Printer
string'  :: String   -> Printer
int      = string . show
integer  = string . show
float    = string . show
double   = string . show
rational = string . show
char'    = string . show
string'  = string . show

instance Pretty Printer where
    pretty = id

instance Pretty () where
    pretty = string . show

instance Pretty Int where
    pretty = int

instance Pretty Float where
    pretty = float

instance Pretty Double where
    pretty = double

instance Pretty Char where
    pretty     = char'
    prettyList = string'

instance Pretty Integer where
    pretty = integer

instance (Pretty a, Pretty b) => Pretty (a,b) where
    pretty (x, y) = parens $ pretty x `g` pretty y
        where x `g` y = x <> char ',' <> y

instance Pretty a => Pretty [a] where
    pretty x = prettyList x

instance Pretty a => Pretty (Maybe a) where
    pretty = maybe empty pretty

instance (Pretty a, Integral a) => Pretty (Ratio a) where
    pretty x = pretty (numerator x) <> string " % " <> pretty (denominator x)  
    
-- | The abstract type of printers.
data Printer
  = Empty                            -- empty
  | NilAbove Printer                     -- string "" </> x
  | TextBeside TextDetails Int Printer   -- string s <> x
  | Nest Int Printer                     -- nest k x

  | Union Printer Printer                    -- ul `union` ur
  | NoPrinter                            -- The empty set of printers
  | Beside Printer Bool Printer              -- True <=> space between
  | Above Printer Bool Printer               -- True <=> never overlap

{-
  A Printer represents a *set* of layouts. A Printer with
  no occurrences of Union or NoPrinter represents just one layout.
  
  Here are the invariants:

  1) The argument of NilAbove is never Empty. Therefore
     a NilAbove occupies at least two lines.

  2) The argument of @TextBeside@ is never @Nest@.

  3) The layouts of the two arguments of @Union@ both flatten to the same
     string.

  4) The arguments of @Union@ are either @TextBeside@, or @NilAbove@.

  5) A @NoPrinter@ may only appear on the first line of the left argument of an
     union. Therefore, the right argument of an union can never be equivalent
     to the empty set (@NoPrinter@).

  6) An empty printer is always represented by @Empty@.  It can't be
     hidden inside a @Nest@, or a @Union@ of two @Empty@s.

  7) The first line of every layout in the left argument of @Union@ is
     longer than the first line of any layout in the right argument.
     (1) ensures that the left argument has a first line.  In view of
     (3), this invariant means that the right argument must have at
     least two lines.

 Notice the difference between
         * NoPrinter (no printers)
         * Empty (one empty printer; no height and no width)
         * string "" (a printer containing the empty string;
                    one line high, but has no width)
-}


-- | RPrinter is a "reduced Printer", guaranteed not to have a top-level Above or Beside.
type RPrinter a = Printer

-- | The TextDetails data type
--
-- A TextDetails represents a fragment of string that will be
-- output at some point.
data TextDetails = Chr  Char   -- ^ A single Char fragment
                 | Str  String -- ^ A whole String fragment

instance Semigroup Printer where
    (<>) = (<->)

instance Monoid Printer where
    mempty  = empty
    mappend = (<->)

instance IsString Printer where
    fromString = string

instance Show Printer where
  showsPrec _ doc cont = runPrinter' (mode style) (lineLength style)
                                    (ribbonsPerLine style)
                                    txtPrinter cont doc

-- ---------------------------------------------------------------------------
-- Values and Predicates on Printers and TextDetails

-- | A printer of height and width 1, containing a literal character.
char :: Char -> Printer
char c = stringBeside_ (Chr c) 1 Empty

-- | A printer of height 1 containing a literal string.
-- 'string' satisfies the following laws:
--
-- * @'string' s '<>' 'string' t = 'string' (s'++'t)@
--
-- * @'string' \"\" '<>' x = x@, if @x@ non-empty
--
-- The side condition on the last law is necessary because @'string' \"\"@
-- has height 1, while 'empty' has no height.
string :: String -> Printer
string s = case length s of {sl -> stringBeside_ (Str s)  sl Empty}

-- | Some string with any width. (@string s = sizedText (length s) s@)
sizedText :: Int -> String -> Printer
sizedText l s = stringBeside_ (Str s) l Empty

-- | Some string, but without any width. Use for non-printing string
-- such as a HTML or Latex tags
zeroWidthText :: String -> Printer
zeroWidthText = sizedText 0

-- | The empty printer, with no height and no width.
-- 'empty' is the identity for '<>', '<+>', '</>' and '<//>', and anywhere
-- in the argument list for 'sep', 'hcat', 'hsep', 'vcat', 'fcat' etc.
empty :: Printer
empty = Empty

-- | Returns 'True' if the printer is empty
isEmpty :: Printer -> Bool
isEmpty Empty = True
isEmpty _     = False

-- an old version inserted tabs being 8 columns apart in the output.
indent :: Int -> String
indent !n = replicate n ' '
{- TODO: GHC Optimised version
-- optimise long indentations using LitString chunks of 8 spaces
indent n r | n >=# _ILIT(8) = LStr (sLit "        ") (_ILIT(8)) `txt`
                              indent (n -# _ILIT(8)) r
           | otherwise      = Str (spaces n) `txt` r
-}

{-
Q: What is the reason for negative indentation (i.e. argument to indent
   is < 0) ?

A:
This indicates an error in the library client's code.
If we compose a <> b, and the first line of b is more indented than some
other lines of b, the law <n6> (<> eats nests) may cause the pretty
printer to produce an invalid layout:

doc       |0123345
------------------
d1        |a...|
d2        |...b|
          |c...|

d1<>d2    |ab..|
         c|....|

Consider a <> b, let `s' be the length of the last line of `a', `k' the
indentation of the first line of b, and `k0' the indentation of the
left-most line b_i of b.

The produced layout will have negative indentation if `k - k0 > s', as
the first line of b will be put on the (s+1)th column, effectively
translating b horizontally by (k-s). Now if the i^th line of b has an
indentation k0 < (k-s), it is translated out-of-page, causing
`negative indentation'.
-}


space_string, nl_string :: TextDetails
space_string = Chr ' '
nl_string    = Chr '\n'


-- | Wrap printer in the given characters.
wrap         :: Char -> Char -> Printer -> Printer
wrap s t p   = char s <> p <> char t
-- | Wrap printer in @(...)@
parens       :: Printer -> Printer
-- | Wrap printer in @[...]@
brackets     :: Printer -> Printer
-- | Wrap printer in @{...}@
braces       :: Printer -> Printer
-- | Wrap printer in @\'...\'@
quotes       :: Printer -> Printer
-- | Wrap printer in @\"...\"@
doubleQuotes :: Printer -> Printer

quotes       = wrap  '\'' '\''
doubleQuotes = wrap  '"'  '"'
parens       = wrap  '('  ')'
brackets     = wrap  '['  ']'
braces       = wrap  '{'  '}'


-- ---------------------------------------------------------------------------
-- Structural operations on Printers

-- | Perform some simplification of a built up @Printer@.
reducePrinter :: Printer -> RPrinter a
reducePrinter (Beside p g q) = beside p g (reducePrinter q)
reducePrinter (Above  p g q) = above  p g (reducePrinter q)
reducePrinter p              = p

-- | List version of '<>'.
hcat :: [Printer] -> Printer
hcat = reduceAB . foldr (beside_' False) empty

-- | List version of '<+>'.
hsep :: [Printer] -> Printer
hsep = reduceAB . foldr (beside_' True)  empty

-- | List version of '</>'.
vcat :: [Printer] -> Printer
vcat = reduceAB . foldr (above_' False) empty

-- | Nest (or indent) a printer by a given number of positions
-- (which may also be negative).  'nest' satisfies the laws:
--
-- * @'nest' 0 x = x@
--
-- * @'nest' k ('nest' k' x) = 'nest' (k+k') x@
--
-- * @'nest' k (x '<>' y) = 'nest' k z '<>' 'nest' k y@
--
-- * @'nest' k (x '</>' y) = 'nest' k x '</>' 'nest' k y@
--
-- * @'nest' k 'empty' = 'empty'@
--
-- * @x '<>' 'nest' k y = x '<>' y@, if @x@ non-empty
--
-- The side condition on the last law is needed because
-- 'empty' is a left identity for '<>'.
nest :: Int -> Printer -> Printer
nest k p = mkNest k (reducePrinter p)

-- | @hang d1 n d2 = sep [d1, nest n d2]@
hang :: Printer -> Int -> Printer -> Printer
hang d1 n d2 = sep [d1, nest n d2]

-- mkNest checks for Nest's invariant that it doesn't have an Empty inside it
mkNest :: Int -> Printer -> Printer
mkNest k _ | k `seq` False = undefined
mkNest k (Nest k1 p)       = mkNest (k + k1) p
mkNest _ NoPrinter             = NoPrinter
mkNest _ Empty             = Empty
mkNest 0 p                 = p
mkNest k p                 = nest_ k p

-- mkUnion checks for an empty printer
mkUnion :: Printer -> Printer -> Printer
mkUnion Empty _ = Empty
mkUnion p q     = p `union_` q

beside_' :: Bool -> Printer -> Printer -> Printer
beside_' _ p Empty = p
beside_' g p q     = Beside p g q

above_' :: Bool -> Printer -> Printer -> Printer
above_' _ p Empty = p
above_' g p q     = Above p g q

reduceAB :: Printer -> Printer
reduceAB (Above  Empty _ q) = q
reduceAB (Beside Empty _ q) = q
reduceAB doc                = doc

nilAbove_ :: RPrinter a -> RPrinter a
nilAbove_ p = NilAbove p

-- Arg of a TextBeside is always an RPrinter
stringBeside_ :: TextDetails -> Int -> RPrinter a -> RPrinter a
stringBeside_ s sl p = TextBeside s sl p

nest_ :: Int -> RPrinter a -> RPrinter a
nest_ k p = Nest k p

union_ :: RPrinter a -> RPrinter a -> RPrinter a
union_ p q = Union p q


-- ---------------------------------------------------------------------------
-- Vertical composition @</>@

-- | Above, except that if the last line of the first argument stops
-- at least one position before the first line of the second begins,
-- these two lines are overlapped.  For example:
--
-- >    string "hi" </> nest 5 (string "there")
--
-- lays out as
--
-- >    hi   there
--
-- rather than
--
-- >    hi
-- >         there
--
-- '</>' is associative, with identity 'empty', and also satisfies
--
-- * @(x '</>' y) '<>' z = x '</>' (y '<>' z)@, if @y@ non-empty.
--
(</>) :: Printer -> Printer -> Printer
p </>  q = above_ p False q

-- | Above, with no overlapping.
-- '<//>' is associative, with identity 'empty'.
(<//>) :: Printer -> Printer -> Printer
p <//> q = above_ p True q

above_ :: Printer -> Bool -> Printer -> Printer
above_ p _ Empty = p
above_ Empty _ q = q
above_ p g q     = Above p g q

above :: Printer -> Bool -> RPrinter a -> RPrinter a
above (Above p g1 q1)  g2 q2 = above p g1 (above q1 g2 q2)
above p@(Beside _ _ _) g  q  = aboveNest (reducePrinter p) g 0 (reducePrinter q)
above p g q                  = aboveNest p             g 0 (reducePrinter q)

aboveNest :: RPrinter a -> Bool -> Int -> RPrinter a -> RPrinter a
-- Specfication: aboveNest p g k q = p $g$ (nest k q)

aboveNest _                   _ k _ | k `seq` False = undefined
aboveNest NoPrinter               _ _ _ = NoPrinter
aboveNest (p1 `Union` p2)     g k q = aboveNest p1 g k q `union_`
                                      aboveNest p2 g k q

aboveNest Empty               _ k q = mkNest k q
aboveNest (Nest k1 p)         g k q = nest_ k1 (aboveNest p g (k - k1) q)
                                  -- p can't be Empty, so no need for mkNest

aboveNest (NilAbove p)        g k q = nilAbove_ (aboveNest p g k q)
aboveNest (TextBeside s sl p) g k q = stringBeside_ s sl rest
                                    where
                                      !k1  = k - sl
                                      rest = case p of
                                                Empty -> nilAboveNest g k1 q
                                                _     -> aboveNest  p g k1 q
aboveNest (Above {})          _ _ _ = error "aboveNest Above"
aboveNest (Beside {})         _ _ _ = error "aboveNest Beside"

nilAboveNest :: Bool -> Int -> RPrinter a -> RPrinter a
-- Specification: string s <> nilaboveNest g k q
--              = string s <> (string "" $g$ nest k q)

nilAboveNest _ k _           | k `seq` False = undefined
nilAboveNest _ _ Empty       = Empty
                               -- Here's why the "string s <>" is in the spec!
nilAboveNest g k (Nest k1 q) = nilAboveNest g (k + k1) q

nilAboveNest g k q           | not g && k > 0      -- No newline if no overlap
                             = stringBeside_ (Str (indent k)) k q
                             | otherwise           -- Put them really above
                             = nilAbove_ (mkNest k q)








-- ---------------------------------------------------------------------------
-- Horizontal composition @<>@

-- We intentionally avoid Data.Monoid.(<>) here due to interactions of
-- Data.Monoid.(<>) and (<+>).  See
-- http://www.haskell.org/pipermail/libraries/2011-November/017066.html

-- | Beside.
-- '<>' is associative, with identity 'empty'.
(<->) :: Printer -> Printer -> Printer
p <->  q = beside_ p False q

-- | Beside, separated by space, unless one of the arguments is 'empty'.
-- '<+>' is associative, with identity 'empty'.
(<+>) :: Printer -> Printer -> Printer
p <+> q = beside_ p True  q

beside_ :: Printer -> Bool -> Printer -> Printer
beside_ p _ Empty = p
beside_ Empty _ q = q
beside_ p g q     = Beside p g q

beside :: Printer -> Bool -> RPrinter a -> RPrinter a
-- Specification: beside g p q = p <g> q

beside NoPrinter               _ _   = NoPrinter
beside (p1 `Union` p2)     g q   = beside p1 g q `union_` beside p2 g q
beside Empty               _ q   = q
beside (Nest k p)          g q   = nest_ k $! beside p g q
beside p@(Beside p1 g1 q1) g2 q2
         | g1 == g2              = beside p1 g1 $! beside q1 g2 q2
         | otherwise             = beside (reducePrinter p) g2 q2
beside p@(Above _ _ _)     g q   = let !d = reducePrinter p in beside d g q
beside (NilAbove p)        g q   = nilAbove_ $! beside p g q
beside (TextBeside s sl p) g q   = stringBeside_ s sl $! rest
                               where
                                  rest = case p of
                                           Empty -> nilBeside g q
                                           _     -> beside p g q

nilBeside :: Bool -> RPrinter a -> RPrinter a
-- Specification: string "" <> nilBeside g p
--              = string "" <g> p

nilBeside _ Empty         = Empty -- Hence the string "" in the spec
nilBeside g (Nest _ p)    = nilBeside g p
nilBeside g p | g         = stringBeside_ space_string 1 p
              | otherwise = p







-- ---------------------------------------------------------------------------
-- Separate, @sep@

-- Specification: sep ps  = oneLiner (hsep ps)
--                         `union`
--                          vcat ps

-- | Either 'hsep' or 'vcat'.
sep  :: [Printer] -> Printer
sep = sepX True   -- Separate with spaces

-- | Either 'hcat' or 'vcat'.
cat :: [Printer] -> Printer
cat = sepX False  -- Don't

sepX :: Bool -> [Printer] -> Printer
sepX _ []     = empty
sepX x (p:ps) = sep1 x (reducePrinter p) 0 ps


-- Specification: sep1 g k ys = sep (x : map (nest k) ys)
--                            = oneLiner (x <g> nest k (hsep ys))
--                              `union` x </> nest k (vcat ys)

sep1 :: Bool -> RPrinter a -> Int -> [Printer] -> RPrinter a
sep1 _ _                   k _  | k `seq` False = undefined
sep1 _ NoPrinter               _ _  = NoPrinter
sep1 g (p `Union` q)       k ys = sep1 g p k ys `union_`
                                  aboveNest q False k (reducePrinter (vcat ys))

sep1 g Empty               k ys = mkNest k (sepX g ys)
sep1 g (Nest n p)          k ys = nest_ n (sep1 g p (k - n) ys)

sep1 _ (NilAbove p)        k ys = nilAbove_
                                  (aboveNest p False k (reducePrinter (vcat ys)))
sep1 g (TextBeside s sl p) k ys = stringBeside_ s sl (sepNB g p (k - sl) ys)
sep1 _ (Above {})          _ _  = error "sep1 Above"
sep1 _ (Beside {})         _ _  = error "sep1 Beside"

sepNB :: Bool -> Printer -> Int -> [Printer] -> Printer
-- Specification: sepNB p k ys = sep1 (string "" <> p) k ys
-- Called when we have already found some string in the first item
-- We have to eat up nests

sepNB g (Nest _ p) k ys
  = sepNB g p k ys -- Never triggered, because of invariant (2)
sepNB g Empty k ys
  = oneLiner (nilBeside g (reducePrinter rest)) `mkUnion`
-- XXX: PRETTY: Used True here
    nilAboveNest False k (reducePrinter (vcat ys))
  where
    rest | g         = hsep ys
         | otherwise = hcat ys
sepNB g p k ys
  = sep1 g p k ys









-- ---------------------------------------------------------------------------
-- @fill@

-- | \"Paragraph fill\" version of 'cat'.
fcat :: [Printer] -> Printer
fcat = fill False

-- | \"Paragraph fill\" version of 'sep'.
fsep :: [Printer] -> Printer
fsep = fill True

-- Specification:
--
-- fill g docs = fillIndent 0 docs
--
-- fillIndent k [] = []
-- fillIndent k [p] = p
-- fillIndent k (p1:p2:ps) =
--    oneLiner p1 <g> fillIndent (k + length p1 + g ? 1 : 0)
--                               (remove_nests (oneLiner p2) : ps)
--     `Union`
--    (p1 $*$ nest (-k) (fillIndent 0 ps))
--
-- $*$ is defined for layouts (not Printers) as
-- layout1 $*$ layout2 | hasMoreThanOneLine layout1 = layout1 </> layout2
--                     | otherwise                  = layout1 <//> layout2

fill :: Bool -> [Printer] -> RPrinter a
fill _ []     = empty
fill g (p:ps) = fill1 g (reducePrinter p) 0 ps

fill1 :: Bool -> RPrinter a -> Int -> [Printer] -> Printer
fill1 _ _                   k _  | k `seq` False = undefined
fill1 _ NoPrinter               _ _  = NoPrinter
fill1 g (p `Union` q)       k ys = fill1 g p k ys `union_`
                                   aboveNest q False k (fill g ys)
fill1 g Empty               k ys = mkNest k (fill g ys)
fill1 g (Nest n p)          k ys = nest_ n (fill1 g p (k - n) ys)
fill1 g (NilAbove p)        k ys = nilAbove_ (aboveNest p False k (fill g ys))
fill1 g (TextBeside s sl p) k ys = stringBeside_ s sl (fillNB g p (k - sl) ys)
fill1 _ (Above {})          _ _  = error "fill1 Above"
fill1 _ (Beside {})         _ _  = error "fill1 Beside"

fillNB :: Bool -> Printer -> Int -> [Printer] -> Printer
fillNB _ _           k _  | k `seq` False = undefined
fillNB g (Nest _ p)  k ys   = fillNB g p k ys
                              -- Never triggered, because of invariant (2)
fillNB _ Empty _ []         = Empty
fillNB g Empty k (Empty:ys) = fillNB g Empty k ys
fillNB g Empty k (y:ys)     = fillNBE g k y ys
fillNB g p k ys             = fill1 g p k ys


fillNBE :: Bool -> Int -> Printer -> [Printer] -> Printer
fillNBE g k y ys
  = nilBeside g (fill1 g ((elideNest . oneLiner . reducePrinter) y) k' ys)
-- XXX: PRETTY: Used True here
    `mkUnion` nilAboveNest False k (fill g (y:ys))
  where k' = if g then k - 1 else k

elideNest :: Printer -> Printer
elideNest (Nest _ d) = d
elideNest d          = d



-- ---------------------------------------------------------------------------
-- Derived combinators

-- |
-- Join with separator.
--
-- > sepBy q [x1,x2..xn] = x1 <> q <> x2 <> q .. xn.
sepBy :: Printer -> [Printer] -> Printer

-- |
-- Join with initiator.
--
-- > initBy q [x1,x2..xn] = q <> x1 <> q <> x2 <> q .. xn.
initBy :: Printer -> [Printer] -> Printer

-- |
-- Join with terminator.
--
-- > termBy q [x1,x2..xn] = x1 <> q <> x2 <> q .. xn <> q.
termBy :: Printer -> [Printer] -> Printer
sepBy  p = list empty $ \x -> (x <>) . initBy p
initBy p = hcat . map (p <>)
termBy p = hcat . map (<> p)


-- |
-- Join with separator followed by space.
--
-- > sepByS q [x1,x2..xn] = x1 <> q <+> x2 <> q <+>.. xn.
sepByS :: Printer -> [Printer] -> Printer

-- |
-- Join with initiator followed by space.
--
-- > initByS q [x1,x2..xn] = q <+> x1 <> q <+> x2 <> q <+> .. xn.
initByS :: Printer -> [Printer] -> Printer

-- |
-- Join with terminator followed by space.
--
-- > termByS q [x1,x2..xn] = x1 <> q <+> x2 <> q <+> .. xn <> q.
termByS :: Printer -> [Printer] -> Printer
sepByS  p = list empty $ \x -> (x <>) . initByS p
initByS p = hcat . map (p <+>)
termByS p = hsep . map (<> p)






-- ---------------------------------------------------------------------------
-- Selecting the best layout

best :: Int     -- Line length
     -> Int     -- Ribbon length
     -> RPrinter a
     -> RPrinter a  -- No unions in here!
best w0 r p0
  = get w0 p0
  where
    get w _ | w == 0 && False = undefined
    get _ Empty               = Empty
    get _ NoPrinter               = NoPrinter
    get w (NilAbove p)        = nilAbove_ (get w p)
    get w (TextBeside s sl p) = stringBeside_ s sl (get1 w sl p)
    get w (Nest k p)          = nest_ k (get (w - k) p)
    get w (p `Union` q)       = nicest w r (get w p) (get w q)
    get _ (Above {})          = error "best get Above"
    get _ (Beside {})         = error "best get Beside"

    get1 w _ _ | w == 0 && False  = undefined
    get1 _ _  Empty               = Empty
    get1 _ _  NoPrinter               = NoPrinter
    get1 w sl (NilAbove p)        = nilAbove_ (get (w - sl) p)
    get1 w sl (TextBeside t tl p) = stringBeside_ t tl (get1 w (sl + tl) p)
    get1 w sl (Nest _ p)          = get1 w sl p
    get1 w sl (p `Union` q)       = nicest1 w r sl (get1 w sl p)
                                                   (get1 w sl q)
    get1 _ _  (Above {})          = error "best get1 Above"
    get1 _ _  (Beside {})         = error "best get1 Beside"

nicest :: Int -> Int -> Printer -> Printer -> Printer
nicest !w !r p q = nicest1 w r 0 p q

nicest1 :: Int -> Int -> Int -> Printer -> Printer -> Printer
nicest1 !w !r !sl p q | fits ((w `min` r) - sl) p = p
                      | otherwise                 = q

fits :: Int  -- Space available
     -> Printer
     -> Bool -- True if *first line* of Printer fits in space available
fits n _ | n < 0           = False
fits _ NoPrinter               = False
fits _ Empty               = True
fits _ (NilAbove _)        = True
fits n (TextBeside _ sl p) = fits (n - sl) p
fits _ (Above {})          = error "fits Above"
fits _ (Beside {})         = error "fits Beside"
fits _ (Union {})          = error "fits Union"
fits _ (Nest {})           = error "fits Nest"

-- | @first@ returns its first argument if it is non-empty, otherwise its second.
first :: Printer -> Printer -> Printer
first p q | nonEmptySet p = p -- unused, because (get OneLineMode) is unused
          | otherwise     = q

nonEmptySet :: Printer -> Bool
nonEmptySet NoPrinter              = False
nonEmptySet (_ `Union` _)      = True
nonEmptySet Empty              = True
nonEmptySet (NilAbove _)       = True
nonEmptySet (TextBeside _ _ p) = nonEmptySet p
nonEmptySet (Nest _ p)         = nonEmptySet p
nonEmptySet (Above {})         = error "nonEmptySet Above"
nonEmptySet (Beside {})        = error "nonEmptySet Beside"

-- @oneLiner@ returns the one-line members of the given set of @Printer@s.
oneLiner :: Printer -> Printer
oneLiner NoPrinter               = NoPrinter
oneLiner Empty               = Empty
oneLiner (NilAbove _)        = NoPrinter
oneLiner (TextBeside s sl p) = stringBeside_ s sl (oneLiner p)
oneLiner (Nest k p)          = nest_ k (oneLiner p)
oneLiner (p `Union` _)       = oneLiner p
oneLiner (Above {})          = error "oneLiner Above"
oneLiner (Beside {})         = error "oneLiner Beside"















-- ---------------------------------------------------------------------------
-- Rendering

-- | A printing style.
data Style
  = Style { mode           :: Mode  -- ^ The printing mode
          , lineLength     :: Int   -- ^ Length of line, in chars
          , ribbonsPerLine :: Float -- ^ Ratio of ribbon length to line length
          }

-- | The default style (@mode=PageMode, lineLength=100, ribbonsPerLine=1.5@).
style :: Style
style = Style { lineLength = 100, ribbonsPerLine = 1.5, mode = PageMode }

-- | Rendering mode.
data Mode = PageMode     -- ^ Normal
          | ZigZagMode   -- ^ With zig-zag cuts
          | LeftMode     -- ^ No indentation, infinitely long lines
          | OneLineMode  -- ^ All on one line

-- | Render the @Printer@ to a String using the default @Style@.
runPrinter :: Printer -> String
runPrinter doc = runPrinter' (mode style) (lineLength style) (ribbonsPerLine style)
                        txtPrinter "" doc

-- | Render the @Printer@ to a String using the given @Style@.
runPrinterStyle :: Style -> Printer -> String
runPrinterStyle s doc = runPrinter' (mode s) (lineLength s) (ribbonsPerLine s)
                    txtPrinter "" doc

-- | Default TextDetails printer
txtPrinter :: TextDetails -> String -> String
txtPrinter (Chr c)   s  = c:s
txtPrinter (Str s1)  s2 = s1 ++ s2

-- | The general printing interface.
runPrinter' :: Mode                     -- ^ Rendering mode
           -> Int                      -- ^ Line length
           -> Float                    -- ^ Ribbons per line
           -> (TextDetails -> a -> a)  -- ^ What to do with string
           -> a                        -- ^ What to do at the end
           -> Printer                  -- ^ The printer
           -> a                        -- ^ Result
runPrinter' OneLineMode _ _ txt end doc
  = easy_display space_string (\_ y -> y) txt end (reducePrinter doc)
runPrinter' LeftMode    _ _ txt end doc
  = easy_display nl_string first txt end (reducePrinter doc)

runPrinter' m lineLen ribbons txt rest doc
  = display m lineLen ribbonLen txt rest doc'
  where
    doc' = best bestLineLen ribbonLen (reducePrinter doc)

    bestLineLen, ribbonLen :: Int
    ribbonLen   = round (fromIntegral lineLen / ribbons)
    bestLineLen = case m of
                      ZigZagMode -> maxBound
                      _          -> lineLen

easy_display :: TextDetails
             -> (Printer -> Printer -> Printer)
             -> (TextDetails -> a -> a)
             -> a
             -> Printer
             -> a
easy_display nl_space_string choose txt end doc
  = lay doc
  where
    lay NoPrinter              = error "easy_display: NoPrinter"
    lay (Union p q)        = lay (choose p q)
    lay (Nest _ p)         = lay p
    lay Empty              = end
    lay (NilAbove p)       = nl_space_string `txt` lay p
    lay (TextBeside s _ p) = s `txt` lay p
    lay (Above {})         = error "easy_display Above"
    lay (Beside {})        = error "easy_display Beside"

display :: Mode -> Int -> Int -> (TextDetails -> a -> a) -> a -> Printer -> a
display m !page_width !ribbon_width txt end doc
  = case page_width - ribbon_width of { gap_width ->
    case gap_width `quot` 2 of { shift ->
    let
        lay k _            | k `seq` False = undefined
        lay k (Nest k1 p)  = lay (k + k1) p
        lay _ Empty        = end
        lay k (NilAbove p) = nl_string `txt` lay k p
        lay k (TextBeside s sl p)
            = case m of
                    ZigZagMode |  k >= gap_width
                               -> nl_string `txt` (
                                  Str (replicate shift '/') `txt` (
                                  nl_string `txt`
                                  lay1 (k - shift) s sl p ))

                               |  k < 0
                               -> nl_string `txt` (
                                  Str (replicate shift '\\') `txt` (
                                  nl_string `txt`
                                  lay1 (k + shift) s sl p ))

                    _ -> lay1 k s sl p
        lay _ (Above {})   = error "display lay Above"
        lay _ (Beside {})  = error "display lay Beside"
        lay _ NoPrinter        = error "display lay NoPrinter"
        lay _ (Union {})   = error "display lay Union"

        lay1 !k s !sl p    = let !r = k + sl
                             in Str (indent k) `txt` (s `txt` lay2 r p)

        lay2 k _ | k `seq` False   = undefined
        lay2 k (NilAbove p)        = nl_string `txt` lay k p
        lay2 k (TextBeside s sl p) = s `txt` lay2 (k + sl) p
        lay2 k (Nest _ p)          = lay2 k p
        lay2 _ Empty               = end
        lay2 _ (Above {})          = error "display lay2 Above"
        lay2 _ (Beside {})         = error "display lay2 Beside"
        lay2 _ NoPrinter               = error "display lay2 NoPrinter"
        lay2 _ (Union {})          = error "display lay2 Union"
    in
    lay 0 doc
    }}

