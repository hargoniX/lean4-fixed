/-
  Copyright (c) 2021 Henrik Böving. All rights reserved.
  Released under MIT license as described in the file LICENSE.
  Authors: Henrik Böving
-/

/--
  Fixed n represents a fixed precision number with resolution n.
-/
inductive Fixed (resolution : Nat) where
| MkFixed : Int → Fixed resolution
  deriving Inhabited, BEq, DecidableEq, Repr
-- TODO: Implement a nice ToString instance
-- TODO: Derive Ord with next nightly

abbrev Uni := Fixed 1
abbrev Deci := Fixed 10
abbrev Centi := Fixed 100
abbrev Milli := Fixed 1000
abbrev Micro := Fixed 1000000
abbrev Nano := Fixed 1000000000
abbrev Pico := Fixed 1000000000000

namespace Fixed

def add : Fixed n → Fixed n → Fixed n
| MkFixed a, MkFixed b => MkFixed $ a + b

def sub : Fixed n → Fixed n → Fixed n
| MkFixed a, MkFixed b => MkFixed $ a - b

def neg : Fixed n → Fixed n
| MkFixed a => MkFixed $ -a

def mul : Fixed n → Fixed n → Fixed n
| MkFixed a, MkFixed b => MkFixed $ (a * b) / n

def div : Fixed n → Fixed n → Fixed n
| MkFixed a, MkFixed b => MkFixed $ (a * n) / b

instance : Add (Fixed n) := ⟨add⟩
instance : Sub (Fixed n) := ⟨sub⟩
instance : Mul (Fixed n) := ⟨mul⟩
instance : Div (Fixed n) := ⟨div⟩
instance : Neg (Fixed n) := ⟨neg⟩

instance : OfNat (Fixed n) m where
  ofNat := MkFixed $ n * m

def ofInt : Int → Fixed n := λ m => MkFixed $ n * m

def toFloat : Fixed n → Float
| MkFixed a => (Float.ofInt a) / (Float.ofNat n)

def nextMetricLevel : Fixed n → Fixed (n * 10)
| MkFixed a => MkFixed $ a * 100

theorem nextMetricLevel_correct : ∀ (a : Int) (n : Nat), (ofInt a : Fixed (n * 10)) * 10 = (ofInt a : Fixed n).nextMetricLevel := by
  intro a n
  simp only [ofInt, nextMetricLevel]
  apply congrArg MkFixed
  -- Since nothing about Int is formalized this equation turns out to be rather annoying
  -- to solve despite being trivial for human eye
  sorry

end Fixed
