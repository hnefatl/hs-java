module JVM.AccessFlag where

import JVM.BitMask.BitMask

data AccessFlag =
    ACC_PUBLIC       -- 0x0001 Visible for all
  | ACC_PRIVATE      -- 0x0002 Visible only for defined class
  | ACC_PROTECTED    -- 0x0004 Visible only for subclasses
  | ACC_STATIC       -- 0x0008 Static method or variable
  | ACC_FINAL        -- 0x0010 No further subclassing or assignments
  | ACC_SYNCHRONIZED -- 0x0020 Uses monitors
  | ACC_VOLATILE     -- 0x0040 Could not be cached
  | ACC_TRANSIENT    -- 0x0080
  | ACC_NATIVE       -- 0x0100 Implemented in other language
  | ACC_INTERFACE    -- 0x0200 Class is interface
  | ACC_ABSTRACT     -- 0x0400
  deriving (Eq, Show, Ord, Enum)

instance BitMask AccessFlag where
  maskBit x = case x of
    ACC_PUBLIC         -> 0      -- 0x0001
    ACC_PRIVATE        -> 1      -- 0x0002
    ACC_PROTECTED      -> 2      -- 0x0004
    ACC_STATIC         -> 3      -- 0x0008
    ACC_FINAL          -> 4      -- 0x0010
    ACC_SYNCHRONIZED   -> 5      -- 0x0020
    ACC_VOLATILE       -> 6      -- 0x0040
    ACC_TRANSIENT      -> 7      -- 0x0080
    ACC_NATIVE         -> 8      -- 0x0100
    ACC_INTERFACE      -> 9      -- 0x0200
    ACC_ABSTRACT       -> 10     -- 0x0400

data MethodAccessFlag =
  M_PUBLIC                       -- 0x0001 Declared public; may be accessed from outside its package.
  | M_PRIVATE                    -- 0x0002 Declared private; accessible only within the defining class.
  | M_PROTECTED                  -- 0x0004 Declared protected; may be accessed within subclasses.
  | M_STATIC                     -- 0x0008 Declared static.
  | M_FINAL                      -- 0x0010 Declared final; must not be overridden (§5.4.5).
  | M_SYNCHRONIZED               -- 0x0020 Declared synchronized; invocation is wrapped by a monitor use.
  | M_BRIDGE                     -- 0x0040 A bridge method, generated by the compiler.
  | M_VARARGS                    -- 0x0080 Declared with variable number of arguments.
  | M_NATIVE                     -- 0x0100 Declared native; implemented in a language other than Java.
  | M_ABSTRACT                   -- 0x0400 Declared abstract; no implementation is provided.
  | M_STRICT                     -- 0x0800 Declared strictfp; floating-point mode is FP-strict.
  | M_SYNTHETIC                  -- 0x1000
  deriving (Eq, Show, Ord, Enum)

instance BitMask MethodAccessFlag where
  maskBit x = case x of
    M_PUBLIC           ->  0        -- 0x0001
    M_PRIVATE          ->  1        -- 0x0002
    M_PROTECTED        ->  2        -- 0x0004
    M_STATIC           ->  3        -- 0x0008
    M_FINAL            ->  4        -- 0x0010
    M_SYNCHRONIZED     ->  5        -- 0x0020
    M_BRIDGE           ->  6        -- 0x0040
    M_VARARGS          ->  7        -- 0x0080
    M_NATIVE           ->  8        -- 0x0100
    M_ABSTRACT         ->  10       -- 0x0400
    M_STRICT           ->  11       -- 0x0800
    M_SYNTHETIC        ->  12       -- 0x1000

data FieldAccessFlag =
  F_PUBLIC                    -- 0x0001 Declared public; may be accessed from outside its package.
  | F_PRIVATE                 -- 0x0002 Declared private; usable only within the defining class.
  | F_PROTECTED               -- 0x0004 Declared protected; may be accessed within subclasses.
  | F_STATIC                  -- 0x0008 Declared static.
  | F_FINAL                   -- 0x0010 Declared final; never directly assigned to after object construction (JLS §17.5).
  | F_VOLATILE                -- 0x0040 Declared volatile; cannot be cached.
  | F_TRANSIENT               -- 0x0080 Declared transient; not written or read by a persistent object manager.
  | F_SYNTHETIC               -- 0x1000 Declared synthetic; not present in the source code.
  | F_ENUM                    -- 0x4000 Declared as an element of an enum.
  deriving (Eq, Show, Ord, Enum)

instance BitMask FieldAccessFlag where
  maskBit x = case x of
    F_PUBLIC       -> 0             -- 0x0001
    F_PRIVATE      -> 1             -- 0x0002
    F_PROTECTED    -> 2             -- 0x0004
    F_STATIC       -> 3             -- 0x0008
    F_FINAL        -> 4             -- 0x0010
    F_VOLATILE     -> 6             -- 0x0040
    F_TRANSIENT    -> 7             -- 0x0080
    F_SYNTHETIC    -> 12            -- 0x1000
    F_ENUM         -> 14            -- 0x4000