# plegg
plegg is a semi-cross-platform Haskell interface for OpenBSD's pledge(2).
## "Semi-Cross-Platform"
The term "semi-cross-platform" is used because although the functions which plegg exports use `pledge(2)` and `unveil(2)` on OpenBSD, these functions do absolutely nothing on other systems.
