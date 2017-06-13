A simple stream library modified from code in flexi-streams.

This has a single exported function MAKE-UPDATE-STRING that takes an adjustable array with a fill-pointer and
returns a stream that reads bytes into that array.  This is useful for times where you want to stub out the
filesystem or network in test code.
