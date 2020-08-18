::a::
goto b  -- valid (forward jump)
goto a  -- valid (backward jump)
::b::
do
  ::c::
  goto a  -- valid (backward jump out of nested block)
  goto c  -- valid (forward jump out of nested block)
  goto e  -- valid (forward jump out of nested block)
end

do ::e:: end  -- valid, but not visible outside the block; above "goto e" sees only next line
::e::  -- valid
local x
::f::
goto e  -- valid (backward jump across local definition)
