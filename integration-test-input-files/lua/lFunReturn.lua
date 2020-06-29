function newCounter ()
   local n = 0
   local k = 0
	
   return function ()
      k = n
      n = n + 1
      return n
   end
	
end
