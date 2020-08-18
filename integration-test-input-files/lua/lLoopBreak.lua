local i = 1

while a[i] do
if a[i] == v then break end
i = i + 1
end

for j in {1,2,3,4,5}
do
if a[i] == v then break end
        i = i + 1
end

repeat
if a[i] == v then break end
   i = i + 1
until( i > 15 )


