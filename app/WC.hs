{-- snippet main --}
-- lines beginning with "--" are comments.

main = interact wordCount
    where wordCount input = show (length (words input)) ++ "\n"
{-- /snippet main --}
  
{-- snippet lastButOne --}
lastButOne (x:xs) = if null (drop 1 xs)
                    then x
                    else lastButOne xs
{-- /snippet lastButOne --}
