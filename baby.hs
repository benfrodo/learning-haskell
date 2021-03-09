doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                    then x
                    else doubleMe x


boomBang xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum[1 | _ <- xs]

factorial :: Integer -> Integer 
factorial x = product [1..x]