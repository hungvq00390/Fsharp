module COUNT100

val countTo100: n:int{0 <= n /\ n <= 100} -> Tot (x:int{x=100}) (decreases %[100-n])
let rec countTo100 n =
  if n < 100 then countTo100 (n + 1)
  else n

let test1 = assert((countTo100 20) = 100)