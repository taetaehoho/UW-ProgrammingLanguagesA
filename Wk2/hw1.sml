
(* 1. Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if
the first argument is a date that comes before the second argument. (If the two dates are the same, the result is false.) *)
fun is_older (x : (int * int * int), y : (int * int * int)) =
	let fun date_to_days(x: (int * int * int)) =
				(#1 date) * 365 + (#2 date) * 30 + (#3 date)
	in
		date_to_days(x) < date_to_days(y)
	end

(* 2. Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns
how many dates in the list are in the given month. *)
fun number_in_month (xs : (int * int * int) list, y : int, n : int) = 
    if null xs
    then n 
    else if y = #2 (hd xs) 
    then number_in_month(tl xs, y, n+1)
    else number_in_month(tl xs, y, n)

(* 3. Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that are in any of the months in the list of months.
Assume the list of months has no number repeated. Hint: Use your answer to the previous problem. *)
fun number_in_months (xs : (int * int * int) list, ys : int list, n :int) = 
    if null xs 
    then n 
    else if null ys 
    then n
    else number_in_months(xs, tl ys, n + number_in_month (xs, hd ys, 0))

(* 4.Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
list holding the dates from the argument list of dates that are in the month. The returned list should
contain dates in the order they were originally given.  *)
fun dates_in_month (xs : (int * int * int) list, y : int, ys : (int * int * int) list) = 
    if null xs
    then ys
    else if y = #2 (hd xs)
    then dates_in_month(tl xs, y, (hd xs) :: ys)
    else dates_in_month(tl xs, y, ys)

(* 5. Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the
previous problem and SML’s list-append operator (@). *)
fun dates_in_months (xs : (int * int * int) list, ys : int list, acc : (int * int * int) list) = 
    if null xs 
    then acc  
    else if null ys 
    then acc
    else dates_in_months (xs, tl ys, dates_in_month(xs, hd ys, acc))

(* 6. Write a function get_nth that takes a list of strings and an int n and returns the n
th element of the list where the head of the list is 1st. Do not worry about the case where the
list has too few elements: your function may apply hd or tl to the empty list in this case, which is okay *)
fun get_nth (xs : string list, n : int, acc : int) =
    if null xs 
    then ""
    else if n = acc 
    then hd xs
    else get_nth (tl xs, n, acc+1)

(* 7. Write a function date_to_string that takes a date and returns a string of the form January 20, 2013
(for example). Use the operator ^ for concatenating strings and the library function Int.toString
for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, put a
comma following the day and use capitalized English month names: January, February, March, April,
May, June, July, August, September, October, November, December. *)
fun date_to_string (x : (int * int * int)) = 
    let 
        val months = ["January", "February", "March", "April",
        "May", "June", "July", "August", "September", "October", "November", "December"]
        val month = get_nth(months, (#2 x), 1)
    in month ^ " " ^ Int.toString(#3 x) ^ ", " ^ Int.toString(#1 x) 
    end 

(* 8. Write a function number_before_reaching_sum that takes an int called sum, which you can assume
is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
You should return an int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in
value; it is okay for an exception to occur if this is not the case. *)
fun number_before_reaching_sum(sum : int, xs : int list, acc : int) = 
    if acc + hd xs + (hd (tl xs)) >= sum
    then hd xs 
    else number_before_reaching_sum(sum, tl xs, hd xs + acc)

(* 9. Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns
what month that day is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your
answer to the previous problem. *)
fun what_month (x : int) = 
    let 
        val length_of_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        val months = ["January", "February", "March", "April",
        "May", "June", "July", "August", "September", "October", "November", "December"]
        fun number_before_reaching_sum(sum : int, xs : int list, acc : int, index : int) = 
            if acc + hd xs >= sum
            then index 
            else number_before_reaching_sum(sum, tl xs, hd xs + acc, index + 1)
    in number_before_reaching_sum(x, length_of_months, 0, 1)
    end 

(* 10. Write a function month_range that takes two days of the year day1 and day2 and returns an int list
[m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month
of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2. *)
fun month_range (x : int, y : int, xs : int list) = 
    let 
        val month = what_month (x)
    in 
        if x = y 
        then month :: xs
        else if x > y 
        then xs 
        else month_range(x+1, y, month :: xs)
    end

(* 11. Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It
evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list. *)
(*fn: int list -> int option *)
fun oldest (xs : (int * int * int) list) = 
    if null xs 
    then NONE 
    else 
        let val old = oldest (tl xs)
        in 
            if isSome old andalso is_older(valOf old, hd xs)
            then old
            else SOME (hd xs)   
        end