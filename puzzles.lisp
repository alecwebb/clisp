(defun fib (n)
	;;call the helper function with our base cases
	(fib-helper 1 0 n)
)
(defun fib-helper (x y n_times) 
	;;use iteration to calculate each step using recursion
	(if (eq n_times 0) y
		(fib-helper (+ x y) x (- n_times 1)) ) 
)
(defun reversed (xs)
	;;pull the head, and append it to the reversed tail recursively
	(if (null xs) nil
	(append (reversed (cdr xs)) (list (car xs))))
)
(defun is_prime (n)
	;;prime numbers are greater than 1, not divisible by anything but themselves
	;;cycle through 2 to n-1 to try all possibilities
	(if (<= n 1)
		(return-from is_prime nil)
	    (dotimes (i n)
		(if (and (> i 1) (< i n))
				(if (zerop (mod n i))
					(return-from is_prime nil)))))
	(return-from is_prime t)
)
(defun nub (xs)
	;;create an empty list, traverse the given list
	;;push items from xs not in nub into nub
	(setq nub nil)
	(loop for i in xs do
		(if (null (find i nub)) (push i nub))

	)
	(return-from nub (reversed nub))
)

(defun zip_with (f xs ys)
	;;first check if the lists have entries
	;;if they do map the function over the list
	(if (or (< (list-length xs) 1) (< (list-length ys) 1)) nil
		(map 'list f xs ys))
)
(defun collatz (n)
	;;create a list to place the numbers in
	(setq c nil)

	;;push the first number into the list
	(push n c)

	;;check the even and odd, apply the correct calculation, place into list
	(loop
		(if (eq n 1) (return-from collatz c))
		(if (evenp n) (setq n (/ n 2)) (setq n (+ (* n 3) 1)) )
		(push n c)
		
	(when (= n 1) (return (reversed c)))
	)
)
(defun mean (xs)
	;;use apply to sum the list, then divide by list length, cast into a float and return
	(return-from mean (setq s (float (/ (apply #'+ xs) (list-length xs)))))
)
(defun median (xs)
	;;put the list in order
	(sort xs #'<)

	;;find the middle, if odd pull element from list, return m
	;;if even average the two middle positions
	(setq f (floor (list-length xs) 2))
	(if (oddp (list-length xs)) (float (setq m (elt xs f)))
		(setq n (float (/ (+ (elt xs f) (elt xs (- f 1)) ) 2)))

	)
)
(defun mode (xs)
	;;put the list in order
	(setq a (sort xs #'<))

	;;create new list with the unique items
	(setq b (nub xs))

	;;find the maximum item that we need to loop until
	(setq mi (reduce #'max b))
	
	;;make list that stores counts
	(setq c nil)
	(loop for i in b do
		(push (count i a :from-end t) c)
	)

	;;now we have two lists, b holds the numbers, d holds the count of each number in the same order
	(setq d (reversed c))

	;;one last mode list
	(setq mode nil)

	;;find the count max
	(setq cmax (reduce #'max d))

	(setq j 0)
	
	;;loop again to place the right numbers
	(loop for i in d do
		(if (= (elt d (position i d)) cmax) (push (elt b j) mode ) )
		(incf j)
	)
	(reversed mode)
)
(defun list_report (xs)
	;;use helper functions
	(list (mean xs) (median xs) (mode xs))
)
(defun checkvalid (grid)
	;;pull out the rows 1 at a time and check the mode
	;;if mode contains each element then we have a valid solution
	(loop for (a b c d e f g h i) in grid collect
		( = (list-length (mode (list a b c d e f g h i))) 9 )
	)
)
(defun checkrows (grid)
	(checkvalid grid)
)
(defun checkcols (grid)
	;;(apply #'mapcar #'list grid) transposes the list
	(checkvalid (apply #'mapcar #'list grid))
)
(defun grabgrp1 (grid)
	;;this grabs the first 9x3 grid into lists
	(loop for (a b c d e f g h i) in grid collect
		(list a b c)
	)
)
(defun grabgrp2 (grid)
	;;this grabs the second 9x3 grid into lists
	(loop for (a b c d e f g h i) in grid collect
		(list d e f)
	)
)
(defun grabgrp3 (grid)
	;;this grabs the third 9x3 grid into lists
	(loop for (a b c d e f g h i) in grid collect
		(list g h i)
	)
)
(defun flatten (grid)
	;;remove sublisting
	(apply #'append grid)
)
(defun sep3by3 (grid)
	(setq grp1 nil)
	(setq grp2 nil)
	(setq grp3 nil)
	(loop for i in grid do
		(if (<= (list-length grp1) 8) (push i grp1) 
			(if (<= (list-length grp2) 8) (push i grp2)
				(if (<= (list-length grp3) 8) (push i grp3)
				)
			)		
		)

	)
	;;make 3 lists by placing the 9 elements
	;;then we can run the modes on each and feed, use this function 3
	;;times in conjunction with the grabs
	
	(return-from sep3by3 (list grp1 grp2 grp3))
)
(defun checkgrps (grid)
	;;build up the lists
	(list
	(checkvalid (sep3by3 (flatten (grabgrp1 grid))))
	(checkvalid (sep3by3 (flatten (grabgrp2 grid))))
	(checkvalid (sep3by3 (flatten (grabgrp3 grid))))
	)
	;;should use badrows/badcols to check for correctness.
)
(defun check_sudoku (grid)
	(setq matchrowcol (list t t t t t t t t t))
	;;could prob refactor using flatten
	(setq matchgrps (list (list t t t) (list t t t) (list t t t)))
	(and (equal (checkrows grid) matchrowcol) (equal (checkcols grid) matchrowcol) 
	     (equal (checkgrps grid) matchgrps)

	)
)
