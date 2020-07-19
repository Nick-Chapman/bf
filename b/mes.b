
This program prints the message "Ocaml\n"
It makes use of value wrapping; but technically this behaviour is undefined
Also it takes care to zero cells before use; but this is not needed

[-] >				| 0
[-] ++++++++++ ++++++++++ >	| 20
[-] -- >			| minus 2
[-] ++++++++++++ >		| 12
[-] - >				| minus 1
[-] >				| 0

'O'
[-]>[-]<
+++++        5
[->++++<]>   times 4
[-<++++>]<   times 4
- .          minus 1 (79) then print

<<[<]>       back to start of data increments (20)
[*
    [ [>]>+<<[<]>- ] > [>]>   .   add increment to print cell & print
    <<[<]>			  back for next data item
]

newline
++++++++++.
