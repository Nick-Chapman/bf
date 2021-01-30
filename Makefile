
OUT = _build
GCC_FLAGS = -O1 -Winline -Wall -Werror
INTER = b/inter.b

top: test-fac-quick-haskell
qui: test-fac-quick
fac: test-fac
exe: $(OUT)/bf.exe
man: run-mandelbrot


# run the basic haskell interpreter on the quick factorization example
test-fac-quick-haskell: $(OUT)/fac.b
	#echo 1234567 | stack run $^
	echo 12345 | stack run $^ # even quicker


# use the input value from the Bendersky blog; takes my c-interpreter about 23s
test-fac: $(OUT)/bf.exe $(OUT)/fac.b
	echo 179424691 | (bash -c 'time $^')

# use a smaller input value so the run is almost instantaneous
test-fac-quick: $(OUT)/bf.exe $(OUT)/fac.b
	echo 1234567 | $^

# compressed (decommented) version of b/factor.b
$(OUT)/fac.b: $(OUT)/bf.exe b/decomment.b b/factor.b
	cat b/factor.b | $< b/decomment.b > $@



# run a bf program directly
run-%: $(OUT)/bf.exe b/%.b
	$^

# run a bf program via an interpreter
run1-%: $(OUT)/bf.exe $(INTER) b/%.b
	(cat $(word 3, $^); echo '!') | $(word 1, $^) $(word 2, $^)

# (try!) run a bf program via 2 levels of interpreter
run2-%: $(OUT)/bf.exe $(INTER) b/%.b
	(cat $(word 2, $^); echo '!'; cat $(word 3, $^); echo '!') | $(word 1, $^) $(word 2, $^)

$(OUT)/bf.exe: bf.c .dir
	gcc $(GCC_FLAGS) $< -o $@

$(OUT)/bf.s: bf.c .dir
	gcc $(GCC_FLAGS) $< -S -o $@

.dir:
	mkdir -p $(OUT)
