
OUT = _build
GCC_FLAGS = -O1 -Winline -Wall -Werror # -O2 makes things faster!
INTER = b/inter.b

top: hc-man

ci-man: ci-run-mandelbrot
hi-man: hi3-run-mandelbrot
hc-man: hc-run-mandelbrot

#ci-fac: ci-test-fac
#hc-fac: hc-run-factor

.SECONDARY:


# Haskell compiler...

hc-run-%: $(OUT)/%.exe
#	echo 179424691 | bash -c 'time $^'
	bash -c 'time $^'

$(OUT)/%.exe : $(OUT)/%.c wrap.c .dir
	gcc $(GCC_FLAGS) -D GEN=$< wrap.c -o $@

$(OUT)/%.c: b/%.b src/*.hs .dir
	stack run 4 $<


# Haskell intepreter...

HEXE = .stack-work/dist/x86_64-linux/Cabal-3.0.1.0/build/main.exe/main.exe

hi1-run-%: b/%.b
	stack build; bash -c 'time $(HEXE) 1 $^'

hi2-run-%: b/%.b
	stack build; bash -c 'time $(HEXE) 2 $^'

hi3-run-%: b/%.b
	stack build; bash -c 'time $(HEXE) 3 $^'


# C intepreter...


## use the input value from the Bendersky blog; takes my c-interpreter about 23s
#ci-test-fac: $(OUT)/bf.exe $(OUT)/fac.b
#	#echo 179424691 | (bash -c 'time $^')
#	echo 1234567 | (bash -c 'time $^')

## compressed (decommented) version of b/factor.b
#$(OUT)/fac.b: $(OUT)/bf.exe b/decomment.b b/factor.b .dir
#	cat b/factor.b | $< b/decomment.b > $@


# run a bf program directly
ci-run-%: $(OUT)/bf.exe b/%.b
	bash -c 'time $^'


# run a bf program via an interpreter
ci-runI-%: $(OUT)/bf.exe $(INTER) b/%.b
	(cat $(word 3, $^); echo '!') | $(word 1, $^) $(word 2, $^)

# (try!) run a bf program via 2 levels of interpreter
ci-runII-%: $(OUT)/bf.exe $(INTER) b/%.b
	(cat $(word 2, $^); echo '!'; cat $(word 3, $^); echo '!') | $(word 1, $^) $(word 2, $^)


$(OUT)/bf.exe: bf.c .dir
	gcc $(GCC_FLAGS) $< -o $@

#$(OUT)/bf.s: bf.c .dir
#	gcc $(GCC_FLAGS) $< -S -o $@

.dir:
	mkdir -p $(OUT)
