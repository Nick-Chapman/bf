
OUT = _build
GCC_FLAGS = -Wall -Werror -g
INTER = b/inter.b

top: run-mes run1-mes

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

.dir:
	mkdir -p $(OUT)
