AS=/home/caoyf/code/rust/asm/target/debug/assembler
LD=/home/caoyf/code/rust/asm/target/debug/linker
sources=$(wildcard *.s)
targets=$(patsubst %.s,%, $(sources))
objects=$(patsubst %.s,%.o, $(sources))

all: $(targets)

$(targets):%: %.o
	$(LD) -o $@ $^

%.o:%.s
	$(AS) -o $@ $^

.PHONY: run
run: $(targets)
	@for p in $^; do  \
		echo "---" $$p "---"; \
		./$$p;       \
		echo "";      \
	done

.PHONY: clean
clean:
	rm $(targets) $(objects)