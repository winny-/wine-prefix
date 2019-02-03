JOBS=$(shell nproc 2>/dev/null || grep -c ^processor /proc/cpuinfo)
reinstall:
	-raco pkg remove -j $(JOBS) wine-prefix
	raco pkg install -j $(JOBS) --auto --link

.PHONY: reinstall
