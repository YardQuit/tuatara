# tools/benchmark/reference-env.sh --- The cores and memory of the speed reference environment.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
#
# Sourced, not run: by tools/benchmark/run.sh and by the speed tests' own
# script. It sets four variables.
#
#   reference_cpus    the processors a measured program is pinned to (taskset);
#   reference_memory  the memory of the reference machine, in bytes;
#   reference_spare   processors left for the program that measures it;
#   reference_wrap    the command that runs a program in the reference
#                     environment: in a systemd user scope whose memory
#                     (resident and page cache, not address space) is capped
#                     at reference_memory, pinned to reference_cpus.
#
# The speed tests run SBCL inside a toolbox container, out of reach of a user
# scope, with a heap of a set size; they keep their own address-space cap
# (prlimit --as) of reference_memory.
#
# The reference is a laptop of 16 GB with an Intel Core Ultra X7 358H (4
# performance, 8 efficiency and 4 low-power efficiency cores, 16 threads).
# The default layout is for an i9-13900KF, whose processors 0-15 are the two
# threads of its 8 performance cores and 16-31 its efficiency cores: one
# thread of each of 4 performance cores, 8 efficiency cores, and 4 more
# efficiency cores standing in for the low-power ones. On another machine set
# TUATARA_REFERENCE_CPUS (and TUATARA_REFERENCE_SPARE) to a layout as near as
# it allows; the benchmark's report records the layout used. The clock is not
# set here: cap it in the firmware or with the CPU frequency governor.
reference_cpus=${TUATARA_REFERENCE_CPUS:-0,2,4,6,16-23,24-27}
reference_spare=${TUATARA_REFERENCE_SPARE:-8,10,12,14}
reference_memory=${TUATARA_REFERENCE_MEMORY:-$((16 * 1024 * 1024 * 1024))}
reference_wrap="systemd-run --user --scope --quiet --collect -p MemoryMax=$reference_memory taskset -c $reference_cpus"
