load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_toolchain_library",
    "haskell_library",
    "haskell_binary",
)

STACK_DEPS = ["array", "base", "text", "containers",
        "parallel", "extra", "directory", "unix", "random",
        "attoparsec"]

DEPS = ["@stackage//:{}".format(i) for i in STACK_DEPS]

def aoc_day(day):
    haskell_binary(
        name = "d{}".format(day),
        srcs = ["Day{}.hs".format(day)],
        deps = DEPS + [":Commons"],
        data = [
            "//:inputs/{}/test.txt".format(day),
            "//:inputs/{}/input.txt".format(day)
        ],
    )

def aoc_lib():
    haskell_library(
        name = "Commons",
        srcs = ["//:Commons.hs"],
        deps = DEPS,
        linkstatic = True,
    )
