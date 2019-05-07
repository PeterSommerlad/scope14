# scope14
C++14 version of `unique_resource` and scope_guards from LFTS3

**HELP WANTED with making it compile and work with -std=c++14 **

need to re-establish factories for scope guards and `unique_resource` (done)

need replacements for traits (semi-done, is_invokable missing might disable static asserts for the moment)

need to make tests compile (plenty of work)

replace if constexpr (quick hack strip constexpr, but need overload SFINAE again)
