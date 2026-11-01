# Assorted constructs that exercised generator fixes.
from __future__ import annotations
from os import *

type Alias = list[int]


def f(a: int = 1, *, b: str = "x", **kw) -> None:
    counts: dict[str, int] = {}
    total: int
    if a is None or a is not True:
        pass
    return ...


def g(n):
    match n:
        case 0:
            return False
        case _:
            return n * 0x1F + 0b10 + 1e3


class C:
    def m(self, xs):
        return [x for x in xs if x] + list(map(lambda y: y, xs))
