"""Test functions."""

from __future__ import annotations
import hydra.lib.equality
import hydra.lib.logic

x = 890

y = 52

str = "awful"

def eq(str: str) -> bool:
    return hydra.lib.equality.equal_string(str, "awesome")

def func(x: int, y: int) -> bool:
    return hydra.lib.logic.and_(hydra.lib.equality.gte_int32(x, 10), hydra.lib.equality.lt_int32(y, 100))

def describe_number(y: int) -> str:
    return hydra.lib.logic.if_else(hydra.lib.equality.lt_int32(y, 0), "negative", hydra.lib.logic.if_else(hydra.lib.equality.equal_int32(y, 0), "zero", "positive"))
