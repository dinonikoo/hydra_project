from dataclasses import dataclass

x = 890

y = 52

str = "awful"

def eq(str: str) -> bool:
    return (str == "awesome")

def func(x: int, y: int) -> bool:
    return ((x >= 10) and (y < 100))

def describe_number(y: int) -> str:
    return ("negative" if (y < 0) else ("zero" if (y == 0) else "positive"))
