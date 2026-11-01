# String literals: the text lives in a hidden external token (_string_content).
plain = "hello"
empty = ""
escaped = "tab\there\nnewline \q"
raw = r"\d+"
fstring = f"x = {x!r:>10} and {{braces}}"
triple = """multi
line"""
joined = "a" "b"
