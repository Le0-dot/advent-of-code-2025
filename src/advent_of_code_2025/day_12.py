from pathlib import Path


def solution() -> None:
    text = Path("input").read_text()
    print(text)
