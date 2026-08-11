from __future__ import annotations

import colorsys
import hashlib

DEFAULT_COLORS = [
    "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F",
    "#E5C494", "#B3B3B3", "#FF69B4", "#1F78B4", "#33A02C", "#FB9A99",
    "#CAB2D6", "#FDBF6F", "#B15928", "#A6CEE3", "#B2DF8A", "#6A3D9A",
    "#FF7F00", "#FFFF99", "#E31A1C", "#F4A582", "#92C5DE", "#D1E5F0",
]

SPECIAL_COLORS = {
    "男": "#92C5DE", "女": "#E78AC3", "+": "#FF7F00", "结果无效": "#B3B3B3",
    "-": "#B2DF8A", "±": "#CAB2D6", "阴性": "#B3B3B3", "正常": "#B2DF8A",
    "偏高": "#FF7F00", "偏低": "#92C5DE",
}


def string_to_color(text: object, saturation: float = 0.6, lightness: float = 0.6) -> str:
    value = "" if text is None else str(text)
    if not value or value == "NA":
        return "#CCCCCC"
    if value in SPECIAL_COLORS:
        return SPECIAL_COLORS[value]
    digest = hashlib.md5(value.encode("utf-8"), usedforsecurity=False).hexdigest()
    hue = (int(digest[:6], 16) % 360) / 360
    red, green, blue = colorsys.hls_to_rgb(hue, lightness, saturation)
    return "#{:02X}{:02X}{:02X}".format(round(red * 255), round(green * 255), round(blue * 255))


def palette(values: list[object], mode: str = "默认颜色") -> list[str]:
    if mode == "哈希颜色":
        return [string_to_color(value) for value in values]
    return [DEFAULT_COLORS[i % len(DEFAULT_COLORS)] for i in range(len(values))]
