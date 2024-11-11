from manim import *
from math import sqrt, cos, sin

def delayed_animation (anim, delay_time: float):
    tracker = ValueTracker(0)
    return Succession(
        ApplyMethod(tracker.set_value, 1, run_time = delay_time),
        anim,
        # lag_ratio = 1
    )

def create_glow(vmobject, rad = 1.0, col = YELLOW_C, num = 100, dispersion = 1.002):
    glow_group = VGroup()
    for i in range(num):
        new_circle = Circle(radius = rad*(dispersion**(i**2))/50, stroke_opacity = 0, fill_color = col, fill_opacity = 0.2-i/300).move_to(vmobject)
        glow_group.add(new_circle)
    return glow_group

def transform_succession(mobject, mobjects, unit_time, rate_func = (lambda _: rush_from)):
    return Succession(*[
        Transform(mobject, mobjects[i+1], run_time = unit_time, rate_func = rate_func(i))
        for i in range(len(mobjects)-1)
    ])

def apply_to_group(group, anim, *args, ratio = 0.0001, **kwargs):
    return LaggedStart(
        *[anim(mobject, *args, **kwargs) for mobject in group],
        lag_ratio = ratio
    )

# def shift(group, *args, ratio = 0.0001, **kwargs):
#     return LaggedStart(
#         map(lambda m: m.animate(**kwargs).shift(*args), group),
#         lag_ratio = ratio
#     )

def dir (angle: float):
    return (cos(angle)*RIGHT + sin(angle)*UP)

def move (group, start, end, *args, **kwargs):
    return MoveAlongPath(group, Line(start = start, end = end), *args, **kwargs)

def polygon_center(p):
    return sum(p.get_vertices())/len(p.get_vertices())
