from manim import *
from numpy import sqrt

def logo (scene: Scene):
    violin = SVGMobject(
        "./violin-2.svg"
    ).scale(2.7).shift(0.5*UP).set_color(WHITE)
    writing = Tex("\\textit{Lindsey Stirling}").shift(3*DOWN)
    group = Group(violin, writing)
    
    scene.play(DrawBorderThenFill(violin), Write(writing), t = 2)
    # play(FadeOut(group))

    number_of_squares = 3
    s = 1.8
    rotating_squares = [ Square(z_index = -1, color = "#0289FE").scale(s/(sqrt(2)**i)).rotate(i*PI/4) for i in range(number_of_squares) ]

    scene.play(
        FadeOut(group, scale = 0.8, shift = DOWN),
        FadeIn(VGroup(*rotating_squares), scale = 0.8, shift = DOWN),
    )

    scene.clear()


# class Logo (Scene):
#     def construct (self):
#         def play (*args, **kwargs):
#             self.play(*args, **kwargs)
#
#         violin = SVGMobject(
#             "./violin-2.svg"
#         ).scale(2.7).shift(0.5*UP).set_color(WHITE)
#         writing = Tex("\\textit{Lindsey Stirling}").shift(3*DOWN)
#         group = Group(violin, writing)
#         
#         play(DrawBorderThenFill(violin), Write(writing), t = 2)
#         # play(FadeOut(group))
#
#         number_of_squares = 3
#         s = 1.8
#         rotating_squares = [ Square(z_index = -1, color = "#0289FE").scale(s/(sqrt(2)**i)).rotate(i*PI/4) for i in range(number_of_squares) ]
#
#         play(
#             FadeOut(group, scale = 0.8, shift = DOWN),
#             FadeIn(VGroup(*rotating_squares), scale = 0.8, shift = DOWN),
#         )
