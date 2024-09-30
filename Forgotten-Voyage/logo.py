from manim import *

class Logo (Scene):
    def construct (self):
        def play (*args, **kwargs):
            self.play(*args, **kwargs)

        violin = SVGMobject(
            "./violin-2.svg"
        ).scale(2.7).shift(0.5*UP).set_color(WHITE)
        writing = Tex("\\textit{Lindsey Stirling}").shift(3*DOWN)
        group = Group(violin, writing)
        
        play(DrawBorderThenFill(violin), Write(writing), t = 2)
        play(FadeOut(group))
