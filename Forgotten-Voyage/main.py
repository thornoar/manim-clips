# from manim import *
# from math import sqrt, cos, sin
# from manim.utils.color.X11 import *

from combined import *
from current import *

class Main (MovingCameraScene):
    def construct(self):
        # self.add_sound("./part-2.mp3")
        combine(self)
        # current(self)
