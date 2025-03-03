# from manim import *
# from math import sqrt, cos, sin
# from manim.utils.color.X11 import *

from combined import *
from logo import *
from current import *

class Main (MovingCameraScene):
    def construct(self):
        logo(self)
        combine(self)
        # current(self)
        self.clear()
