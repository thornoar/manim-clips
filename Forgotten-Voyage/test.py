from common import *

class Test (Scene):
    def construct (self):
        nucleus_number = 200
        nucleus_radius = 4.5
        nucleus_dispersion = 1.041
        nucleus_opacity = 0.2

        # glow = create_glow(Dot(), num = 20, dispersion = 1.01, rad = 2)
        glow = create_glow(Dot(), rad = nucleus_radius, num = nucleus_number, dispersion = nucleus_dispersion, opacity = nucleus_opacity)

        self.add(glow)

        self.play(
            FadeIn(glow)
        )

        self.wait(10)

        # print(len(glow))
        #
        # track_disp = ValueTracker(1.0)
        # track_scale = ValueTracker(1.0)
        #
        # def update(m):
        #     m.restore()
        #     scale_glow(m, disp_scale = track_disp.get_value())
        #     m.scale(track_scale.get_value())
        #
        # glow.save_state()
        # glow.add_updater(update)
        #
        # self.play(
        #     track_disp.animate.increment_value(0.00002),
        #     track_scale.animate.set_value(2),
        #     run_time = 2
        # )
