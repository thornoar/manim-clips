from common import *

class Test (Scene):
    def construct (self):
        beat_time = 0.515

        nucleus_number = 200
        nucleus_radius = 4.5
        nucleus_dispersion = 1.041
        nucleus_opacity = 0.7

        # glow = create_glow(Dot(), num = 20, dispersion = 1.01, rad = 2)
        glow = create_glow(Dot(), rad = nucleus_radius, num = nucleus_number, dispersion = nucleus_dispersion, opacity = nucleus_opacity)

        self.add(glow)

        # self.play(
        #     FadeIn(glow)
        # )

        self.wait(1)

        cur_time = 3*beat_time

        n = len(glow)

        circ = Circle(color = YELLOW, fill_color = YELLOW, fill_opacity = 1.0)

        self.play(LaggedStart(
            *[
                Indicate(circle, color = ORANGE, run_time = 5*cur_time/n)
                for _,circle in enumerate(glow)
            ],
            lag_ratio = 0.2
        ))

        # self.play(
        #     Indicate(glow[0], color = ORANGE, run_time = beat_time)
        #     # *[
        #     #     Indicate(circle, color = ORANGE, run_time = beat_time, rate_func = rush_from)
        #     #     for _,circle in enumerate(glow)
        #     # ],
        # )

        # self.play(
        #     FadeIn(circ)
        # )
        # self.play(Indicate(circ, color = ORANGE))

        self.wait(1)

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
