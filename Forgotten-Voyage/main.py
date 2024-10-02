from manim import *
from math import sqrt, cos, sin

def delayed_animation (anim, delay_time: float):
    tracker = ValueTracker(0)
    return Succession(
        ApplyMethod(tracker.set_value, 1, run_time = delay_time),
        anim,
        # lag_ratio = 1
    )

def create_glow(vmobject, rad = 1.0, col = YELLOW, num = 100, dispersion = 1.002):
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

def shift(group, *args, ratio = 0.0001, **kwargs):
    return LaggedStart(
        map(lambda m: m.animate(**kwargs).shift(*args), group),
        lag_ratio = ratio
    )

def dir (angle: float):
    return (cos(angle)*RIGHT + sin(angle)*UP)

def move (group, start, end, *args, **kwargs):
    return MoveAlongPath(group, Line(start = start, end = end), *args, **kwargs)

def polygon_center(p):
    return sum(p.get_vertices())/len(p.get_vertices())

class Main (MovingCameraScene):
    def construct(self):
        initial_wait = 0.140
        beat_time = 0.515
        photon_lag_ratio = 0.001
        photon_dispersion = 1.0015
        photon_number = 100
        photon_radius = 0.5
        number_of_squares = 3
        s = 1.8

        colors = [ "#0289FE", "#FC5105", "#B232D1", "#1FDEFF", "#FF4F04", "#B530D1", "#74CE03", "#29E0FF", "#FF0C41", "#019DFF", "#FF5007", "#FF1042", "#B630D3", "#75CF05", "#1EDFFE", "#FF0E43" ]

        rotating_squares = [ Square(z_index = -1, color = colors[0]).scale(s/(sqrt(2)**i)).rotate(i*PI/4) for i in range(number_of_squares) ]

        beat_square = Square(z_index = -1).scale(s).scale(sqrt(2)).set_color(BLACK)

        r_tracker = ValueTracker(8)
        theta_tracker = ValueTracker(0)
        dots = [ Dot(color = YELLOW).add_updater(lambda m: m.move_to(r_tracker.get_value() * (cos(theta_tracker.get_value())*RIGHT + sin(theta_tracker.get_value())*UP))), Dot(color = YELLOW).add_updater(lambda m: m.move_to( r_tracker.get_value() * (cos(theta_tracker.get_value()+PI/2)*RIGHT + sin(theta_tracker.get_value()+PI/2)*UP))), Dot(color = YELLOW).add_updater(lambda m: m.move_to(r_tracker.get_value() * (dir(theta_tracker.get_value()+PI)))), Dot(color = YELLOW).add_updater(lambda m: m.move_to(r_tracker.get_value() * dir(theta_tracker.get_value()+3*PI/2))), ]
        tracing_paths = [ TracedPath(dots[i].get_center, dissipating_time = 0.3, stroke_opacity = [1,1]).set_color(YELLOW) for i in range(4) ]

        number_of_cycles = 8

        rotating_square_transforms = [[square] for square in rotating_squares]
        beat_squares = [beat_square]
        for i in range(number_of_cycles):
            for j in range(number_of_squares):
                tlist = rotating_square_transforms[j]
                tlist.append(
                    tlist[len(tlist)-1].copy().rotate((-1)**j * PI/2).set_color(colors[i])
                )
        for i in range(number_of_cycles-2):
            if (i % 2 == 1):
                beat_squares.append(beat_squares[len(beat_squares)-1].copy().rotate(PI/2).scale(1.5).set_color(BLACK))
            else: 
                beat_squares.append(beat_squares[len(beat_squares)-1].copy().rotate(PI/2).scale(1/1.5).set_color(colors[i]))

        # The approah of the photons

        self.add(*dots)

        self.add_sound("./track-short.mp3")

        self.add(*rotating_squares)
        self.add(*tracing_paths)
        self.wait(initial_wait)
        self.play(
            *[ transform_succession(rotating_squares[i], rotating_square_transforms[i], beat_time) for i in range(number_of_squares) ],
            transform_succession(beat_square, beat_squares, beat_time, rate_func = lambda i: rush_from if (i % 2 == 1) else rush_into),
            r_tracker.animate(rate_func = rush_from, run_time = number_of_cycles*beat_time).set_value(s),
            theta_tracker.animate(rate_func = rush_from, run_time = number_of_cycles*beat_time).set_value(2*PI),
        )

        # Building the shell

        square_color = colors[number_of_cycles-1]
        circle_color = BLUE
        photons = [create_glow(dot, rad = photon_radius, num = photon_number, dispersion = photon_dispersion) for dot in dots]
        self.remove(*dots)
        self.remove(beat_square)
        self.remove(*tracing_paths)
        self.play(
            *[FadeIn(photon, scale = 2, run_time = beat_time, rate_func = rush_from) for photon in photons],
            *[Flash(dot, run_time = beat_time, rate_func = linear) for dot in dots]
        )
        tracing_paths = [ TracedPath(photons[i][0].get_center, dissipating_time = 0.2, stroke_opacity = [1,1]).set_color(YELLOW) for i in range(4) ]
        self.add(*tracing_paths)

        photons[1].add_updater(lambda m: m.move_to(rotating_squares[1].height/2 * UP))
        photons[3].add_updater(lambda m: m.move_to(rotating_squares[1].height/2 * DOWN))
        circle1 = Circle(color = YELLOW, radius = .15).move_to(photons[0])
        circle2 = Circle(color = YELLOW, radius = .15).move_to(photons[2])
        self.play(
            Transform( rotating_squares[1], rotating_squares[1].copy().apply_matrix([[1,0],[0,sqrt(2)]]).set_color(YELLOW)),
            FadeOut(circle1, scale = 2),
            FadeOut(circle2, scale = 2),
            run_time = beat_time,
            rate_func = rush_from,
        )
        photons[1].clear_updaters()
        photons[3].clear_updaters()
        r_tracker.set_value(s*sqrt(2))
        theta_tracker.set_value(PI/2)
        photons[1].add_updater(lambda m: m.move_to(r_tracker.get_value() * dir(theta_tracker.get_value())))
        photons[3].add_updater(lambda m: m.move_to(r_tracker.get_value() * dir(theta_tracker.get_value()-PI)))
        self.play(
            r_tracker.animate.set_value(s),
            theta_tracker.animate.set_value(0),
            rotating_squares[1].animate.set_color(GREEN),
            photons[0].animate.shift(s*DOWN),
            photons[2].animate.shift(s*UP),
            run_time = beat_time,
            rate_func = rush_from
        )
        photons[1].clear_updaters()
        photons[3].clear_updaters()
        photons[1].add_updater(lambda m: m.move_to(rotating_squares[1].width/2 * RIGHT))
        photons[3].add_updater(lambda m: m.move_to(rotating_squares[1].width/2 * LEFT))

        self.play(
            Transform(
                rotating_squares[1],
                rotating_squares[1].copy().apply_matrix([[sqrt(2),0],[0,1]]).set_color(YELLOW)
            ),
            photons[0].animate.shift(s*(1-1/sqrt(2))*(UP+LEFT)),
            run_time = beat_time,
            rate_func = rush_from
        )

        photons[1].clear_updaters()
        photons[3].clear_updaters()

        inner_shell_circle = Circle(radius = s, color = YELLOW, z_index = -0.5).rotate(-PI/4)
        inner_shell_circle_copy = inner_shell_circle.copy()
        middle_shell_octagon = RegularPolygon(8, color = YELLOW, z_index = -1).scale(sqrt(2)*s)
        middle_shell_octagon_copy = middle_shell_octagon.copy()
        middle_shell_circle = Circle(radius = s*sqrt(2), color = YELLOW, z_index = -1).rotate(PI/2+PI/4)
        middle_shell_circle_copy = middle_shell_circle.copy()

        outer_arc1 = Arc(radius = 2*s, start_angle = PI, angle = -PI/2, color = YELLOW)
        outer_arc2 = Arc(radius = 2*s, start_angle = 0, angle = -PI/2, color = YELLOW)

        number_of_cycles = 4
        cur_time = number_of_cycles*beat_time

        circles_inner = [
            Circle(radius = .15, color = YELLOW).move_to(s*dir(-PI/4+i*2*PI/number_of_cycles))
            for i in range(number_of_cycles)
        ]
        circles_middle = [
            Circle(radius = .15, color = YELLOW).move_to(s*sqrt(2)*dir(PI-PI/4+i*2*PI/number_of_cycles))
            for i in range(number_of_cycles)
        ]
        circles_middle1 = [
            Circle(radius = .15, color = YELLOW).move_to(s*sqrt(2)*dir(i*2*PI/number_of_cycles))
            for i in range(number_of_cycles)
        ]

        self.play(
            Create(inner_shell_circle, run_time = cur_time, rate_func = linear),
            MoveAlongPath(photons[0], inner_shell_circle_copy, run_time = cur_time, rate_func = linear),
            Create(middle_shell_octagon, run_time = cur_time, rate_func = linear),
            MoveAlongPath(photons[1], middle_shell_octagon_copy, run_time = cur_time, rate_func = linear),
            Create(middle_shell_circle, run_time = cur_time, rate_func = linear),
            MoveAlongPath(photons[2], middle_shell_circle_copy, run_time = cur_time, rate_func = linear),
            rotating_squares[1].animate(run_time = cur_time, rate_func = linear).set_color(square_color),
            LaggedStart(photons[3].animate(run_time = beat_time, rate_func = rush_from).move_to(2*s*LEFT), AnimationGroup( Create(outer_arc1), MoveAlongPath(photons[3], outer_arc1.copy()), run_time = beat_time, rate_func = rush_from), MoveAlongPath(photons[3], Line(start = 2*s*UP, end = 2*s*RIGHT), run_time = beat_time, rate_func = rush_from), AnimationGroup( Create(outer_arc2), MoveAlongPath(photons[3], outer_arc2.copy()), run_time = beat_time, rate_func = rush_from), lag_ratio = 1.0),
            LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 2, run_time = .9*beat_time, rate_func = rush_from)) for circle in circles_inner ], lag_ratio = 1.0),
            LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 2, run_time = .9*beat_time, rate_func = rush_from)) for circle in circles_middle ], lag_ratio = 1.0),
            LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 2, run_time = .9*beat_time, rate_func = rush_from)) for circle in circles_middle1 ], lag_ratio = 1.0),
        )

        number_of_cycles = 7
        cur_time = number_of_cycles*beat_time

        circles_inner = [
            Circle(radius = .15, color = YELLOW).move_to(s/sqrt(2)*dir(PI - PI/4 - i*PI/2))
            for i in range(4)
        ]

        middle_layer = VGroup(inner_shell_circle, rotating_squares[1], rotating_squares[0], middle_shell_octagon, middle_shell_circle)
        outer_layer = VGroup(outer_arc1, outer_arc2)

        circle1 = Circle(radius = 0.15, color = YELLOW).move_to(sqrt(2)*s*RIGHT)
        circle2 = Circle(radius = 0.15, color = YELLOW).move_to(s*(RIGHT+DOWN))

        self.play(
            inner_shell_circle.animate(run_time = cur_time, rate_func = linear).set_color(circle_color),
            middle_shell_octagon.animate(run_time = cur_time, rate_func = linear).set_color(circle_color),
            middle_shell_circle.animate(run_time = cur_time, rate_func = linear).set_color(circle_color),
            Succession(
                MoveAlongPath(photons[3], Line(start = 2*s*DOWN, end = s/2*(LEFT+DOWN)), run_time = beat_time, rate_func = rush_from),
                MoveAlongPath(photons[3], Line(start = s/2*(LEFT+DOWN), end = s/2*(LEFT+UP)), run_time = beat_time, rate_func = rush_from),
                AnimationGroup(
                    MoveAlongPath(photons[3], rotating_squares[2].copy().reverse_direction(), run_time = 4*beat_time, rate_func = linear),
                    Uncreate(rotating_squares[2], run_time = 4*beat_time, rate_func = linear),
                ),
                move(photons[3], s/2*(LEFT+UP), DOWN, run_time = beat_time, rate_func = rush_from),
            ),
            LaggedStart( Wait(run_time = 2*beat_time), *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 2, run_time = .9*beat_time, rate_func = rush_from)) for circle in circles_inner ], lag_ratio = 1.0 ),
            Succession(
                move(photons[2], s*(LEFT+UP), 2*s*LEFT, run_time = beat_time, rate_func = rush_from),
                MoveAlongPath(photons[2], Arc(radius = 2*s, start_angle = PI, angle = -PI/4), run_time = beat_time, rate_func = rush_from),
                MoveAlongPath(photons[2], Arc(radius = 2*s, start_angle = PI-PI/4, angle = -PI/4), run_time = beat_time, rate_func = rush_from),
                MoveAlongPath(photons[2], Arc(radius = 2*s, start_angle = PI/2, angle = -PI/4), run_time = beat_time, rate_func = rush_from),
                MoveAlongPath(photons[2], Arc(radius = 2*s, start_angle = PI/4, angle = -PI/2), run_time = beat_time, rate_func = rush_from),
                photons[2].animate(run_time = 2*beat_time, rate_func = rush_from).move_to(LEFT)
            ),
            Succession(
                move(photons[1], sqrt(2)*s*RIGHT, 2*s*RIGHT, run_time = beat_time, rate_func = rush_from),
                MoveAlongPath(photons[1], Arc(radius = 2*s, start_angle = 0, angle = -PI/4), run_time = beat_time, rate_func = rush_from),
                MoveAlongPath(photons[1], Arc(radius = 2*s, start_angle = -PI/4, angle = -PI/4), run_time = beat_time, rate_func = rush_from),
                MoveAlongPath(photons[1], Arc(radius = 2*s, start_angle = -PI/2, angle = -PI/4), run_time = beat_time, rate_func = rush_from),
                MoveAlongPath(photons[1], Arc(radius = 2*s, start_angle = -PI/2-PI/4, angle = -PI/2), run_time = beat_time, rate_func = rush_from),
                photons[1].animate(run_time = 2*beat_time, rate_func = rush_from).move_to(UP)
            ),
            Succession(
                Wait(run_time = beat_time),
                *[ Rotate(outer_layer, -PI/4, run_time = beat_time, rate_func = rush_from) for _ in range(3) ],
                AnimationGroup(
                    Transform(outer_arc1, Arc(radius = 2*s, start_angle = PI/4, angle = -PI/2, color = circle_color), run_time = 3*beat_time, rate_func = linear),
                    Transform(outer_arc2, Arc(radius = 2*s, start_angle = -PI/2-PI/4, angle = -PI/2, color = circle_color), run_time = 3*beat_time, rate_func = linear),
                ),
            ),
            LaggedStart(
                move(photons[0], s/sqrt(2)*(RIGHT+DOWN), s*sqrt(2)*DOWN , run_time = beat_time, rate_func = rush_from),
                AnimationGroup(
                    Rotate(rotating_squares[1], PI/2),
                    MoveAlongPath(photons[0], Arc(radius = s*sqrt(2), start_angle = -PI/2, angle = PI/2)),
                    run_time = beat_time, rate_func = rush_from
                ),
                Succession(
                    FadeIn(circle1, run_time = .1*beat_time),
                    FadeOut(circle1, scale = 2, run_time = .9*beat_time, rate_func = rush_from),
                ),
                move(photons[0], s*sqrt(2)*RIGHT, s*(RIGHT+UP), run_time = beat_time, rate_func = rush_from),
                AnimationGroup(
                    Rotate(rotating_squares[0], -PI/2),
                    MoveAlongPath(photons[0], Arc(radius = s*sqrt(2), start_angle = PI/4, angle = -PI/2)),
                    run_time = beat_time, rate_func = rush_from
                ),
                Succession(
                    FadeIn(circle2, run_time = .1*beat_time),
                    FadeOut(circle2, scale = 2, run_time = .9*beat_time, rate_func = rush_from),
                ),
                move(photons[0], s*(RIGHT+DOWN), RIGHT, run_time = beat_time, rate_func = rush_from),
                lag_ratio = 1.0,
            )   
        )
        zoom_factor = 1.4
        s = s*zoom_factor
        self.play(
            middle_layer.animate.scale(zoom_factor),
            outer_layer.animate.scale(zoom_factor),
            *[Flash(photon[0], run_time = beat_time, rate_func = rush_from) for photon in photons],
            run_time = beat_time,
            rate_func = rush_from
        )

        # Building the core

        c = 1.2
        self.play(
            photons[0].animate.move_to(ORIGIN),
            photons[1].animate.move_to(c*dir(PI/2)),
            photons[2].animate.move_to(c*dir(PI/2+2*PI/3)),
            photons[3].animate.move_to(c*dir(PI/2+4*PI/3)),
            run_time = beat_time,
            rate_func = rush_from
        )

        number_of_cycles = 2
        cur_time = number_of_cycles*beat_time

        line1 = Line(start = photons[1][0].get_center(), end = photons[2][0].get_center(), color = YELLOW, z_index = -0.5)
        line2 = Line(start = photons[2][0].get_center(), end = photons[3][0].get_center(), color = YELLOW, z_index = -0.5)
        line3 = Line(start = photons[3][0].get_center(), end = photons[1][0].get_center(), color = YELLOW, z_index = -0.5)
        circles = [Circle(radius = .01, color = YELLOW) for _ in range(4)]
        circle = Circle(radius = .01, color = YELLOW)

        self.play(
            Create(line1, run_time = cur_time, rate_func = linear),
            Create(line2, run_time = cur_time, rate_func = linear),
            Create(line3, run_time = cur_time, rate_func = linear),
            move(photons[1], c*dir(PI/2), c*dir(PI/2+2*PI/3), run_time = cur_time, rate_func = linear),
            move(photons[2], c*dir(PI/2+2*PI/3), c*dir(PI/2+4*PI/3), run_time = cur_time, rate_func = linear),
            move(photons[3], c*dir(PI/2+4*PI/3), c*dir(PI/2), run_time = cur_time, rate_func = linear),
            photons[0].animate(run_time = cur_time, rate_func = rush_into).scale(1.5),
            Succession(
                *[
                    Succession(FadeIn(circles[i], run_time = .1*beat_time), FadeOut(circles[i], scale = 30, run_time = .9*beat_time, rate_func = rush_from))
                    for i in range(number_of_cycles)
                ],
                # lag_ratio = 1.0
            )
        )

        core_triangle = VGroup(line1, line2, line3)
        core_circle_small = Circle(color = YELLOW, z_index = -0.1).scale(c/2).rotate(PI/6)
        core_circle_big = Circle(color = YELLOW, z_index = -1).scale(c).rotate(-2*PI/3-PI/6)

        self.play(
            Rotate(
                VGroup(core_triangle, photons[1], photons[2], photons[3]), -2*PI/3, about_point = ORIGIN,
                run_time = beat_time,
                rate_func = rush_from,
            ),
            Succession(FadeIn(circle, run_time = .1*beat_time), FadeOut(circle, scale = 30, run_time = .9*beat_time, rate_func = rush_from)),
        )

        number_of_cycles = 2
        cur_time = number_of_cycles*beat_time

        line1p = Line(start = photons[1][0].get_center(), end = photons[2][0].get_center(), color = ORANGE, z_index = -0.2)
        line2p = Line(start = photons[2][0].get_center(), end = photons[3][0].get_center(), color = ORANGE, z_index = -0.2)
        line3p = Line(start = photons[3][0].get_center(), end = photons[1][0].get_center(), color = ORANGE, z_index = -0.2)

        self.play(
            core_triangle.animate(run_time = cur_time, rate_func = linear).set_color(square_color),
            move(photons[1], c*dir(PI/2), c*dir(PI/2+2*PI/3), run_time = cur_time, rate_func = linear),
            move(photons[2], c*dir(PI/2+2*PI/3), c*dir(PI/2+4*PI/3), run_time = cur_time, rate_func = linear),
            move(photons[3], c*dir(PI/2+4*PI/3), c*dir(PI/2), run_time = cur_time, rate_func = linear),
            Create(line1p, run_time = cur_time, rate_func = linear),
            Create(line2p, run_time = cur_time, rate_func = linear),
            Create(line3p, run_time = cur_time, rate_func = linear),
            photons[0].animate(run_time = cur_time, rate_func = rush_into).scale(1.5),
            Succession(
                *[
                    Succession(FadeIn(circles[i], run_time = .1*beat_time), FadeOut(circles[i], scale = 30, run_time = .9*beat_time, rate_func = rush_from))
                    for i in range(number_of_cycles)
                ],
            )
        )

        core_triangle.set_color(ORANGE)
        self.remove(line1p, line2p, line3p)

        self.play(
            Flash(photons[1][0], run_time = beat_time, rate_func = rush_from),
            Flash(photons[2][0], run_time = beat_time, rate_func = rush_from),
            Flash(photons[3][0], run_time = beat_time, rate_func = rush_from),
            FadeOut(line1.copy(), shift = .5*dir(PI/6), run_time = beat_time, rate_func = rush_from),
            FadeOut(line2.copy(), shift = .5*dir(PI/6+2*PI/3), run_time = beat_time, rate_func = rush_from),
            FadeOut(line3.copy(), shift = .5*dir(PI/6+4*PI/3), run_time = beat_time, rate_func = rush_from),
            Succession(FadeIn(circle, run_time = .1*beat_time), FadeOut(circle, scale = 30, run_time = .9*beat_time, rate_func = rush_from)),
        )
        self.play(
            move(photons[3], c*dir(PI/2), c*dir(PI/2+2*PI/3), run_time = beat_time, rate_func = rush_into),
            move(photons[2], c*dir(-PI/6), c/2*dir(PI/6), run_time = beat_time, rate_func = rush_into),
            move(photons[1], c*dir(PI/6+PI), ORIGIN, run_time = beat_time, rate_func = rush_into),
            photons[0].animate(run_time = beat_time, rate_func = rush_into).scale(1.5),
        )

        number_of_cycles = 3
        cur_time = number_of_cycles * beat_time

        core_circle_big_copy = core_circle_big.copy()
        core_circle_small_copy = core_circle_small.copy()

        circles_inner = [Circle(radius = 0.15, color = YELLOW).move_to(c/2*dir(PI/6+i*2*PI/3)) for i in range(3)]
        circles_outer = [Circle(radius = 0.15, color = YELLOW).move_to(c*dir(PI/6+PI+i*2*PI/3)) for i in range(3)]

        self.play(
            Create(core_circle_big, run_time = cur_time, rate_func = linear),
            MoveAlongPath(photons[3], core_circle_big_copy, run_time = cur_time, rate_func = linear),
            Create(core_circle_small, run_time = cur_time, rate_func = linear),
            MoveAlongPath(photons[2], core_circle_small_copy, run_time = cur_time, rate_func = linear),
            LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 2, run_time = .9*beat_time, rate_func = rush_from)) for circle in circles_inner ], lag_ratio = 1.0),
            LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 2, run_time = .9*beat_time, rate_func = rush_from)) for circle in circles_outer ], lag_ratio = 1.0),
            Succession(
                *[
                    Succession(FadeIn(circles[i], run_time = .1*beat_time), FadeOut(circles[i], scale = 40, run_time = .9*beat_time, rate_func = rush_from))
                    for i in range(number_of_cycles)
                ],
            )
        )

        core_circle_big_copy.set_color(ORANGE)
        core_circle_small_copy.set_color(ORANGE)

        # self.play(
        #     move(photons[2], c/2*dir(PI/6), c*dir(PI/6), run_time = beat_time, rate_func = rush_into),
        #     move(photons[3], c*dir(PI/6+PI), c/2*dir(PI/6+PI), run_time = beat_time, rate_func = rush_into),
        #     Succession(FadeIn(circle, run_time = .1*beat_time), FadeOut(circle, scale = 70, run_time = .9*beat_time, rate_func = rush_from)),
        # )

        number_of_cycles = 3
        cur_time = number_of_cycles * beat_time

        center = Dot(color = YELLOW)

        self.play(
            MoveAlongPath(photons[3], core_circle_big_copy.copy(), run_time = cur_time, rate_func = linear),
            MoveAlongPath(photons[2], core_circle_small_copy.copy(), run_time = cur_time, rate_func = linear),
            core_circle_big.animate(run_time = cur_time, rate_func = linear).set_color(circle_color),
            core_circle_small.animate(run_time = cur_time, rate_func = linear).set_color(circle_color),
            Create(core_circle_big_copy, run_time = cur_time, rate_func = linear),
            Create(core_circle_small_copy, run_time = cur_time, rate_func = linear),
            FadeIn(center, scale = .5, run_time = cur_time, rate_func = linear),
            LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 2, run_time = .9*beat_time, rate_func = rush_from)) for circle in circles_inner ], lag_ratio = 1.0),
            LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 2, run_time = .9*beat_time, rate_func = rush_from)) for circle in circles_outer ], lag_ratio = 1.0),
            Succession(
                *[
                    Succession(FadeIn(circles[i], run_time = .1*beat_time), FadeOut(circles[i], scale = 40, run_time = .9*beat_time, rate_func = rush_from))
                    for i in range(number_of_cycles)
                ],
            ),
        )

        core_circle_big.set_color(ORANGE)
        core_circle_small.set_color(ORANGE)
        self.remove(core_circle_big_copy, core_circle_small_copy)

        core_layer = VGroup(core_circle_big, core_circle_small, core_triangle)
        lightship = VGroup(outer_layer, middle_layer, core_layer, center)

        zoom_factor = 0.6
        s *= zoom_factor
        c *= zoom_factor

        self.play(
            lightship.animate.scale(zoom_factor),
            photons[0].animate(path_arc = 2*PI/3).scale(1/(1.5**4)).move_to(4*RIGHT+1.8*UP),
            photons[1].animate(path_arc = 2*PI/3).scale(1/(1.5**1)).move_to(4.8*LEFT+0.9*UP),
            photons[2].animate(path_arc = 2*PI/3).scale(1/(1.5**1)).move_to(4.2*LEFT+2.1*DOWN),
            photons[3].animate(path_arc = 2*PI/3).scale(1/(1.5**1)).move_to(4.7*RIGHT+1.7*DOWN),
            run_time = 2*beat_time,
            rate_func = rush_from
        )

        # Starting the lightship

        number_of_cycles = 2
        cur_time = number_of_cycles * beat_time

        def radiate (num, intense = False, respond = False, focus = False):
            factor = 1.01 if intense else 1.005
            for _ in range(num):
                self.play(
                    LaggedStart(
                        Indicate(core_layer, color = YELLOW if intense else ORANGE, scale_factor = factor),
                        Indicate(middle_layer, color = YELLOW if intense else square_color, scale_factor = factor),
                        Indicate(outer_layer, color = YELLOW if intense else circle_color, scale_factor = factor),
                        lag_ratio = .2,
                        run_time = beat_time,
                        rate_func = linear
                    ),
                    FadeOut(Circle(radius = 0.05, color = YELLOW).set_fill(YELLOW, opacity = 0.3 if intense else 0.0), scale = 4*s/0.1, run_time = beat_time, rate_func = linear),
                    *(
                        [
                            FadeOut(Circle(radius = 0.05, color = YELLOW).move_to(photons[i]), scale = 20, run_time = beat_time, rate_func = rush_from)
                            for i in range(4)
                        ]
                        if respond else []
                    ),
                    *(
                        [ FocusOn(ORIGIN, run_time = beat_time/2, color = YELLOW, opacity = .1, rate_func = linear), ]
                        if focus else []
                    ),
                )

        radiate(2)
        self.play(
            photons[0].animate(path_arc = PI/2).shift(.5*(LEFT+UP)),
            photons[1].animate(path_arc = PI/7).shift(.2*(RIGHT+UP)),
            photons[2].animate(path_arc = -2*PI/3).shift(.7*RIGHT + .3*DOWN),
            photons[3].animate(path_arc = PI/4).shift(.5*LEFT+DOWN),
            run_time = beat_time,
            rate_func = rush_from
        )
        self.play(
            *[
                FadeOut(Circle(radius = 0.1, color = YELLOW).move_to(photons[i]), scale = 10, run_time = beat_time, rate_func = rush_from)
                for i in range(4)
            ]
        )

        radiate(2)

        pos = [photon[0].get_center() for photon in photons]
        self.play(
            *[
                # FadeOut(Circle(radius = 0.1, color = YELLOW).move_to(photons[i]), scale = 2, run_time = beat_time, rate_func = rush_from)
                Flash(photon[0], run_time = beat_time, rate_func = rush_from)
                for photon in photons
            ],
            photons[0].animate(path_arc = PI/2, run_time = 2*beat_time, rate_func = rush_from).move_to(pos[1] + 0.2*RIGHT + 0.6*UP),
            photons[1].animate(path_arc = PI/2, run_time = 2*beat_time, rate_func = rush_from).move_to(pos[2] - 0.3*RIGHT + 0.3*UP),
            photons[2].animate(path_arc = PI/2, run_time = 2*beat_time, rate_func = rush_from).move_to(pos[3] + 0.1*RIGHT + 0.6*UP),
            photons[3].animate(path_arc = PI/2, run_time = 2*beat_time, rate_func = rush_from).move_to(pos[0] + 0.6*RIGHT - 0.4*UP),
        )

        radiate(2)
        self.play(
            photons[0].animate(path_arc = -PI/5).shift(.4*(LEFT+UP)),
            photons[1].animate(path_arc = PI/1).shift(.2*RIGHT),
            photons[2].animate(path_arc = -2*PI/3).shift(.2*RIGHT + .3*DOWN),
            photons[3].animate(path_arc = PI/4).shift(.3*LEFT+.1*UP),
            run_time = beat_time,
            rate_func = rush_from
        )
        self.play(
            *[
                FadeOut(Circle(radius = 0.1, color = YELLOW).move_to(photons[i]), scale = 10, run_time = beat_time, rate_func = rush_from)
                for i in range(4)
            ]
        )

        radiate(2)

        circles = [ Circle(radius = 0.05, color = YELLOW).move_to(photons[i][0]) for i in range(len(photons)) ]
        scales = [20,15,25,30]

        self.play(
            LaggedStart(
                # FadeOut(Circle(radius = 0.1, color = YELLOW).move_to(photons[0]), scale = 15, run_time = beat_time, rate_func = rush_from),
                # FadeOut(Circle(radius = 0.1, color = YELLOW).move_to(photons[2]), scale = 10, run_time = beat_time, rate_func = rush_from),
                # FadeOut(Circle(radius = 0.1, color = YELLOW).move_to(photons[3]), scale = 20, run_time = beat_time, rate_func = rush_from),
                # FadeOut(Circle(radius = 0.1, color = YELLOW).move_to(photons[1]), scale = 15, run_time = beat_time, rate_func = rush_from),
                *[
                    Succession(
                        FadeIn(circles[i], scale = .5, run_time = .1*beat_time),
                        FadeOut(circles[i], scale = scales[i], run_time = .9*beat_time, rate_func = rush_from)
                    )
                    for i in range(4)
                ],
                lag_ratio = .25
            )
        )

        radiate(3, intense = True)
        radiate(1, intense = True, respond = True)
        radiate(3, intense = True)
        radiate(1, intense = True, respond = True)
        radiate(2, intense = True)
        radiate(1, intense = True, respond = False, focus = True)
        self.play(
            *[
                FadeOut(Circle(radius = 0.1, color = YELLOW).move_to(photons[i]), scale = 10, run_time = beat_time, rate_func = rush_from)
                for i in range(4)
            ]
        )
        
        cur_time = 4*beat_time

        zoom_factor = 1.2
        self.play(
            LaggedStart(
                *[
                    FocusOn(ORIGIN, run_time = beat_time, color = YELLOW, opacity = .1, rate_func = rush_from)
                    for _ in range(7)
                ],
                lag_ratio = .5,
                run_time = cur_time
            ),
            lightship.animate(run_time = cur_time, rate_func = rush_into).scale(zoom_factor),
            photons[0].animate(path_arc = PI/3, run_time = cur_time, rate_func = linear).shift(.1*(LEFT+UP)),
            photons[1].animate(path_arc = -PI/2, run_time = cur_time, rate_func = linear).shift(1.2*LEFT+.2*DOWN),
            photons[2].animate(path_arc = 3*PI/4, run_time = cur_time, rate_func = linear).shift(.2*RIGHT + .5*UP),
            photons[3].animate(path_arc = PI/4, run_time = cur_time, rate_func = linear).shift(.9*RIGHT+.3*DOWN),
        )

        s *= zoom_factor
        c *= zoom_factor

        # Light emerges, first voyage

        nucleus = create_glow(center, rad = 2.5, num = 300, dispersion = 1.0016)

        self.play(
            FadeOut(center, scale = .5, rate_func = rush_from),
            FadeIn(nucleus, scale = 0.05, rate_func = rush_from),
            Flash(ORIGIN, color = YELLOW, num_lines = 30, flash_radius = 0.1*c/2, line_length = 0.8*c/2, rate_func = rush_from),
            Flash(core_circle_small, color = YELLOW, num_lines = 30, flash_radius = c/2 + 0.1*c/2, line_length = 0.8*c/2, rate_func = rush_from),
            Flash(core_circle_big, color = YELLOW, num_lines = 30, flash_radius = c + 0.1*(s-c), line_length = 0.8*(s-c), rate_func = rush_from),
            *[Flash(photon[0], run_time = beat_time, rate_func = rush_from) for photon in photons],
            core_circle_small.animate.set_color(YELLOW),
            Rotate(middle_layer, -PI/4, rate_func = linear),
            Rotate(core_layer, PI/3, rate_func = linear),
            run_time = beat_time,
        )

        pos = [photon[0].get_center() for photon in photons ]

        self.play(
            Flash(ORIGIN, color = YELLOW, num_lines = 30, flash_radius = 0.1*c/2, line_length = 0.8*c/2, rate_func = rush_from),
            *[Flash(photon[0], run_time = beat_time, rate_func = rush_from) for photon in photons],
            photons[0].animate(path_arc = PI/3, rate_func = rush_from).move_to(pos[1]),
            photons[1].animate(path_arc = PI/3, rate_func = rush_from).move_to(pos[0]),
            photons[2].animate(path_arc = PI/3, rate_func = rush_from).move_to(pos[3]),
            photons[3].animate(path_arc = PI/3, rate_func = rush_from).move_to(pos[2]),
            Rotate(middle_layer, -PI/4, rate_func = linear),
            Rotate(core_layer, PI/3, rate_func = linear),
            run_time = beat_time,
        )

        self.play(
            Rotate(middle_layer, -PI/4, rate_func = linear),
            Rotate(core_layer, PI/3, rate_func = linear),
            *[
                Flash(photon[0], run_time = beat_time, rate_func = rush_from)
                for photon in photons
            ],
            photons[0].animate(path_arc = -2*PI/5, run_time = beat_time, rate_func = rush_from).move_to(pos[0]),
            photons[1].animate(path_arc = -2*PI/5, run_time = beat_time, rate_func = rush_from).move_to(pos[3]),
            photons[2].animate(path_arc = -2*PI/5, run_time = beat_time, rate_func = rush_from).move_to(pos[2]),
            photons[3].animate(path_arc = -2*PI/5, run_time = beat_time, rate_func = rush_from).move_to(pos[1]),
            run_time = beat_time
        )

        self.play(
            Rotate(middle_layer, -PI/4, rate_func = linear, run_time = beat_time),
            Rotate(core_layer, PI/3, rate_func = linear, run_time = beat_time),
            *[
                Flash(photon[0], run_time = beat_time, rate_func = rush_from)
                for photon in photons
            ],
            photons[0].animate(path_arc = PI/2, run_time = beat_time, rate_func = rush_from).move_to(.2*RIGHT),
            photons[1].animate(path_arc = PI/2, run_time = beat_time, rate_func = rush_from).move_to(.2*UP),
            photons[2].animate(path_arc = PI/2, run_time = beat_time, rate_func = rush_from).move_to(.2*LEFT),
            photons[3].animate(path_arc = PI/2, run_time = beat_time, rate_func = rush_from).move_to(.2*DOWN),
        )

        nucleus_layer = VGroup(nucleus, *photons)
        command_group = VGroup(outer_layer, *photons)

        self.play(
            Flash(ORIGIN, color = YELLOW, num_lines = 30, flash_radius = 0.1*c/2, line_length = 0.8*c/2, rate_func = rush_from),
            Flash(core_circle_small, color = YELLOW, num_lines = 30, flash_radius = c/2 + 0.1*c/2, line_length = 0.8*c/2, rate_func = rush_from),
            Flash(core_circle_big, color = YELLOW, num_lines = 30, flash_radius = c + 0.1*(s-c), line_length = 0.8*(s-c), rate_func = rush_from),
            Rotate(core_layer, PI/3, rate_func = linear),
            Rotate(middle_layer, -PI/4, rate_func = linear),
            Rotate(command_group, PI, about_point = ORIGIN, rate_func = linear),
            run_time = beat_time
        )

        core_group = core_layer
        core_group.add(nucleus)
        middle_group = middle_layer

        r_tracker_main = ValueTracker(0)
        theta_tracker_main = ValueTracker(PI)
        r_tracker_lag = ValueTracker(0)
        theta_tracker_lag = ValueTracker(PI)
        alpha_tracker = ValueTracker(0)

        def update_core (m):
            m.restore()
            return m.rotate(alpha_tracker.get_value()/3, about_point = ORIGIN).shift(r_tracker_main.get_value()*dir(theta_tracker_main.get_value()))

        def update_middle (m):
            m.restore()
            return m.rotate(-alpha_tracker.get_value()/4, about_point = ORIGIN).shift(r_tracker_main.get_value()*dir(theta_tracker_main.get_value()))

        def update_command (m):
            m.restore()
            return m.rotate(alpha_tracker.get_value(), about_point = ORIGIN).shift(r_tracker_lag.get_value()*dir(theta_tracker_lag.get_value()))

        core_group.save_state()
        core_group.add_updater(update_core)
        middle_group.save_state()
        middle_group.add_updater(update_middle)
        command_group.save_state()
        command_group.add_updater(update_command)

        def move_lightship (r_new, theta_new, lag, run_time, rate_func):
            return [
                alpha_tracker.animate(run_time = run_time, rate_func = linear).increment_value(PI*run_time/beat_time),
                LaggedStart(
                    AnimationGroup(
                        r_tracker_main.animate.set_value(r_new),
                        theta_tracker_main.animate.set_value(theta_new),
                    ),
                    AnimationGroup(
                        r_tracker_lag.animate.set_value(r_new),
                        theta_tracker_lag.animate.set_value(theta_new),
                    ),
                    lag_ratio = lag,
                    run_time = run_time,
                    rate_func = rate_func
                )
            ]

        self.play(
            *move_lightship(1.2, 1.05*PI, 0.0, run_time = beat_time, rate_func = smooth)
        )
        self.play(
            *move_lightship(1.3, 1.5*PI, 0.0, run_time = 2*beat_time, rate_func = smooth)
        )

        self.wait(1)
