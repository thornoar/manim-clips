from manim import *
from math import sqrt, cos, sin
from manim.utils.color.X11 import *

from numpy import random

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
    def part_one (self):
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
        dots = [ Dot(color = YELLOW_C).add_updater(lambda m: m.move_to(r_tracker.get_value() * (cos(theta_tracker.get_value())*RIGHT + sin(theta_tracker.get_value())*UP))), Dot(color = YELLOW_C).add_updater(lambda m: m.move_to( r_tracker.get_value() * (cos(theta_tracker.get_value()+PI/2)*RIGHT + sin(theta_tracker.get_value()+PI/2)*UP))), Dot(color = YELLOW_C).add_updater(lambda m: m.move_to(r_tracker.get_value() * (dir(theta_tracker.get_value()+PI)))), Dot(color = YELLOW_C).add_updater(lambda m: m.move_to(r_tracker.get_value() * dir(theta_tracker.get_value()+3*PI/2))), ]
        tracing_paths = [ TracedPath(dots[i].get_center, dissipating_time = 0.3, stroke_opacity = [1,1]).set_color(YELLOW_C) for i in range(4) ]

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
        self.add(*rotating_squares)
        self.add(*tracing_paths)

        self.add_sound("./track-short.mp3")

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
        photons = [create_glow(dot, rad = photon_radius, num = photon_number, dispersion = photon_dispersion, col = YELLOW_C) for dot in dots]
        self.remove(*dots)
        self.remove(beat_square)
        self.remove(*tracing_paths)
        self.play(
            *[FadeIn(photon, scale = 2, run_time = beat_time, rate_func = rush_from) for photon in photons],
            *[Flash(dot, run_time = beat_time, rate_func = linear) for dot in dots]
        )
        tracing_paths = [ TracedPath(photons[i][0].get_center, dissipating_time = beat_time/2, stroke_opacity = [1,1]).set_color(YELLOW_C) for i in range(4) ]
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
            Circle(radius = .15, color = YELLOW_C).move_to(s*dir(-PI/4+i*2*PI/number_of_cycles))
            for i in range(number_of_cycles)
        ]
        circles_middle = [
            Circle(radius = .15, color = YELLOW_C).move_to(s*sqrt(2)*dir(PI-PI/4+i*2*PI/number_of_cycles))
            for i in range(number_of_cycles)
        ]
        circles_middle1 = [
            Circle(radius = .15, color = YELLOW_C).move_to(s*sqrt(2)*dir(i*2*PI/number_of_cycles))
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
            Circle(radius = .15, color = YELLOW_C).move_to(s/sqrt(2)*dir(PI - PI/4 - i*PI/2))
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
        circles = [Circle(radius = .01, color = YELLOW_C) for _ in range(4)]
        circle = Circle(radius = .01, color = YELLOW_C)

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

        circles_inner = [Circle(radius = 0.15, color = YELLOW_C).move_to(c/2*dir(PI/6+i*2*PI/3)) for i in range(3)]
        circles_outer = [Circle(radius = 0.15, color = YELLOW_C).move_to(c*dir(PI/6+PI+i*2*PI/3)) for i in range(3)]

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
                            FadeOut(Circle(radius = 0.05, color = YELLOW_C).move_to(photons[i]), scale = 20, run_time = beat_time, rate_func = rush_from)
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
                FadeOut(Circle(radius = 0.1, color = YELLOW_C).move_to(photons[i]), scale = 10, run_time = beat_time, rate_func = rush_from)
                for i in range(4)
            ]
        )

        radiate(2)

        pos = [photon[0].get_center() for photon in photons]
        self.play(
            *[
                # FadeOut(Circle(radius = 0.1, color = YELLOW_C).move_to(photons[i]), scale = 2, run_time = beat_time, rate_func = rush_from)
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
                FadeOut(Circle(radius = 0.1, color = YELLOW_C).move_to(photons[i]), scale = 10, run_time = beat_time, rate_func = rush_from)
                for i in range(4)
            ]
        )

        radiate(2)

        circles = [ Circle(radius = 0.05, color = YELLOW_C).move_to(photons[i][0]) for i in range(len(photons)) ]
        scales = [20,15,25,30]

        self.play(
            LaggedStart(
                *[
                    Succession(
                        FadeIn(circles[i], scale = .5, run_time = .1*beat_time),
                        FadeOut(circles[i], scale = scales[i], run_time = .9*beat_time, rate_func = rush_from)
                    )
                    for i in [0,2,1,3]
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
                FadeOut(Circle(radius = 0.1, color = YELLOW_C).move_to(photons[i]), scale = 10, run_time = beat_time, rate_func = rush_from)
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

        # nucleus_layer = VGroup(nucleus, *photons)
        command_group = VGroup(outer_layer, *photons)
        # command_group.rotate(2*PI, about_point = ORIGIN)

        self.remove(*tracing_paths)
        tracing_paths = [ TracedPath(photons[i][0].get_center, dissipating_time = 2*beat_time, stroke_opacity = [1,1]).set_color(YELLOW_C) for i in range(4) ]
        self.add(*tracing_paths)

        cur_time = 1.5*beat_time

        self.play(
            Flash(ORIGIN, color = YELLOW, num_lines = 30, flash_radius = 0.1*c/2, line_length = 0.8*c/2, rate_func = rush_from),
            Flash(core_circle_small, color = YELLOW, num_lines = 30, flash_radius = c/2 + 0.1*c/2, line_length = 0.8*c/2, rate_func = rush_from),
            Flash(core_circle_big, color = YELLOW, num_lines = 30, flash_radius = c + 0.1*(s-c), line_length = 0.8*(s-c), rate_func = rush_from),
            Rotate(core_layer, cur_time/beat_time*PI/3, rate_func = linear),
            Rotate(middle_layer, -cur_time/beat_time*PI/4, rate_func = linear),
            Rotate(command_group, cur_time/beat_time*PI, about_point = ORIGIN, rate_func = linear),
            run_time = cur_time
        )

        core_group = core_layer
        nucleus_group = VGroup(nucleus)
        # core_group.add(nucleus)
        middle_group = middle_layer

        nucleus_scale_tracker = ValueTracker(1)
        r_tracker_main = ValueTracker(0)
        theta_tracker_main = ValueTracker(PI/2)
        r_tracker_lag = ValueTracker(0)
        theta_tracker_lag = ValueTracker(PI/2)
        x_tracker_main = ValueTracker(0)
        y_tracker_main = ValueTracker(0)
        x_tracker_lag = ValueTracker(0)
        y_tracker_lag = ValueTracker(0)
        alpha_tracker = ValueTracker(0)
        middle_scale_tracher = ValueTracker(1.0)
        command_scale_tracher = ValueTracker(1.0)

        def update_nucleus (m):
            m.restore()
            return m.scale(nucleus_scale_tracker.get_value()).shift(r_tracker_main.get_value()*dir(theta_tracker_main.get_value())).shift(x_tracker_main.get_value()*RIGHT + y_tracker_main.get_value()*UP)

        def update_core (m):
            m.restore()
            return m.rotate(alpha_tracker.get_value()/3, about_point = ORIGIN).shift(r_tracker_main.get_value()*dir(theta_tracker_main.get_value())).shift(x_tracker_main.get_value()*RIGHT + y_tracker_main.get_value()*UP)

        def update_middle (m):
            m.restore()
            return m.rotate(-alpha_tracker.get_value()/4, about_point = ORIGIN).scale(middle_scale_tracher.get_value()).shift(r_tracker_main.get_value()*dir(theta_tracker_main.get_value())).shift(x_tracker_main.get_value()*RIGHT + y_tracker_main.get_value()*UP)

        def update_command (m):
            m.restore()
            return m.rotate(alpha_tracker.get_value(), about_point = ORIGIN).scale(command_scale_tracher.get_value()).shift(r_tracker_lag.get_value()*dir(theta_tracker_lag.get_value())).shift(x_tracker_lag.get_value()*RIGHT + y_tracker_lag.get_value()*UP)

        nucleus.save_state()
        nucleus.add_updater(update_nucleus)
        core_group.save_state()
        core_group.add_updater(update_core)
        middle_group.save_state()
        middle_group.add_updater(update_middle)
        command_group.save_state()
        command_group.add_updater(update_command)

        alpha_rate = 1

        def move_lightship (r_delta = 0.0, theta_delta = 0.0, x_delta = 0.0, y_delta = 0.0, run_time = beat_time, r_rate_func = smooth, theta_rate_func = smooth, x_rate_func = smooth, y_rate_func = smooth, move_camera = True, frame_rate_func = linear, lag = 0.05, relative = True):
            r_new  = r_tracker_main.get_value() + r_delta if relative else r_delta
            theta_new  = theta_tracker_main.get_value() + theta_delta if relative else theta_delta
            x_new = x_tracker_main.get_value() + x_delta if relative else x_delta
            y_new = y_tracker_main.get_value() + y_delta if relative else y_delta
            return [
                *([
                    self.camera.frame.animate(run_time = run_time, rate_func = frame_rate_func).move_to(r_new*dir(theta_new) + x_new*RIGHT + y_new*UP) # pyright: ignore[reportAttributeAccessIssue]
                ] if move_camera else []),
                alpha_tracker.animate(run_time = run_time, rate_func = linear).increment_value(alpha_rate*PI*run_time/beat_time),
                (LaggedStart(
                    AnimationGroup(
                        r_tracker_main.animate(rate_func = r_rate_func).increment_value(r_delta),
                        theta_tracker_main.animate(rate_func = theta_rate_func).increment_value(theta_delta),
                        x_tracker_main.animate(rate_func = x_rate_func).increment_value(x_delta),
                        y_tracker_main.animate(rate_func = y_rate_func).increment_value(y_delta),
                    ),
                    AnimationGroup(
                        r_tracker_lag.animate(rate_func = r_rate_func).increment_value(r_delta),
                        theta_tracker_lag.animate(rate_func = theta_rate_func).increment_value(theta_delta),
                        x_tracker_lag.animate(rate_func = x_rate_func).increment_value(x_delta),
                        y_tracker_lag.animate(rate_func = y_rate_func).increment_value(y_delta),
                    ),
                    lag_ratio = lag,
                    run_time = run_time
                ) if relative else LaggedStart(
                    AnimationGroup(
                        x_tracker_main.animate(rate_func = x_rate_func).set_value(x_delta),
                        y_tracker_main.animate(rate_func = y_rate_func).set_value(y_delta),
                    ),
                    AnimationGroup(
                        x_tracker_lag.animate(rate_func = x_rate_func).set_value(x_delta),
                        y_tracker_lag.animate(rate_func = y_rate_func).set_value(y_delta),
                    ),
                    lag_ratio = lag,
                    run_time = run_time
                ))
            ]

        self.play(
            *move_lightship(r_delta = 1.4, theta_delta = 3*PI/2, run_time = 2.8*beat_time, r_rate_func = rush_from, theta_rate_func = rush_from, move_camera = True),
        )

        # Lighting torches

        torch_radius = 0.3
        def create_torch (pos):
            return VGroup(
                Circle(radius = torch_radius).set_color(ORANGE).move_to(pos),
                create_glow(Dot(pos), rad = 0.5, num = 70)
            )

        def prelight_torch (torch, *args, **kwargs):
            return Create(torch[0], *args, **kwargs)

        def light_torch (torch, *args, **kwargs):
            return AnimationGroup(
                torch[0].animate.set_color(YELLOW),
                FadeIn(torch[1], scale = .3),
                *args,
                **kwargs
            )

        def get_current_pos ():
            return r_tracker_main.get_value()*dir(theta_tracker_main.get_value()) + (x_tracker_main.get_value() * RIGHT + y_tracker_lag.get_value() * UP)

        vec = 7*dir(theta_tracker_main.get_value()+PI/2)
        torch1 = create_torch(get_current_pos() + vec)
        self.play(
            *move_lightship(x_delta = vec[0], y_delta = vec[1], run_time = 1.1*beat_time, x_rate_func = rush_from, y_rate_func = rush_from, frame_rate_func = linear),
            prelight_torch(torch1, run_time = 1.1*beat_time, rate_func = linear)
        )
        self.play(
            *move_lightship(r_delta = 0.0, theta_delta = PI/2.5, run_time = 1.1*beat_time, r_rate_func = linear, theta_rate_func = linear, frame_rate_func = linear),
            light_torch(torch1, run_time = 1.1*beat_time, rate_func = rush_from)
        )
        vec = 7*dir(theta_tracker_main.get_value()+PI/2)
        torch2 = create_torch(get_current_pos() + vec)
        self.play(
            *move_lightship(x_delta = vec[0], y_delta = vec[1], run_time = 1*beat_time, x_rate_func = rush_from, y_rate_func = rush_from, frame_rate_func = linear),
            prelight_torch(torch2, run_time = 1.1*beat_time, rate_func = linear)
        )
        self.play(
            *move_lightship(r_delta = 0.0, theta_delta = PI/2.5, run_time = 1*beat_time, r_rate_func = linear, theta_rate_func = linear, frame_rate_func = linear),
            light_torch(torch2, run_time = 1.1*beat_time, rate_func = rush_from)
        )

        vec = 6*dir(theta_tracker_main.get_value()+PI/2)
        self.play(
            *move_lightship(x_delta = vec[0], y_delta = vec[1], run_time = 1*beat_time, x_rate_func = linear, y_rate_func = linear, move_camera = False),
            self.camera.frame.animate(rate_func = linear, run_time = beat_time).shift(-5*RIGHT).scale(2.5), # pyright: ignore[reportAttributeAccessIssue]
        )
        self.play(
            *move_lightship(x_delta = 17.9, y_delta = -6.1, run_time = 1*beat_time, x_rate_func = linear, y_rate_func = linear, move_camera = False),
            r_tracker_main.animate(run_time = beat_time).set_value(0),
            r_tracker_lag.animate(run_time = beat_time).set_value(0),
            self.camera.frame.animate(rate_func = linear, run_time = beat_time).shift(-5*RIGHT).scale(2.5), # pyright: ignore[reportAttributeAccessIssue]
        )

        cur_time = 1.5*beat_time
        self.play(
            # *move_lightship(move_camera = False, run_time = 1.5*beat_time),
            alpha_tracker.animate.increment_value(alpha_rate * PI * cur_time/beat_time),
            middle_scale_tracher.animate.set_value(0.7),
            command_scale_tracher.animate.set_value(1.2),
            nucleus_scale_tracker.animate.set_value(1.2),
            # FocusOn(nucleus[0].get_center(), color = YELLOW, opacity = .15),
            run_time = cur_time,
            rate_func = linear
        )

        number_of_torches = 27
        random.seed(6)
        x_randoms = random.random(30)
        y_randoms = random.random(30)
        x_min = -51
        x_max = 16
        y_min = -13
        y_max = 31
        x_coords = [ interpolate(x_min, x_max, x_randoms[i]) for i in range(number_of_torches) ]
        y_coords = [ interpolate(y_min, y_max, y_randoms[i]) for i in range(number_of_torches) ]

        torches = [ create_torch(x_coords[i]*RIGHT + y_coords[i]*UP) for i in range(number_of_torches) ]
        torches.append(torch1)
        torches.append(torch2)

        alpha_rate = 4

        self.play(
            *move_lightship(x_delta = x_coords[0], y_delta = y_coords[0], relative = False, move_camera = False, run_time = 1.5*beat_time, x_rate_func = rush_from, y_rate_func = rush_from),
            *[ prelight_torch(torch, run_time = 1.5*beat_time) for torch in torches ],
        )

        for i in range(1,8):
            self.play(
                *move_lightship(x_delta = x_coords[i], y_delta = y_coords[i], relative = False, move_camera = False, run_time = beat_time/3, x_rate_func = rush_from, y_rate_func = rush_from),
                light_torch(torches[i-1], run_time = beat_time/3, rate_func = rush_from)
            )

        self.play(
            *move_lightship(x_delta = x_coords[8], y_delta = y_coords[8], relative = False, move_camera = False, run_time = 1.5*beat_time, x_rate_func = rush_from, y_rate_func = rush_from),
            light_torch(torches[7], run_time = beat_time/3, rate_func = rush_from)
        )

        for i in range(9,16):
            self.play(
                *move_lightship(x_delta = x_coords[i], y_delta = y_coords[i], relative = False, move_camera = False, run_time = beat_time/3, x_rate_func = rush_from, y_rate_func = rush_from),
                light_torch(torches[i-1], run_time = beat_time/3, rate_func = rush_from)
            )

        for i in range(16,22):
            self.play(
                *move_lightship(x_delta = x_coords[i], y_delta = y_coords[i], relative = False, move_camera = False, run_time = 1*beat_time, x_rate_func = rush_from, y_rate_func = rush_from),
                light_torch(torches[i-1], run_time = beat_time/3, rate_func = rush_from),
                FadeOut(Circle(radius = 0.3, color = YELLOW).move_to(torches[i-1][0].get_center()), scale = 100, run_time = beat_time, rate_func = rush_from)
            )

        for i in range(22,number_of_torches):
            self.play(
                *move_lightship(x_delta = x_coords[i], y_delta = y_coords[i], relative = False, move_camera = False, run_time = beat_time/3, x_rate_func = rush_from, y_rate_func = rush_from),
                light_torch(torches[i-1], run_time = beat_time/3, rate_func = rush_from)
            )

        alpha_rate = 2

        # Lighting the network

        number_of_connections = 141
        random.seed(6)
        beginnings = [ random.randint(0, number_of_torches+2) for _ in range(number_of_connections) ]
        endings = [ random.randint(0, number_of_torches+2) for _ in range(number_of_connections) ]
        # connections = [ always_redraw(lambda: Line(torches[beginnings[i]][0].get_center(), torches[endings[i]][0].get_center(), z_index = -1, stroke_width = 10).set_color(YELLOW)) for i in range(number_of_connections) ]
        connections = [ Line(torches[beginnings[i]][0].get_center(), torches[endings[i]][0].get_center(), z_index = -3, stroke_width = 10).set_color(YELLOW) for i in range(number_of_connections) ]
        connection_group = VGroup(*connections)
        # connections = [ Line(torches[random.randint(0,number_of_torches)][0].get_center(), torches[random.randint(0,number_of_torches)][0].get_center(), z_index = -1, stroke_width = 10).set_color(YELLOW) for _ in range(number_of_connections) ]

        def flash_connection (index, run_time, ratio, *args, **kwargs):
            return Succession(
                Create(connections[index], run_time = run_time*(ratio/(1+ratio)), rate_func = linear),
                FadeOut(connections[index], run_time = run_time/(1+ratio), rate_func = rush_from),
                *args,
                **kwargs
            )

        cur_time = 4*beat_time

        theta_tracker_main.set_value(PI+PI/3.9)
        theta_tracker_lag.set_value(PI+PI/3.9)

        self.play(
            *move_lightship(r_delta = 7, theta_delta = -PI/4, r_rate_func = linear, theta_rate_func = linear, move_camera = False, run_time = cur_time),
            light_torch(torches[number_of_torches-1], run_time = beat_time/3, rate_func = rush_from),
            nucleus_scale_tracker.animate(run_time = cur_time, rate_func = linear).set_value(1),
            middle_scale_tracher.animate(run_time = cur_time, rate_func = linear).set_value(1),
            command_scale_tracher.animate(run_time = cur_time, rate_func = linear).set_value(1),
            # *[ torch.animate(run_time = cur_time, rate_func = linear).shift((random.rand()-.5)*RIGHT + (random.rand()-.5)*UP) for torch in torches ],
            LaggedStart(
                *[ flash_connection(i, beat_time, 1/5) for i in range(18) ],
                lag_ratio = 1/6
            )
        )

        self.play(
            # *move_lightship(r_delta = 3, theta_delta = PI/5, r_rate_func = linear, theta_rate_func = linear, move_camera = False, run_time = cur_time),
            *move_lightship(move_camera = False, run_time = cur_time),
            middle_scale_tracher.animate(run_time = cur_time, rate_func = linear).set_value(1),
            command_scale_tracher.animate(run_time = cur_time, rate_func = linear).set_value(1),
            # *[ torch.animate(run_time = cur_time, rate_func = linear).shift((random.rand()-.5)*RIGHT + (random.rand()-.5)*UP) for torch in torches ],
            LaggedStart(
                *[ flash_connection(i, beat_time, 1/5) for i in range(18,36) ],
                lag_ratio = 1/6
            )
        )

        cur_time = 8*beat_time
        alpha_rate = 1

        self.play(
            *move_lightship(move_camera = False, run_time = cur_time),
            self.camera.frame.animate(run_time = cur_time, rate_func = linear).scale(1.1), # pyright: ignore[reportAttributeAccessIssue]
            LaggedStart(
                *[
                    AnimationGroup(
                        Write(connections[i], run_time = beat_time, rate_func = rush_from),
                        Flash(torches[beginnings[i]][0].get_center(), run_time = beat_time/3, rate_func = linear, line_length = 1.2, color = YELLOW),
                        Succession(
                            torches[beginnings[i]][1].animate(run_time = beat_time/3).scale(2),
                            torches[beginnings[i]][1].animate(run_time = 2*beat_time/3).scale(.5),
                        )
                    ) for i in range(38,80)
                ],
                lag_ratio = 1/6
            )
        )

        self.remove(*tracing_paths)
        tracing_paths = [ TracedPath(photons[i][0].get_center, dissipating_time = beat_time, stroke_opacity = [1,1]).set_color(YELLOW_C) for i in range(4) ]
        self.add(*tracing_paths)

        cur_time = beat_time

        self.play(
            *move_lightship(move_camera = False, run_time = cur_time),
            *[
                torch[1].animate(run_time = beat_time, rate_func = rush_from).scale(3)
                for torch in torches
            ],
            *[
                Flash(torch[0].get_center(), run_time = cur_time, line_length = 2, rate_func = rush_from)
                for torch in torches
            ],
            *[
                Write(connections[i], run_time = cur_time, rate_func = rush_from)
                for i in range(80,number_of_connections)
            ]
        )

        def draw_collection ():
            return VGroup(
                *[
                    Line(torches[beginnings[i]][0].get_center(), torches[endings[i]][0].get_center(), z_index = -3, stroke_width = 10).set_color(YELLOW)
                    for i in range(number_of_connections)
                ]
            )

        self.remove(*connections)
        # connections = [ always_redraw(lambda: Line(torches[beginnings[i]][0].get_center(), torches[endings[i]][0].get_center(), z_index = -3, stroke_width = 10).set_color(YELLOW)) for i in range(number_of_connections) ]
        # self.add(*connections)
        collection = always_redraw(draw_collection)
        self.add(collection)

        cur_time = beat_time

        def tilt ():
            self.play(
                *move_lightship(move_camera = False, run_time = cur_time),
                *[
                    torch.animate(run_time = cur_time, rate_func = rush_from).shift(2*(random.random()-.5)*RIGHT + 2*(random.random()-.5)*UP)
                    for torch in torches
                ]
            )

        for _ in range(3):
            tilt()

        self.play(
            *move_lightship(r_delta = 25, theta_delta = -2.1*PI, run_time = 6*beat_time, r_rate_func = rush_from, theta_rate_func = linear, move_camera = False, lag = 0.005),
            self.camera.frame.animate(run_time = 6*beat_time, rate_func = rush_from).scale(2).shift(10*DOWN), # pyright: ignore[reportAttributeAccessIssue]
            *[
                torch.animate(run_time = cur_time, rate_func = rush_from).shift(2*(random.random()-.5)*RIGHT + 2*(random.random()-.5)*UP)
                for torch in torches
            ]
            # *[ send_pulse(i, run_time = 2*beat_time) for i in range(80,100) ]
        )

        self.wait(1)


    def construct(self):
        self.part_one()
