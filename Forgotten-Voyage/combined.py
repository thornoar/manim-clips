from common import *
from numpy import arctan2, random

def combine (scene: Scene):
    scene.add_sound("./final.mp3")

    initial_wait = 0.140
    beat_time = 0.515

    # photon_lag_ratio = 0.001

    photon_dispersion = 1.06
    photon_number = 100
    photon_radius = 1.5
    photon_opacity = 0.2
    # photon_dispersion = 1.00001
    # photon_number = 1
    # photon_radius = .1
    # photon_opacity = 1.0

    nucleus_number = 200
    nucleus_radius = 4.5
    nucleus_dispersion = 1.041
    nucleus_opacity = 0.2
    # nucleus_number = 2
    # nucleus_radius = 4.5
    # nucleus_dispersion = 3
    # nucleus_opacity = 0.2

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

    scene.add(*dots)
    scene.add(*rotating_squares)
    scene.add(*tracing_paths)

    scene.wait(initial_wait)
    scene.play(
        *[ transform_succession(rotating_squares[i], rotating_square_transforms[i], beat_time) for i in range(number_of_squares) ],
        transform_succession(beat_square, beat_squares, beat_time, rate_func = lambda i: rush_from if (i % 2 == 1) else rush_into),
        r_tracker.animate(rate_func = rush_from, run_time = number_of_cycles*beat_time).set_value(s),
        theta_tracker.animate(rate_func = rush_from, run_time = number_of_cycles*beat_time).set_value(2*PI),
    )

    # Building the shell

    for dot in dots:
        dot.clear_updaters()

    square_color = colors[number_of_cycles-1]
    circle_color = BLUE
    photons = [create_glow(dot, rad = photon_radius, num = photon_number, dispersion = photon_dispersion, col = YELLOW_C, z_index = 100, opacity = photon_opacity) for dot in dots]
    scene.remove(*dots)
    scene.remove(beat_square)
    scene.remove(*tracing_paths)
    scene.play(
        *[FadeIn(photon, scale = 2, run_time = beat_time, rate_func = rush_from) for photon in photons],
        *[Flash(dot, run_time = beat_time, rate_func = linear) for dot in dots]
    )
    tracing_paths = [ TracedPath(photons[i][0].get_center, dissipating_time = beat_time/2, stroke_opacity = [1,1]).set_color(YELLOW_C) for i in range(4) ]
    scene.add(*tracing_paths)

    photons[1].add_updater(lambda m: m.move_to(rotating_squares[1].height/2 * UP))
    photons[3].add_updater(lambda m: m.move_to(rotating_squares[1].height/2 * DOWN))
    circle1 = Circle(color = YELLOW, radius = .15).move_to(photons[0])
    circle2 = Circle(color = YELLOW, radius = .15).move_to(photons[2])
    scene.play(
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
    scene.play(
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

    scene.play(
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

    scene.play(
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

    scene.play(
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
    scene.play(
        middle_layer.animate.scale(zoom_factor),
        outer_layer.animate.scale(zoom_factor),
        *[Flash(photon[0], run_time = beat_time, rate_func = rush_from) for photon in photons],
        run_time = beat_time,
        rate_func = rush_from
    )

    # Building the core

    c = 1.2
    scene.play(
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

    scene.play(
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

    scene.play(
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

    scene.play(
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
    scene.remove(line1p, line2p, line3p)

    scene.play(
        Flash(photons[1][0], run_time = beat_time, rate_func = rush_from),
        Flash(photons[2][0], run_time = beat_time, rate_func = rush_from),
        Flash(photons[3][0], run_time = beat_time, rate_func = rush_from),
        FadeOut(line1.copy(), shift = .5*dir(PI/6), run_time = beat_time, rate_func = rush_from),
        FadeOut(line2.copy(), shift = .5*dir(PI/6+2*PI/3), run_time = beat_time, rate_func = rush_from),
        FadeOut(line3.copy(), shift = .5*dir(PI/6+4*PI/3), run_time = beat_time, rate_func = rush_from),
        Succession(FadeIn(circle, run_time = .1*beat_time), FadeOut(circle, scale = 30, run_time = .9*beat_time, rate_func = rush_from)),
    )
    scene.play(
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

    scene.play(
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

    scene.play(
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
    scene.remove(core_circle_big_copy, core_circle_small_copy)

    core_layer = VGroup(core_circle_big, core_circle_small, core_triangle)
    lightship = VGroup(outer_layer, middle_layer, core_layer, center)

    zoom_factor = 0.6
    s *= zoom_factor
    c *= zoom_factor

    scene.play(
        lightship.animate.scale(zoom_factor),
        photons[0].animate(path_arc = 2*PI/3).scale(1/(1.5**3)).move_to(4*RIGHT+1.8*UP),
        photons[1].animate(path_arc = 2*PI/3).move_to(4.8*LEFT+0.9*UP),
        photons[2].animate(path_arc = 2*PI/3).move_to(4.2*LEFT+2.1*DOWN),
        photons[3].animate(path_arc = 2*PI/3).move_to(4.7*RIGHT+1.7*DOWN),
        run_time = 2*beat_time,
        rate_func = rush_from
    )

    # Starting the lightship

    number_of_cycles = 2
    cur_time = number_of_cycles * beat_time

    def radiate (num, intense = False, respond = False, focus = False):
        factor = 1.01 if intense else 1.005
        for _ in range(num):
            scene.play(
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
    scene.play(
        photons[0].animate(path_arc = PI/2).shift(.5*(LEFT+UP)),
        photons[1].animate(path_arc = PI/7).shift(.2*(RIGHT+UP)),
        photons[2].animate(path_arc = -2*PI/3).shift(.7*RIGHT + .3*DOWN),
        photons[3].animate(path_arc = PI/4).shift(.5*LEFT+DOWN),
        run_time = beat_time,
        rate_func = rush_from
    )
    scene.play(
        *[
            FadeOut(Circle(radius = 0.1, color = YELLOW_C).move_to(photons[i]), scale = 10, run_time = beat_time, rate_func = rush_from)
            for i in range(4)
        ]
    )

    radiate(2)

    pos = [photon[0].get_center() for photon in photons]
    scene.play(
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
    scene.play(
        photons[0].animate(path_arc = -PI/5).shift(.4*(LEFT+UP)),
        photons[1].animate(path_arc = PI/1).shift(.2*RIGHT),
        photons[2].animate(path_arc = -2*PI/3).shift(.2*RIGHT + .3*DOWN),
        photons[3].animate(path_arc = PI/4).shift(.3*LEFT+.1*UP),
        run_time = beat_time,
        rate_func = rush_from
    )
    scene.play(
        *[
            FadeOut(Circle(radius = 0.1, color = YELLOW_C).move_to(photons[i]), scale = 10, run_time = beat_time, rate_func = rush_from)
            for i in range(4)
        ]
    )

    radiate(2)

    circles = [ Circle(radius = 0.05, color = YELLOW_C).move_to(photons[i][0]) for i in range(len(photons)) ]
    scales = [20,15,25,30]

    scene.play(
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
    scene.play(
        *[
            FadeOut(Circle(radius = 0.1, color = YELLOW_C).move_to(photons[i]), scale = 10, run_time = beat_time, rate_func = rush_from)
            for i in range(4)
        ]
    )

    cur_time = 4*beat_time

    zoom_factor = 1.2
    scene.play(
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

    nucleus = create_glow(center, rad = nucleus_radius, num = nucleus_number, col = YELLOW, dispersion = nucleus_dispersion, opacity = nucleus_opacity, z_index = 2)

    scene.play(
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

    scene.play(
        Flash(ORIGIN, color = YELLOW, num_lines = 30, flash_radius = 0.1*c/2, line_length = 0.8*c/2, rate_func = rush_from),
        *[ Flash(photon[0], run_time = beat_time, rate_func = rush_from) for photon in photons ],
        photons[0].animate(path_arc = PI/3, rate_func = rush_from).move_to(pos[1]),
        photons[1].animate(path_arc = PI/3, rate_func = rush_from).move_to(pos[0]),
        photons[2].animate(path_arc = PI/3, rate_func = rush_from).move_to(pos[3]),
        photons[3].animate(path_arc = PI/3, rate_func = rush_from).move_to(pos[2]),
        Rotate(middle_layer, -PI/4, rate_func = linear),
        Rotate(core_layer, PI/3, rate_func = linear),
        run_time = beat_time,
    )

    scene.play(
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

    scene.play(
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

    scene.remove(*tracing_paths)
    tracing_paths = [ TracedPath(photons[i][0].get_center, dissipating_time = 2*beat_time, stroke_opacity = [1,1]).set_color(YELLOW_C) for i in range(4) ]
    scene.add(*tracing_paths)

    outer_group = VGroup(outer_layer)
    photon_group = VGroup(*photons)

    cur_time = 1.5*beat_time

    scene.play(
        Flash(ORIGIN, color = YELLOW, num_lines = 30, flash_radius = 0.1*c/2, line_length = 0.8*c/2, rate_func = rush_from),
        Flash(core_circle_small, color = YELLOW, num_lines = 30, flash_radius = c/2 + 0.1*c/2, line_length = 0.8*c/2, rate_func = rush_from),
        Flash(core_circle_big, color = YELLOW, num_lines = 30, flash_radius = c + 0.1*(s-c), line_length = 0.8*(s-c), rate_func = rush_from),
        Rotate(core_layer, cur_time/beat_time*PI/3 + PI/6, rate_func = linear),
        Rotate(middle_layer, -cur_time/beat_time*PI/4 - PI/8, rate_func = linear),
        Rotate(outer_group, cur_time/beat_time*PI, about_point = ORIGIN, rate_func = linear),

        Rotate(photon_group, cur_time/beat_time*PI, about_point = ORIGIN, rate_func = linear),

        run_time = cur_time
    )

    core_group = core_layer
    # nucleus_group = VGroup(nucleus)
    # core_group.add(nucleus)
    middle_group = middle_layer

    nucleus_scale_tracker = ValueTracker(1.0)
    nucleus_dispersion_scale_tracker = ValueTracker(1.0)
    nucleus_shift_tracker = ValueTracker(0.0)

    r_tracker_main = ValueTracker(0)
    theta_tracker_main = ValueTracker(PI/2)
    r_tracker_lag = ValueTracker(0)
    theta_tracker_lag = ValueTracker(PI/2)
    x_tracker_main = ValueTracker(0)
    y_tracker_main = ValueTracker(0)
    x_tracker_lag = ValueTracker(0)
    y_tracker_lag = ValueTracker(0)
    alpha_tracker = ValueTracker(0)
    middle_scale_tracker = ValueTracker(1.0)
    command_scale_tracker = ValueTracker(1.0)

    # def update_nucleus (m):
    #     m.restore()
    #     return m.scale(nucleus_scale_tracker.get_value()).shift(r_tracker_main.get_value()*dir(theta_tracker_main.get_value())).shift(x_tracker_main.get_value()*RIGHT + y_tracker_main.get_value()*UP)

    def update_nucleus (m):
        m.restore()
        scale_glow(m, disp_scale = nucleus_dispersion_scale_tracker.get_value())
        m.scale(nucleus_scale_tracker.get_value())
        m.shift(nucleus_shift_tracker.get_value() * RIGHT)
        return m.shift(r_tracker_main.get_value()*dir(theta_tracker_main.get_value())).shift(x_tracker_main.get_value()*RIGHT + y_tracker_main.get_value()*UP)

    # for i, circle in enumerate(nucleus):
    #     circle.add_updater(lambda m: m.shift(r_tracker_main.get_value()*dir(theta_tracker_main.get_value())).shift(x_tracker_main.get_value()*RIGHT + y_tracker_main.get_value()*UP))

    def update_core (m):
        m.restore()
        # return m.shift(r_tracker_main.get_value()*dir(theta_tracker_main.get_value())).shift(x_tracker_main.get_value()*RIGHT + y_tracker_main.get_value()*UP)
        return m.rotate(alpha_tracker.get_value()/3, about_point = ORIGIN).shift(r_tracker_main.get_value()*dir(theta_tracker_main.get_value())).shift(x_tracker_main.get_value()*RIGHT + y_tracker_main.get_value()*UP)

    def update_middle (m):
        m.restore()
        # return m.scale(middle_scale_tracker.get_value()).shift(r_tracker_main.get_value()*dir(theta_tracker_main.get_value())).shift(x_tracker_main.get_value()*RIGHT + y_tracker_main.get_value()*UP)
        return m.rotate(-alpha_tracker.get_value()/4, about_point = ORIGIN).scale(middle_scale_tracker.get_value()).shift(r_tracker_main.get_value()*dir(theta_tracker_main.get_value())).shift(x_tracker_main.get_value()*RIGHT + y_tracker_main.get_value()*UP)

    def update_command (m):
        m.restore()
        return m.rotate(alpha_tracker.get_value(), about_point = ORIGIN).scale(command_scale_tracker.get_value()).shift(r_tracker_lag.get_value()*dir(theta_tracker_lag.get_value())).shift(x_tracker_lag.get_value()*RIGHT + y_tracker_lag.get_value()*UP)

    def update_photons (m):
        m.restore()
        return m.rotate(alpha_tracker.get_value(), about_point = ORIGIN).shift(r_tracker_lag.get_value()*dir(theta_tracker_lag.get_value())).shift(x_tracker_lag.get_value()*RIGHT + y_tracker_lag.get_value()*UP)

    nucleus.save_state()
    nucleus.add_updater(update_nucleus)
    core_group.save_state()
    core_group.add_updater(update_core)
    middle_group.save_state()
    middle_group.add_updater(update_middle)
    outer_group.save_state()
    outer_group.add_updater(update_command)
    photon_group.save_state()
    photon_group.add_updater(update_photons)

    # photons[0].add_updater(lambda m: m.move_to(r_tracker_lag.get_value()*dir(theta_tracker_lag.get_value()) + x_tracker_lag.get_value()*RIGHT + y_tracker_lag.get_value()*UP))

    alpha_rate = 1

    def move_lightship (r_delta = 0.0, theta_delta = 0.0, x_delta = 0.0, y_delta = 0.0, run_time = beat_time, r_rate_func = smooth, theta_rate_func = smooth, x_rate_func = smooth, y_rate_func = smooth, move_camera = True, frame_rate_func = linear, lag = 0.03, relative = True):
        r_new  = r_tracker_main.get_value() + r_delta if relative else r_delta
        theta_new  = theta_tracker_main.get_value() + theta_delta if relative else theta_delta
        x_new = x_tracker_main.get_value() + x_delta if relative else x_delta
        y_new = y_tracker_main.get_value() + y_delta if relative else y_delta
        return [
            *([
                scene.camera.frame.animate(run_time = run_time, rate_func = frame_rate_func).move_to(r_new*dir(theta_new) + x_new*RIGHT + y_new*UP) # pyright: ignore[reportAttributeAccessIssue]
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

    scene.play(
        *move_lightship(r_delta = 1.4, theta_delta = 3*PI/2, run_time = 2.8*beat_time, r_rate_func = rush_from, theta_rate_func = rush_from, move_camera = True),
    )

    # Lighting torches

    torch_radius = 3.5
    torch_number = 70
    torch_dispersion = 1.14
    torch_opacity = 0.4

    torch_radius = 0.3
    def create_torch (pos):
        return VGroup(
            Circle(radius = torch_radius).set_color(ORANGE).move_to(pos),
            create_glow(Dot(pos), rad = torch_radius, num = torch_number, dispersion = torch_dispersion, opacity = torch_opacity)
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
    scene.play(
        *move_lightship(x_delta = vec[0], y_delta = vec[1], run_time = 1.1*beat_time, x_rate_func = rush_from, y_rate_func = rush_from, frame_rate_func = linear),
        prelight_torch(torch1, run_time = 1.1*beat_time, rate_func = linear)
    )
    scene.play(
        *move_lightship(r_delta = 0.0, theta_delta = PI/2.5, run_time = 1.1*beat_time, r_rate_func = linear, theta_rate_func = linear, frame_rate_func = linear),
        light_torch(torch1, run_time = 1.1*beat_time, rate_func = rush_from)
    )
    vec = 7*dir(theta_tracker_main.get_value()+PI/2)
    torch2 = create_torch(get_current_pos() + vec)
    scene.play(
        *move_lightship(x_delta = vec[0], y_delta = vec[1], run_time = 1*beat_time, x_rate_func = rush_from, y_rate_func = rush_from, frame_rate_func = linear),
        prelight_torch(torch2, run_time = 1.1*beat_time, rate_func = linear)
    )
    scene.play(
        *move_lightship(r_delta = 0.0, theta_delta = PI/2.5, run_time = 1*beat_time, r_rate_func = linear, theta_rate_func = linear, frame_rate_func = linear),
        light_torch(torch2, run_time = 1.1*beat_time, rate_func = rush_from)
    )

    vec = 6*dir(theta_tracker_main.get_value()+PI/2)
    scene.play(
        *move_lightship(x_delta = vec[0], y_delta = vec[1], run_time = 1*beat_time, x_rate_func = linear, y_rate_func = linear, move_camera = False),
        scene.camera.frame.animate(rate_func = linear, run_time = beat_time).shift(-5*RIGHT).scale(2.5), # pyright: ignore[reportAttributeAccessIssue]
    )
    scene.play(
        *move_lightship(x_delta = 17.9, y_delta = -6.1, run_time = 1*beat_time, x_rate_func = linear, y_rate_func = linear, move_camera = False),
        r_tracker_main.animate(run_time = beat_time).set_value(0),
        r_tracker_lag.animate(run_time = beat_time).set_value(0),
        scene.camera.frame.animate(rate_func = linear, run_time = beat_time).shift(-5*RIGHT).scale(2.5), # pyright: ignore[reportAttributeAccessIssue]
    )

    cur_time = 1.5*beat_time
    scene.play(
        # *move_lightship(move_camera = False, run_time = 1.5*beat_time),
        alpha_tracker.animate.increment_value(alpha_rate * PI * cur_time/beat_time),
        middle_scale_tracker.animate.set_value(0.7),
        command_scale_tracker.animate.set_value(1.2),
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

    scene.play(
        *move_lightship(x_delta = x_coords[0], y_delta = y_coords[0], relative = False, move_camera = False, run_time = 1.5*beat_time, x_rate_func = rush_from, y_rate_func = rush_from),
        *[ prelight_torch(torch, run_time = 1.5*beat_time) for torch in torches ],
    )

    for i in range(1,8):
        scene.play(
            *move_lightship(x_delta = x_coords[i], y_delta = y_coords[i], relative = False, move_camera = False, run_time = beat_time/3, x_rate_func = rush_from, y_rate_func = rush_from),
            light_torch(torches[i-1], run_time = beat_time/3, rate_func = rush_from)
        )

    scene.play(
        *move_lightship(x_delta = x_coords[8], y_delta = y_coords[8], relative = False, move_camera = False, run_time = 1.5*beat_time, x_rate_func = rush_from, y_rate_func = rush_from),
        light_torch(torches[7], run_time = beat_time/3, rate_func = rush_from)
    )

    for i in range(9,16):
        scene.play(
            *move_lightship(x_delta = x_coords[i], y_delta = y_coords[i], relative = False, move_camera = False, run_time = beat_time/3, x_rate_func = rush_from, y_rate_func = rush_from),
            light_torch(torches[i-1], run_time = beat_time/3, rate_func = rush_from)
        )

    for i in range(16,22):
        scene.play(
            *move_lightship(x_delta = x_coords[i], y_delta = y_coords[i], relative = False, move_camera = False, run_time = 1*beat_time, x_rate_func = rush_from, y_rate_func = rush_from),
            light_torch(torches[i-1], run_time = beat_time/3, rate_func = rush_from),
            FadeOut(Circle(radius = 0.3, color = YELLOW).move_to(torches[i-1][0].get_center()), scale = 100, run_time = beat_time, rate_func = rush_from)
        )

    for i in range(22,number_of_torches):
        scene.play(
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
    # connection_group = VGroup(*connections)
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

    scene.play(
        *move_lightship(r_delta = 7, theta_delta = -PI/4, r_rate_func = linear, theta_rate_func = linear, move_camera = False, run_time = cur_time),
        light_torch(torches[number_of_torches-1], run_time = beat_time/3, rate_func = rush_from),
        nucleus_scale_tracker.animate(run_time = cur_time, rate_func = linear).set_value(1),
        middle_scale_tracker.animate(run_time = cur_time, rate_func = linear).set_value(1),
        command_scale_tracker.animate(run_time = cur_time, rate_func = linear).set_value(1),
        # *[ torch.animate(run_time = cur_time, rate_func = linear).shift((random.rand()-.5)*RIGHT + (random.rand()-.5)*UP) for torch in torches ],
        LaggedStart(
            *[ flash_connection(i, beat_time, 1/5) for i in range(18) ],
            lag_ratio = 1/6
        )
    )

    scene.play(
        # *move_lightship(r_delta = 3, theta_delta = PI/5, r_rate_func = linear, theta_rate_func = linear, move_camera = False, run_time = cur_time),
        *move_lightship(move_camera = False, run_time = cur_time),
        middle_scale_tracker.animate(run_time = cur_time, rate_func = linear).set_value(1),
        command_scale_tracker.animate(run_time = cur_time, rate_func = linear).set_value(1),
        # *[ torch.animate(run_time = cur_time, rate_func = linear).shift((random.rand()-.5)*RIGHT + (random.rand()-.5)*UP) for torch in torches ],
        LaggedStart(
            *[ flash_connection(i, beat_time, 1/5) for i in range(18,36) ],
            lag_ratio = 1/6
        )
    )

    cur_time = 8*beat_time
    # alpha_rate = 1

    scene.play(
        *move_lightship(move_camera = False, run_time = cur_time),
        scene.camera.frame.animate(run_time = cur_time, rate_func = linear).scale(1.1), # pyright: ignore[reportAttributeAccessIssue]
        LaggedStart(
            *[
                AnimationGroup(
                    Write(connections[i], run_time = beat_time, rate_func = rush_from),
                    Flash(torches[beginnings[i]][0].get_center(), run_time = beat_time/3, rate_func = linear, line_length = 1.2, color = YELLOW),
                    # Succession(
                    #     torches[beginnings[i]][1].animate(run_time = beat_time/3).scale(1.5),
                    #     torches[beginnings[i]][1].animate(run_time = 2*beat_time/3).scale(.5),
                    # )
                    torches[beginnings[i]][1].animate(run_time = beat_time/3).scale(1.5),
                ) for i in range(36,78)
            ],
            lag_ratio = 1/6
        )
    )

    scene.remove(*tracing_paths)
    tracing_paths = [ TracedPath(photons[i][0].get_center, dissipating_time = beat_time, stroke_opacity = [1,1]).set_color(YELLOW_C) for i in range(4) ]
    scene.add(*tracing_paths)

    cur_time = beat_time

    scene.play(
        *move_lightship(move_camera = False, run_time = cur_time),
        *[
            torch[1].animate(run_time = beat_time, rate_func = rush_from).scale(2)
            for torch in torches
        ],
        *[
            Flash(torch[0].get_center(), run_time = cur_time, line_length = 2, rate_func = rush_from)
            for torch in torches
        ],
        *[
            Write(connections[i], run_time = cur_time, rate_func = rush_from)
            for i in range(78,number_of_connections)
        ],
        *[
            Write(connections[i], run_time = cur_time, rate_func = rush_from)
            for i in range(36)
        ]
    )

    def draw_collection ():
        return VGroup(
            *[
                Line(torches[beginnings[i]][0].get_center(), torches[endings[i]][0].get_center(), z_index = -3, stroke_width = 10).set_color(YELLOW)
                for i in range(number_of_connections)
            ]
        )

    scene.remove(*connections)
    # connections = [ always_redraw(lambda: Line(torches[beginnings[i]][0].get_center(), torches[endings[i]][0].get_center(), z_index = -3, stroke_width = 10).set_color(YELLOW)) for i in range(number_of_connections) ]
    # scene.add(*connections)
    collection = always_redraw(draw_collection)
    scene.add(collection)

    def move_network (flash = True):
        return AnimationGroup(
            *[
                torch.animate(run_time = beat_time, rate_func = rush_from).shift(10*(random.random()-.5)*RIGHT + 5*(random.random()-.5)*UP)
                for torch in torches
            ],
            *([
                Flash(torch[0], run_time = beat_time, line_length = 2, rate_func = rush_from)
                for torch in torches
            ] if flash else [])
        )

    cur_time = beat_time

    def tilt ():
        scene.play(
            *move_lightship(move_camera = False, run_time = cur_time),
            move_network()
        )

    for _ in range(3):
        tilt()

    scene.play(
        *move_lightship(r_delta = 15, theta_delta = -1.9*PI, run_time = 4*beat_time, r_rate_func = rush_from, theta_rate_func = linear, move_camera = False, lag = 0.005),
        scene.camera.frame.animate(run_time = 4*beat_time, rate_func = rush_from).scale(1.6).shift(5*DOWN), # pyright: ignore[reportAttributeAccessIssue]
        Succession(
            *[
                move_network(False)
                for _ in range(4)
            ]
        ),
        *[
            Flash(torch[0], run_time = beat_time, line_length = 2, rate_func = rush_from)
            for torch in torches
        ]
        # *[ send_pulse(i, run_time = 2*beat_time) for i in range(80,100) ]
    )

    cur_time = 4*beat_time

    scene.play(
        *move_lightship(r_delta = 85, theta_delta = -.2*PI, run_time = cur_time, r_rate_func = rush_from, theta_rate_func = rush_from, lag = 0.003),
        Succession(
            *[
                move_network(False)
                for _ in range(4)
            ]
        ),
        *[
            Flash(torch[0], run_time = beat_time, line_length = 2, rate_func = rush_from)
            for torch in torches
        ]
    )

    cur_time = beat_time

    scene.play(
        *move_lightship(r_delta = 20, theta_delta = 0.03*PI, r_rate_func = rush_into, theta_rate_func = rush_from, move_camera = False, lag = 0.005),
        scene.camera.frame.animate(run_time = cur_time, rate_func = linear).shift(30*LEFT).shift(5*UP).scale(0.8) # pyright: ignore[reportAttributeAccessIssue]
    )
    scene.play(
        *move_lightship(r_delta = 20, theta_delta = -0.03*PI, r_rate_func = rush_from, theta_rate_func = rush_into, move_camera = False, lag = 0.005),
        scene.camera.frame.animate(run_time = cur_time, rate_func = linear).shift(30*LEFT).shift(5*UP).scale(0.8) # pyright: ignore[reportAttributeAccessIssue]
    )
    cur_time = 1.5*beat_time
    scene.play(
        *move_lightship(r_delta = 50, theta_delta = 0.01*PI, run_time = cur_time, r_rate_func = rush_into, theta_rate_func = rush_from, move_camera = False, lag = 0.005),
        scene.camera.frame.animate(run_time = cur_time, rate_func = linear).shift(30*LEFT).shift(5*UP).scale(0.8) # pyright: ignore[reportAttributeAccessIssue]
    )

    scene.remove(*connections, collection, *torches)

    cur_time = 1.5*beat_time
    scene.play(
        *move_lightship(run_time = cur_time)
    )

    scene.remove(*tracing_paths)

    center = nucleus[0].get_center()
    lightship.move_to(ORIGIN)
    scene.camera.frame.move_to(ORIGIN) # pyright: ignore[reportAttributeAccessIssue]
    r_tracker_main.set_value(0)
    r_tracker_lag.set_value(0)
    theta_tracker_main.set_value(0)
    theta_tracker_lag.set_value(0)
    x_tracker_main.set_value(0)
    x_tracker_lag.set_value(0)
    y_tracker_main.set_value(0)
    y_tracker_lag.set_value(0)

    # --------------- PART 2 ---------------

    # Encountering the black domain

    outer_group.remove(*photons)

    black_domain_size = 9
    black_domain_pos_num = 20
    black_domain_pos = black_domain_pos_num*LEFT
    black_domain = Circle(color = BLACK, fill_color = BLACK, fill_opacity = 1.0, radius = black_domain_size, z_index = 2).shift(black_domain_pos)

    scene.add(black_domain)

    domain_fade_circle = Circle(color = RED, radius = black_domain_size, stroke_width = 20, z_index = 10).shift(black_domain_pos)

    cur_time = 3*beat_time
    scene.play(
        *move_lightship(run_time = cur_time),
        # nucleus_dispersion_scale_tracker.animate(run_time = cur_time, rate_func = rush_into).increment_value(0.00001),
        nucleus_scale_tracker.animate(run_time = cur_time, rate_func = rush_into).increment_value(3.5),
        scene.camera.frame.animate(run_time = cur_time, rate_func = linear).scale(0.85) # pyright: ignore[reportAttributeAccessIssue]
    )

    scale = 0.95

    cur_time = beat_time
    scene.play(
        *move_lightship(run_time = cur_time, move_camera = False),
        FadeOut(Circle(color = YELLOW, radius = 0.2), scale = 100, rate_func = rush_from, run_time = cur_time),
        scene.camera.frame.animate(run_time = cur_time, rate_func = linear).shift(2*LEFT) # pyright: ignore[reportAttributeAccessIssue]
    )
    for _ in range(3):
        scene.play(
            *move_lightship(run_time = cur_time, move_camera = False),
            # FadeOut(Circle(color = RED, radius = black_domain_size, z_index = 1).shift(black_domain_pos), scale = 0.05, rate_func = rush_from, run_time = cur_time)
            
            Succession(FadeIn(domain_fade_circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(domain_fade_circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)),
            # FadeOut(domain_fade_circle, scale = 0.05, rate_func = rush_from, run_time = cur_time)
            scene.camera.frame.animate(run_time = cur_time, rate_func = linear).shift(2*LEFT) # pyright: ignore[reportAttributeAccessIssue]
        )

    photon_group.clear_updaters()

    alpha_trackerp = ValueTracker(-PI/4)

    outer_group.clear_updaters()
    def update_outer_p (m):
        m.restore()
        return m.rotate(alpha_trackerp.get_value(), about_point = ORIGIN).scale(command_scale_tracker.get_value()).shift(r_tracker_lag.get_value()*dir(theta_tracker_lag.get_value())).shift(x_tracker_lag.get_value()*RIGHT + y_tracker_lag.get_value()*UP)
    # outer_group.save_state()
    outer_group.add_updater(update_outer_p)

    # domain_cage_dist = black_domain_size * (sqrt(2)-1)
    domain_cage_dist = black_domain_size * 0.6
    cage_circle_size = black_domain_size + domain_cage_dist

    bait_position1 = 1*LEFT+11*UP
    bait_position2 = 4*RIGHT+8*UP
    bait_position3 = 7*RIGHT+6*UP
    bait_position1p = 1*RIGHT+7*DOWN
    bait_position2p = 3*RIGHT+9*DOWN
    bait_position3p = 9*RIGHT+6*DOWN

    tracing_paths = [ TracedPath(photons[i][0].get_center, dissipating_time = beat_time/2, stroke_width = 7, z_index = 50, stroke_opacity = [1,1]).set_color(YELLOW_C) for i in range(4) ]
    scene.add(*tracing_paths)

    cur_time = beat_time
    scene.play(
        *move_lightship(run_time = cur_time, move_camera = False),
        photons[0].animate(run_time = cur_time, rate_func = rush_from).move_to(black_domain_pos+black_domain_size*RIGHT+domain_cage_dist*RIGHT).scale(3),
        photons[1].animate(run_time = cur_time, rate_func = rush_from).move_to(black_domain_pos+black_domain_size*RIGHT+domain_cage_dist*RIGHT).scale(3),
        photons[2].animate(run_time = cur_time, rate_func = rush_from).move_to(bait_position1).scale(3),
        photons[3].animate(run_time = cur_time, rate_func = rush_from).move_to(bait_position1p).scale(3),
    )

    cage_arc1 = Arc(radius = black_domain_size+domain_cage_dist, start_angle = 0, angle = PI, color = YELLOW, stroke_width = 10).shift(black_domain_pos)
    cage_arc2 = Arc(radius = black_domain_size+domain_cage_dist, start_angle = 0, angle = -PI, color = YELLOW, stroke_width = 10).shift(black_domain_pos)
    cage_arc1_copy = cage_arc1.copy()
    cage_arc2_copy = cage_arc2.copy()

    cur_time = 4*beat_time

    arc_circles1 = [Circle(radius = .15, color = YELLOW).move_to(black_domain_pos+(black_domain_size+domain_cage_dist)*dir(i*PI/4)) for i in range(0,4)]
    arc_circles2 = [Circle(radius = .15, color = YELLOW).move_to(black_domain_pos+(black_domain_size+domain_cage_dist)*dir(-i*PI/4)) for i in range(0,4)]
    bait_circle1 = Circle(radius =  .4, color = YELLOW).move_to(bait_position1)
    bait_circle2 = Circle(radius =  .4, color = YELLOW).move_to(bait_position2)
    bait_circle3 = Circle(radius =  .4, color = YELLOW).move_to(bait_position3)
    bait_circle1p = Circle(radius = .4, color = YELLOW).move_to(bait_position1p)
    bait_circle2p = Circle(radius = .4, color = YELLOW).move_to(bait_position2p)
    bait_circle3p = Circle(radius = .4, color = YELLOW).move_to(bait_position3p)

    alpha_rate = 1.6

    lightship_shift = domain_cage_dist+3

    # nucleus.clear_updaters()

    scene.play(
        MoveAlongPath(photons[0], cage_arc1_copy, run_time = cur_time, rate_func = linear),
        MoveAlongPath(photons[1], cage_arc2_copy, run_time = cur_time, rate_func = linear),
        Create(cage_arc1, run_time = cur_time, rate_func = linear),
        Create(cage_arc2, run_time = cur_time, rate_func = linear),
        black_domain.animate(run_time = cur_time, rate_func = rush_into).shift(domain_cage_dist*RIGHT),
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 10, run_time = .9*beat_time, rate_func = rush_from)) for circle in arc_circles1 ], lag_ratio = 1.0),
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 10, run_time = .9*beat_time, rate_func = rush_from)) for circle in arc_circles2 ], lag_ratio = 1.0),
        LaggedStart(
            MoveAlongPath(photons[2], Line(bait_position1, bait_position1), run_time = beat_time, rate_func = rush_from),
            MoveAlongPath(photons[2], Line(bait_position1, bait_position2), run_time = beat_time, rate_func = rush_from),
            Wait(beat_time),
            MoveAlongPath(photons[2], Line(bait_position2, bait_position3), run_time = beat_time, rate_func = rush_from),
            lag_ratio = 1.0
        ),
        LaggedStart(
            MoveAlongPath(photons[3], Line(bait_position1p, bait_position1p), run_time = beat_time, rate_func = rush_from),
            MoveAlongPath(photons[3], Line(bait_position1p, bait_position2p), run_time = beat_time, rate_func = rush_from),
            Wait(beat_time),
            MoveAlongPath(photons[3], Line(bait_position2p, bait_position3p), run_time = beat_time, rate_func = rush_from),
            lag_ratio = 1.0
        ),
        LaggedStart(
            Succession( FadeIn(bait_circle1, run_time = .1*beat_time, rate_func = rush_from), FadeOut(bait_circle1, scale = 10, run_time = .9*beat_time, rate_func = rush_from)),
            Wait(beat_time),
            Succession( FadeIn(bait_circle2, run_time = .1*beat_time, rate_func = rush_from), FadeOut(bait_circle2, scale = 10, run_time = .9*beat_time, rate_func = rush_from)),
            Wait(beat_time),
            # Succession( FadeIn(bait_circle3, run_time = .1*beat_time, rate_func = rush_from), FadeOut(bait_circle2, scale = 10, run_time = .9*beat_time, rate_func = rush_from)),
            lag_ratio = 1.0
        ),
        LaggedStart(
            Succession( FadeIn(bait_circle1p, run_time = .1*beat_time, rate_func = rush_from), FadeOut(bait_circle1p, scale = 10, run_time = .9*beat_time, rate_func = rush_from)),
            Wait(beat_time),
            Succession( FadeIn(bait_circle2p, run_time = .1*beat_time, rate_func = rush_from), FadeOut(bait_circle2p, scale = 10, run_time = .9*beat_time, rate_func = rush_from)),
            Wait(beat_time),
            # Succession( FadeIn(bait_circle3, run_time = .1*beat_time, rate_func = rush_from), FadeOut(bait_circle2, scale = 10, run_time = .9*beat_time, rate_func = rush_from)),
            lag_ratio = 1.0
        ),
        *move_lightship(x_delta = lightship_shift, x_rate_func = rush_into, move_camera = False, run_time = cur_time),
        # nucleus_dispersion_scale_tracker.animate(run_time = cur_time, rate_func = rush_into).increment_value(0.0001),
        nucleus_scale_tracker.animate(run_time = cur_time, rate_func = rush_into).increment_value(3),
        # scale_glow_animation(nucleus, disp_scale = 1.01, abs_scale = 1.5, shift = lightship_shift*RIGHT, run_time = cur_time, rate_func = rush_into),
    )

    square_circles_1 = [Circle(radius = .15, color = YELLOW).move_to(black_domain_pos+cage_circle_size*dir(PI+i*PI/2)) for i in range(0,3)]
    square_circles_2 = [Circle(radius = .15, color = YELLOW).move_to(black_domain_pos+cage_circle_size*dir(PI-i*PI/2)) for i in range(0,3)]

    line_1_1 = Line(black_domain_pos + cage_circle_size*LEFT, black_domain_pos + cage_circle_size*UP, color = YELLOW, stroke_width = 10)
    line_1_2 = Line(black_domain_pos + cage_circle_size*UP, black_domain_pos + cage_circle_size*RIGHT, color = YELLOW, stroke_width = 10)
    line_2_1 = Line(black_domain_pos + cage_circle_size*LEFT, black_domain_pos + cage_circle_size*DOWN, color = YELLOW, stroke_width = 10)
    line_2_2 = Line(black_domain_pos + cage_circle_size*DOWN, black_domain_pos + cage_circle_size*RIGHT, color = YELLOW, stroke_width = 10)
    line_1_1_copy = line_1_1.copy()
    line_1_2_copy = line_1_2.copy()
    line_2_1_copy = line_2_1.copy()
    line_2_2_copy = line_2_2.copy()

    domain_position1 = black_domain_pos + (-domain_cage_dist+(black_domain_size*(sqrt(2)-1)))*RIGHT
    domain_position2 = black_domain_pos + (cage_circle_size/sqrt(2) - black_domain_size)*dir(PI/4)
    domain_position3 = black_domain_pos + (cage_circle_size/sqrt(2) - black_domain_size)*dir(PI/4+PI)
    domain_position4 = black_domain_pos + (cage_circle_size/sqrt(2) - black_domain_size)*dir(PI/4+PI/2)
    domain_position5 = black_domain_pos + (cage_circle_size/sqrt(2) - black_domain_size)*dir(-PI/4)
    domain_position6 = domain_position1
    domain_position7 = domain_position5
    domain_position8 = domain_position2
    domain_position9 = domain_position4
    domain_position10 = black_domain_pos

    cage_circle = Circle(radius = cage_circle_size, color = circle_color, stroke_width = 10).move_to(black_domain_pos)

    cur_time = 1*beat_time

    alpha_rate = 1.2

    angle = alpha_tracker.get_value()
    while angle > 0:
        angle -= 2*PI
    alpha_tracker.set_value(angle)

    outer_group.clear_updaters()

    scene.play(
        MoveAlongPath(photons[0], line_1_1_copy, run_time = beat_time, rate_func = linear),
        Create(line_1_1, run_time = beat_time, rate_func = linear),
        MoveAlongPath(photons[1], line_2_1_copy, run_time = beat_time, rate_func = linear),
        Create(line_2_1, run_time = beat_time, rate_func = linear),
        Succession( FadeIn(square_circles_1[0], run_time = .1*beat_time, rate_func = rush_from), FadeOut(square_circles_1[0], scale = 10, run_time = .9*beat_time, rate_func = rush_from)),
        Succession( FadeIn(square_circles_2[0], run_time = .1*beat_time, rate_func = rush_from), FadeOut(square_circles_2[0], scale = 10, run_time = .9*beat_time, rate_func = rush_from)),
        MoveAlongPath(black_domain, Line(black_domain_pos + domain_cage_dist*RIGHT, domain_position1), run_time = beat_time, rate_func = rush_from),
        Succession( FadeIn(bait_circle3, run_time = .1*beat_time, rate_func = rush_from), FadeOut(bait_circle3, scale = 10, run_time = .9*beat_time, rate_func = rush_from)),
        Succession( FadeIn(bait_circle3p, run_time = .1*beat_time, rate_func = rush_from), FadeOut(bait_circle3p, scale = 10, run_time = .9*beat_time, rate_func = rush_from)),
        *move_lightship(run_time = cur_time, move_camera = False),
        photons[2].animate(run_time = cur_time, rate_func = rush_from).move_to(lightship_shift*RIGHT + 2*s*dir(PI/4)),
        photons[3].animate(run_time = cur_time, rate_func = rush_from).move_to(lightship_shift*RIGHT + 2*s*dir(-PI/2-PI/4)),

        # outer_arc1.animate(rate_func = linear, run_time = cur_time).set(stroke_width = 10).rotate(PI/4-angle, about_point = lightship_shift*RIGHT),
        # outer_arc2.animate(rate_func = linear, run_time = cur_time).set(stroke_width = 10).rotate(PI/4-angle, about_point = lightship_shift*RIGHT),
        
        # Rotate(outer_group, -(-angle-PI+PI/4), about_edge = lightship_shift*RIGHT, run_time = cur_time, rate_func = linear),

        # alpha_trackerp.animate.set_value(-PI/2-PI/4)
    )

    scene.remove(cage_arc1, cage_arc2)
    scene.add(cage_circle)

    x_tracker = ValueTracker(lightship_shift)
    scale_tracker = ValueTracker(2*s)
    # rotate_tracker = ValueTracker(-PI/2-PI/4)
    rotate_tracker = ValueTracker(-PI-PI/4)

    # temp_group = VGroup(photons[2], photons[3])

    photons[2].add_updater(lambda m: m.move_to(x_tracker.get_value()*RIGHT + scale_tracker.get_value() * dir(rotate_tracker.get_value()+PI/2+PI)))
    photons[3].add_updater(lambda m: m.move_to(x_tracker.get_value()*RIGHT + scale_tracker.get_value() * dir(rotate_tracker.get_value()+PI/2)))

    def update_temp (m):
        m.restore()
        return m.rotate(rotate_tracker.get_value()+angle).move_to(x_tracker.get_value()*RIGHT).scale(scale_tracker.get_value()/(2*s))

    # outer_group.clear_updaters()
    outer_group.set_z_index(3)
    outer_group.save_state()
    outer_group.add_updater(update_temp)

    lightship_shift2 = 14

    cur_time = 3*beat_time

    alpha_rate = 0.8

    scene.play(
        LaggedStart(
            MoveAlongPath(photons[0], line_1_2_copy, run_time = beat_time, rate_func = linear),
            MoveAlongPath(photons[0], Arc(radius = cage_circle_size, start_angle = 0, angle = PI/2+PI/4).shift(black_domain_pos), run_time = beat_time, rate_func = rush_into),
            MoveAlongPath(photons[0], Line(cage_circle_size*dir(PI/2+PI/4), cage_circle_size/sqrt(2)*dir(PI/2+PI/4)).shift(black_domain_pos), run_time = beat_time, rate_func = rush_into),
            lag_ratio = 1.0
        ),
        Create(line_1_2, run_time = beat_time, rate_func = linear),
        LaggedStart(
            MoveAlongPath(photons[1], line_2_2_copy, run_time = beat_time, rate_func = linear),
            MoveAlongPath(photons[1], Arc(radius = cage_circle_size, start_angle = 0, angle = -PI/4).shift(black_domain_pos), run_time = beat_time, rate_func = rush_into),
            MoveAlongPath(photons[1], Line(cage_circle_size*dir(-PI/4), cage_circle_size/sqrt(2)*dir(-PI/4)).shift(black_domain_pos), run_time = beat_time, rate_func = rush_into),
            lag_ratio = 1.0
        ),
        Create(line_2_2, run_time = beat_time, rate_func = linear),
        LaggedStart(
            MoveAlongPath(black_domain, Line(domain_position1, domain_position2), run_time = beat_time, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position2, domain_position3), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position3, domain_position4), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position4, domain_position5), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position5, domain_position6), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position6, domain_position7), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position7, domain_position8), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position8, domain_position9), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position9, domain_position10), run_time = beat_time/4, rate_func = rush_from),
            lag_ratio = 1.0
        ),
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 10, run_time = .9*beat_time, rate_func = rush_from)) for circle in square_circles_1[1:3] ], lag_ratio = 1.0),
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 10, run_time = .9*beat_time, rate_func = rush_from)) for circle in square_circles_2[1:3] ], lag_ratio = 1.0),
        cage_arc1.animate(rate_func = linear, run_time = cur_time).set_color(circle_color),
        cage_arc2.animate(rate_func = linear, run_time = cur_time).set_color(circle_color),
        x_tracker.animate(rate_func = linear, run_time = cur_time).set_value(-black_domain_pos_num),
        scale_tracker.animate(rate_func = linear, run_time = cur_time).set_value(2*cage_circle_size),
        rotate_tracker.animate(rate_func = linear, run_time = cur_time).increment_value(PI),
        *move_lightship(x_delta = lightship_shift2, x_rate_func = linear, run_time = cur_time, move_camera = False),
        nucleus_dispersion_scale_tracker.animate(run_time = cur_time, rate_func = rush_into).increment_value(0.00001),
        nucleus_scale_tracker.animate(run_time = cur_time, rate_func = rush_into).increment_value(3),
        # scale_glow_animation(nucleus, disp_scale = 1.01, abs_scale = 1.5, shift = lightship_shift2*RIGHT, run_time = cur_time, rate_func = rush_from),
        scene.camera.frame.animate(run_time = cur_time, rate_func = linear).scale(1.7).shift(3*RIGHT) # pyright: ignore[reportAttributeAccessIssue]
    )

    segment1_1_1 = Line(cage_circle_size/sqrt(2)*dir(PI/2+PI/4), cage_circle_size*dir(PI/2), z_index = 1, stroke_width = 10).set_color(YELLOW).shift(black_domain_pos)
    segment1_1_2 = Line(cage_circle_size/sqrt(2)*dir(PI/2+PI/4), cage_circle_size*dir(PI), z_index = 1, stroke_width = 10).set_color(YELLOW).shift(black_domain_pos)
    segment1_2_1 = segment1_1_1.copy()
    segment1_2_2 = segment1_1_2.copy()
    segment2_1_1 = Line(cage_circle_size/sqrt(2)*dir(-PI/4), cage_circle_size*dir(-PI/2), z_index = 1, stroke_width = 10).set_color(YELLOW).shift(black_domain_pos)
    segment2_1_2 = Line(cage_circle_size/sqrt(2)*dir(-PI/4), cage_circle_size*dir(0), z_index = 1, stroke_width = 10).set_color(YELLOW).shift(black_domain_pos)
    segment2_2_1 = segment2_1_1.copy()
    segment2_2_2 = segment2_1_2.copy()

    cage_square1 = Square(color = square_color, z_index = 1.5, stroke_width = 10).scale(cage_circle_size/sqrt(2)).rotate(PI/4).shift(black_domain_pos)

    cur_time = 1*beat_time

    alpha_rate = 0.4

    photons[2].clear_updaters()
    photons[3].clear_updaters()
    outer_group.clear_updaters()

    scene.play(
        Create(segment1_1_1, run_time = beat_time, rate_func = rush_into),
        Create(segment1_2_1, run_time = beat_time, rate_func = rush_into),
        Create(segment1_1_2, run_time = beat_time, rate_func = rush_into),
        Create(segment1_2_2, run_time = beat_time, rate_func = rush_into),
        Create(segment2_1_1, run_time = beat_time, rate_func = rush_into),
        Create(segment2_2_1, run_time = beat_time, rate_func = rush_into),
        Create(segment2_1_2, run_time = beat_time, rate_func = rush_into),
        Create(segment2_2_2, run_time = beat_time, rate_func = rush_into),
        line_1_1.animate(rate_func = linear, run_time = cur_time).set_color(square_color),
        line_1_2.animate(rate_func = linear, run_time = cur_time).set_color(square_color),
        line_2_1.animate(rate_func = linear, run_time = cur_time).set_color(square_color),
        line_2_2.animate(rate_func = linear, run_time = cur_time).set_color(square_color),
        photons[2].animate(rate_func = rush_from, run_time = cur_time, path_arc = PI/2).move_to(black_domain_pos + cage_circle_size*RIGHT),
        photons[3].animate(rate_func = rush_from, run_time = cur_time, path_arc = PI/2).move_to(black_domain_pos + cage_circle_size*LEFT),
        # FadeOut(Circle(color = RED, radius = black_domain_size, z_index = 1).shift(black_domain_pos), scale = 0.05, rate_func = rush_from, run_time = cur_time),
        Succession(FadeIn(domain_fade_circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(domain_fade_circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)),
        *move_lightship(run_time = cur_time, move_camera = False),
    )

    scene.remove(line_1_1, line_1_2, line_2_1, line_2_2)
    scene.add(cage_square1)

    cage_arc1_1 = Arc(radius = cage_circle_size/sqrt(2), start_angle = PI/2+PI/4, angle = -PI/2-PI/4, color = YELLOW, z_index = 1, stroke_width = 10).shift(black_domain_pos)
    cage_arc1_2 = Arc(radius = cage_circle_size/sqrt(2), start_angle = 0, angle = -PI/2, color = YELLOW, z_index = 1, stroke_width = 10).shift(black_domain_pos)
    cage_arc2_1 = Arc(radius = cage_circle_size/sqrt(2), start_angle = -PI/4, angle = -PI/2-PI/4, color = YELLOW, z_index = 1, stroke_width = 10).shift(black_domain_pos)
    cage_arc2_2 = Arc(radius = cage_circle_size/sqrt(2), start_angle = PI, angle = -PI/2, color = YELLOW, z_index = 1, stroke_width = 10).shift(black_domain_pos)

    cage_arc1_1_copy = cage_arc1_1.copy()
    cage_arc1_2_copy = cage_arc1_2.copy()
    cage_arc2_1_copy = cage_arc2_1.copy()
    cage_arc2_2_copy = cage_arc2_2.copy()

    cur_time = 2*beat_time

    alpha_rate = 0.0

    line_1_1p = line_1_1.copy().set_z_index(2).set_color(ORANGE)
    line_1_2p = line_1_2.copy().set_z_index(2).set_color(ORANGE)
    line_2_1p = line_2_1.copy().set_z_index(2).reverse_direction().set_color(ORANGE)
    line_2_2p = line_2_2.copy().set_z_index(2).reverse_direction().set_color(ORANGE)

    # domain_fade_circle_copy1 = domain_fade_circle.copy().set_color(BLACK)

    scene.play(
        Succession(
            AnimationGroup(
                MoveAlongPath(photons[0], cage_arc1_1_copy, run_time = beat_time, rate_func = rush_from),
                MoveAlongPath(photons[1], cage_arc2_1_copy, run_time = beat_time, rate_func = rush_from),
                Create(cage_arc1_1, run_time = beat_time, rate_func = rush_from),
                Create(cage_arc2_1, run_time = beat_time, rate_func = rush_from),
                Rotate(segment1_1_1, angle = -PI/2-PI/4, about_point = black_domain_pos, run_time = beat_time, rate_func = rush_from),
                Rotate(segment1_1_2, angle = -PI/2-PI/4, about_point = black_domain_pos, run_time = beat_time, rate_func = rush_from),
                Rotate(segment1_2_1, angle = -PI/2-PI/4, about_point = black_domain_pos, run_time = beat_time, rate_func = rush_from),
                Rotate(segment1_2_2, angle = -PI/2-PI/4, about_point = black_domain_pos, run_time = beat_time, rate_func = rush_from),
                Rotate(segment2_1_1, angle = -PI/2-PI/4, about_point = black_domain_pos, run_time = beat_time, rate_func = rush_from),
                Rotate(segment2_1_2, angle = -PI/2-PI/4, about_point = black_domain_pos, run_time = beat_time, rate_func = rush_from),
                Rotate(segment2_2_1, angle = -PI/2-PI/4, about_point = black_domain_pos, run_time = beat_time, rate_func = rush_from),
                Rotate(segment2_2_2, angle = -PI/2-PI/4, about_point = black_domain_pos, run_time = beat_time, rate_func = rush_from),
                # FadeOut(Circle(color = RED, radius = black_domain_size, z_index = 1).shift(black_domain_pos), scale = 0.05, rate_func = rush_from, run_time = beat_time),
                # Succession(FadeIn(domain_fade_circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(domain_fade_circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)),
            ),
            AnimationGroup(
                MoveAlongPath(photons[0], cage_arc1_2_copy, run_time = beat_time, rate_func = rush_from),
                MoveAlongPath(photons[1], cage_arc2_2_copy, run_time = beat_time, rate_func = rush_from),
                Create(cage_arc1_2, run_time = beat_time, rate_func = rush_from),
                Create(cage_arc2_2, run_time = beat_time, rate_func = rush_from),
                Rotate(segment1_2_1, angle = -PI/2, about_point = black_domain_pos, run_time = beat_time, rate_func = rush_from),
                Rotate(segment1_2_2, angle = -PI/2, about_point = black_domain_pos, run_time = beat_time, rate_func = rush_from),
                Rotate(segment2_2_1, angle = -PI/2, about_point = black_domain_pos, run_time = beat_time, rate_func = rush_from),
                Rotate(segment2_2_2, angle = -PI/2, about_point = black_domain_pos, run_time = beat_time, rate_func = rush_from),
            )
        ),
        MoveAlongPath(photons[2], Circle(radius = cage_circle_size).shift(black_domain_pos), run_time = cur_time, rate_func = linear),
        MoveAlongPath(photons[3], Circle(radius = cage_circle_size).shift(black_domain_pos).rotate(PI), run_time = cur_time, rate_func = linear),
        FadeOut(domain_fade_circle, scale = 0.05, run_time = beat_time, rate_func = rush_from),
    )

    cur_time = 4*beat_time

    cage_circle_inner = Circle(radius = cage_circle_size/sqrt(2), z_index = 2, color = ORANGE, stroke_width = 10).rotate(PI/2).shift(black_domain_pos)
    cage_circle_inner_copy = cage_circle_inner.copy()

    segment1_1_1_copy = segment1_1_1.copy().set(stroke_width = 10, color = ORANGE, z_index = 2)
    segment1_1_2_copy = segment1_1_2.copy().set(stroke_width = 10, color = ORANGE, z_index = 2).reverse_direction()
    segment1_2_1_copy = segment1_2_1.copy().set(stroke_width = 10, color = ORANGE, z_index = 2)
    segment1_2_2_copy = segment1_2_2.copy().set(stroke_width = 10, color = ORANGE, z_index = 2).reverse_direction()
    segment2_1_1_copy = segment2_1_1.copy().set(stroke_width = 10, color = ORANGE, z_index = 2)
    segment2_1_2_copy = segment2_1_2.copy().set(stroke_width = 10, color = ORANGE, z_index = 2).reverse_direction()
    segment2_2_1_copy = segment2_2_1.copy().set(stroke_width = 10, color = ORANGE, z_index = 2)
    segment2_2_2_copy = segment2_2_2.copy().set(stroke_width = 10, color = ORANGE, z_index = 2).reverse_direction()

    cage_inner_octagon = RegularPolygon(8, color = YELLOW, z_index = 0, stroke_width = 10).scale(cage_circle_size).shift(black_domain_pos)
    cage_inner_octagon_copy = cage_inner_octagon.copy()

    cage_square_circles_1 = [Circle(radius = .25, color = YELLOW, stroke_width = 8).move_to(black_domain_pos+cage_circle_size/sqrt(2)*dir(-PI/2-i*PI/2)) for i in range(0,4)]
    cage_square_circles_2 = [Circle(radius = .25, color = YELLOW, stroke_width = 8).move_to(black_domain_pos+cage_circle_size*dir(PI-i*PI/2)) for i in range(0,4)]
    octagon_circles = [Circle(radius = .25, color = YELLOW, stroke_width = 8).move_to(black_domain_pos+cage_circle_size*dir(i*PI/4)) for i in range(0,8)]

    scene.play(
        FadeOut(domain_fade_circle, scale = 0.05, run_time = beat_time, rate_func = rush_from),
        Succession(
            AnimationGroup(
                MoveAlongPath(photons[3], line_1_1, run_time = beat_time, rate_func = rush_from),
                Create(line_1_1p, run_time = beat_time, rate_func = rush_from),
            ),
            AnimationGroup(
                MoveAlongPath(photons[3], line_1_2, run_time = beat_time, rate_func = rush_from),
                Create(line_1_2p, run_time = beat_time, rate_func = rush_from),
            ),
            AnimationGroup(
                MoveAlongPath(photons[3], line_2_2.reverse_direction(), run_time = beat_time, rate_func = rush_from),
                Create(line_2_2p, run_time = beat_time, rate_func = rush_from),
            ),
            AnimationGroup(
                MoveAlongPath(photons[3], line_2_1.reverse_direction(), run_time = beat_time, rate_func = rush_from),
                Create(line_2_1p, run_time = beat_time, rate_func = rush_from),
            ),
        ),
        Succession(
            MoveAlongPath(photons[0], segment1_2_1, run_time = beat_time/2, rate_func = linear),
            MoveAlongPath(photons[0], segment2_1_2.reverse_direction(), run_time = beat_time/2, rate_func = linear),
            MoveAlongPath(photons[0], segment2_1_1, run_time = beat_time/2, rate_func = linear),
            MoveAlongPath(photons[0], segment2_2_2.reverse_direction(), run_time = beat_time/2, rate_func = linear),
            MoveAlongPath(photons[0], segment2_2_1, run_time = beat_time/2, rate_func = linear),
            MoveAlongPath(photons[0], segment1_1_2.reverse_direction(), run_time = beat_time/2, rate_func = linear),
            MoveAlongPath(photons[0], segment1_1_1, run_time = beat_time/2, rate_func = linear),
            MoveAlongPath(photons[0], segment1_2_2.reverse_direction(), run_time = beat_time/2, rate_func = linear),
        ),
        Succession(
            Create(segment1_2_1_copy, run_time = beat_time/2, rate_func = linear),
            Create(segment2_1_2_copy, run_time = beat_time/2, rate_func = linear),
            Create(segment2_1_1_copy, run_time = beat_time/2, rate_func = linear),
            Create(segment2_2_2_copy, run_time = beat_time/2, rate_func = linear),
            Create(segment2_2_1_copy, run_time = beat_time/2, rate_func = linear),
            Create(segment1_1_2_copy, run_time = beat_time/2, rate_func = linear),
            Create(segment1_1_1_copy, run_time = beat_time/2, rate_func = linear),
            Create(segment1_2_2_copy, run_time = beat_time/2, rate_func = linear),
        ),
        segment1_1_1.animate(rate_func = linear, run_time = cur_time).set_color(square_color),
        segment1_1_2.animate(rate_func = linear, run_time = cur_time).set_color(square_color),
        segment1_2_1.animate(rate_func = linear, run_time = cur_time).set_color(square_color),
        segment1_2_2.animate(rate_func = linear, run_time = cur_time).set_color(square_color),
        segment2_1_1.animate(rate_func = linear, run_time = cur_time).set_color(square_color),
        segment2_1_2.animate(rate_func = linear, run_time = cur_time).set_color(square_color),
        segment2_2_1.animate(rate_func = linear, run_time = cur_time).set_color(square_color),
        segment2_2_2.animate(rate_func = linear, run_time = cur_time).set_color(square_color),

        Create(cage_circle_inner, run_time = cur_time, rate_func = linear),
        MoveAlongPath(photons[1], cage_circle_inner_copy, run_time = cur_time, rate_func = linear),
        cage_arc1_1.animate(rate_func = linear, run_time = cur_time).set_color(circle_color),
        cage_arc1_2.animate(rate_func = linear, run_time = cur_time).set_color(circle_color),
        cage_arc2_1.animate(rate_func = linear, run_time = cur_time).set_color(circle_color),
        cage_arc2_2.animate(rate_func = linear, run_time = cur_time).set_color(circle_color),
        # *move_lightship(run_time = cur_time, move_camera = False),
        Create(cage_inner_octagon, run_time = cur_time, rate_func = linear),
        MoveAlongPath(photons[2], cage_inner_octagon_copy, run_time = cur_time, rate_func = linear),

        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 10, run_time = .9*beat_time, rate_func = rush_from)) for circle in cage_square_circles_1 ], lag_ratio = 1.0),
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 10, run_time = .9*beat_time, rate_func = rush_from)) for circle in cage_square_circles_2 ], lag_ratio = 1.0),
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .05*beat_time, rate_func = rush_from), FadeOut(circle, scale = 10, run_time = .45*beat_time, rate_func = rush_from)) for circle in octagon_circles ], lag_ratio = 1.0),
    )

    cage_square2 = Square(color = ORANGE, z_index = 1.5, stroke_width = 10).scale(cage_circle_size/sqrt(2)).shift(black_domain_pos)
    cage_square1.set_color(ORANGE)

    scene.add(cage_square2)

    cage_inner_group = VGroup(cage_square1, cage_square2, cage_circle_inner, cage_circle, cage_inner_octagon)

    cur_time = 1*beat_time

    scene.play(
        FadeOut(domain_fade_circle, scale = 0.05, run_time = beat_time, rate_func = rush_from),
        scene.camera.frame.animate(run_time = cur_time, rate_func = rush_from).scale(0.15).shift(5*RIGHT + (lightship_shift + lightship_shift2)*RIGHT), # pyright: ignore[reportAttributeAccessIssue]
        alpha_tracker.animate(rate_func = rush_from, run_time = cur_time).set_value(0.0),
        photons[0].animate(rate_func = rush_from, run_time = cur_time).move_to((lightship_shift + lightship_shift2)*RIGHT + sqrt(2)*s*UP),
        photons[1].animate(rate_func = rush_from, run_time = cur_time).move_to((lightship_shift + lightship_shift2)*RIGHT + sqrt(2)*s*DOWN),
        photons[2].animate(rate_func = rush_from, run_time = cur_time).move_to((lightship_shift + lightship_shift2)*RIGHT + sqrt(2)*s*RIGHT),
        photons[3].animate(rate_func = rush_from, run_time = cur_time).move_to((lightship_shift + lightship_shift2)*RIGHT + sqrt(2)*s*LEFT),
        nucleus_dispersion_scale_tracker.animate(run_time = cur_time, rate_func = rush_into).increment_value(0.0001),
        # nucleus_scale_tracker.animate(run_time = cur_time, rate_func = rush_into).increment_value(10),
    )

    cage_inner_octagon.set_color(circle_color)

    outer_arc1.set(stroke_width = 7)
    outer_arc2.set(stroke_width = 7)

    cage_outer_group = VGroup(outer_arc1, outer_arc2)

    scene.remove(segment1_1_1, segment1_1_2, segment1_2_1, segment1_2_2, segment2_1_1, segment2_1_2, segment2_2_1, segment2_2_2)
    scene.remove(segment1_1_1_copy, segment1_1_2_copy, segment1_2_1_copy, segment1_2_2_copy, segment2_1_1_copy, segment2_1_2_copy, segment2_2_1_copy, segment2_2_2_copy)
    scene.remove(cage_arc1_1, cage_arc1_2, cage_arc2_1, cage_arc2_2)
    scene.remove(line_1_1, line_1_2, line_2_1, line_2_2)
    scene.remove(line_1_1p, line_1_2p, line_2_1p, line_2_2p)

    cur_time = 1*beat_time

    cage_middle_circle1 = inner_shell_circle.copy().set(color = YELLOW, z_index = 4, stroke_width = 7)
    cage_middle_circle2 = middle_shell_circle.copy().set(color = YELLOW, z_index = 4, stroke_width = 7)
    cage_middle_square1 = rotating_squares[0].copy().set(color = YELLOW, z_index = 4, stroke_width = 7)
    cage_middle_square2 = rotating_squares[1].copy().set(color = YELLOW, z_index = 4, stroke_width = 7)
    cage_middle_octagon = middle_shell_octagon.copy().set(color = YELLOW, z_index = 4, stroke_width = 7)

    cage_middle_group = VGroup(cage_middle_circle1, cage_middle_circle2, cage_middle_square1, cage_middle_square2, cage_middle_octagon)

    scene.play(
        FadeOut(domain_fade_circle, scale = 0.05, run_time = beat_time, rate_func = rush_from),
        *[ photon.animate(run_time = cur_time, rate_func = rush_into).scale(2) for photon in photons ],
        FadeIn(cage_middle_group, run_time = cur_time, rate_func = rush_into),
    )

    scene.remove(middle_group)

    x_tracker.set_value(lightship_shift + lightship_shift2)
    scale_tracker.set_value(1)
    rotate_tracker.set_value(0)

    def update_temp2 (m):
        m.restore()
        return m.rotate(rotate_tracker.get_value()).move_to(x_tracker.get_value()*RIGHT).scale(scale_tracker.get_value())

    cage_middle_group.save_state()
    cage_middle_group.add_updater(update_temp2)

    photons[0].add_updater(lambda m: m.move_to(x_tracker.get_value()*RIGHT + sqrt(2)*s*scale_tracker.get_value()*dir(rotate_tracker.get_value()+PI/2)))
    photons[1].add_updater(lambda m: m.move_to(x_tracker.get_value()*RIGHT + sqrt(2)*s*scale_tracker.get_value()*dir(rotate_tracker.get_value()-PI/2)))
    photons[2].add_updater(lambda m: m.move_to(x_tracker.get_value()*RIGHT + sqrt(2)*s*scale_tracker.get_value()*dir(rotate_tracker.get_value())))
    photons[3].add_updater(lambda m: m.move_to(x_tracker.get_value()*RIGHT + sqrt(2)*s*scale_tracker.get_value()*dir(rotate_tracker.get_value()+PI)))

    cur_time = 5*beat_time

    scene.play(
        FadeOut(domain_fade_circle, scale = 0.05, run_time = beat_time, rate_func = rush_from),
        x_tracker.animate(rate_func = linear, run_time = cur_time).set_value(-black_domain_pos_num),
        scale_tracker.animate(rate_func = linear, run_time = cur_time).set_value(cage_circle_size/s),
        rotate_tracker.animate(rate_func = linear, run_time = cur_time).increment_value(2*PI+PI/2),
        scene.camera.frame.animate(run_time = cur_time, rate_func = rush_from).scale(1/0.15).shift((-lightship_shift-lightship_shift2)*RIGHT),
        nucleus_dispersion_scale_tracker.animate(run_time = cur_time, rate_func = rush_into).increment_value(-0.00005),
    )

    cage_middle_group.clear_updaters()
    photons[0].clear_updaters()
    photons[1].clear_updaters()
    photons[2].clear_updaters()
    photons[3].clear_updaters()

    cur_time = 1*beat_time

    scene.play(
        FadeOut(domain_fade_circle, scale = 0.05, run_time = beat_time, rate_func = rush_from),
        Rotate(outer_group, angle = -PI/2, about_point = black_domain_pos, run_time = cur_time, rate_func = rush_from),
        Rotate(cage_inner_group, angle = PI/4, about_point = black_domain_pos, run_time = cur_time, rate_func = rush_from),
        cage_middle_circle1.animate(rate_func = linear, run_time = cur_time).set_color(circle_color),
        cage_middle_circle2.animate(rate_func = linear, run_time = cur_time).set_color(square_color),
        cage_middle_square1.animate(rate_func = linear, run_time = cur_time).set_color(square_color),
        cage_middle_square2.animate(rate_func = linear, run_time = cur_time).set_color(square_color),
        cage_middle_octagon.animate(rate_func = linear, run_time = cur_time).set_color(square_color),
    )

    cur_time = beat_time

    scene.remove(*tracing_paths)
    tracing_paths = [ TracedPath(photons[i][0].get_center, dissipating_time = beat_time/2, stroke_width = 3, stroke_opacity = [1,1]).set_color(YELLOW_C) for i in range(4) ]
    scene.add(*tracing_paths)

    scene.play(
        scene.camera.frame.animate(run_time = cur_time, rate_func = rush_from).scale(0.075).shift((lightship_shift + lightship_shift2)*RIGHT), # pyright: ignore[reportAttributeAccessIssue]
        photons[0].animate(rate_func = rush_from, run_time = cur_time).move_to((lightship_shift + lightship_shift2)*RIGHT + c*dir(-PI/6)).scale(.1),
        photons[1].animate(rate_func = rush_from, run_time = cur_time).move_to((lightship_shift + lightship_shift2)*RIGHT + c*dir(PI/2)).scale(.1),
        photons[2].animate(rate_func = rush_from, run_time = cur_time).move_to((lightship_shift + lightship_shift2)*RIGHT + c*dir(PI+PI/6)).scale(.1),
        photons[3].animate(rate_func = rush_from, run_time = cur_time).move_to((lightship_shift + lightship_shift2)*RIGHT + c*dir(PI-PI/6)).scale(.1),
        nucleus_dispersion_scale_tracker.animate(run_time = cur_time, rate_func = rush_from).increment_value(0.00005),
    )

    core_arc_small1 = Arc(radius = c/2, start_angle = PI-PI/6, angle = -2*PI/3, color = ORANGE, z_index = -0.5).shift((lightship_shift+lightship_shift2)*RIGHT)
    core_arc_small2 = Arc(radius = c/2, start_angle = -PI/2, angle = -2*PI/3, color = ORANGE, z_index = -0.5).shift((lightship_shift+lightship_shift2)*RIGHT)
    core_arc_small3 = Arc(radius = c/2, start_angle = PI/6, angle = -2*PI/3, color = ORANGE, z_index = -0.5).shift((lightship_shift+lightship_shift2)*RIGHT)

    core_line1_1 = Line(start = c/2*dir(PI/6), end = c*dir(-PI/6), color = ORANGE, z_index = -0.5).shift((lightship_shift+lightship_shift2)*RIGHT)
    core_line1_2 = Line(start = c*dir(PI/2), end = c/2*dir(PI/6), color = ORANGE, z_index = -0.5).shift((lightship_shift+lightship_shift2)*RIGHT)
    core_line2_1 = Line(start = c/2*dir(PI-PI/6), end = c*dir(PI/2), color = ORANGE, z_index = -0.5).shift((lightship_shift+lightship_shift2)*RIGHT)
    core_line2_2 = Line(start = c*dir(PI+PI/6), end = c/2*dir(PI-PI/6), color = ORANGE, z_index = -0.5).shift((lightship_shift+lightship_shift2)*RIGHT)
    core_line3_1 = Line(start = c/2*dir(-PI/2), end = c*dir(PI+PI/6), color = ORANGE, z_index = -0.5).shift((lightship_shift+lightship_shift2)*RIGHT)
    core_line3_2 = Line(start = c*dir(-PI/6), end = c/2*dir(-PI/2), color = ORANGE, z_index = -0.5).shift((lightship_shift+lightship_shift2)*RIGHT)

    core_circle_bigp = Circle(radius = c, color = ORANGE).rotate(PI/2+PI/3).shift((lightship_shift+lightship_shift2)*RIGHT)

    scene.add(core_arc_small1, core_arc_small2, core_arc_small3)
    scene.add(core_line1_1, core_line1_2, core_line2_1, core_line2_2, core_line3_1, core_line3_2)
    scene.add(core_circle_bigp)
    scene.remove(core_group)

    cur_time = 6*beat_time

    scene.play(
        MoveAlongPath(photons[3], core_circle_bigp.copy().reverse_direction(), run_time = cur_time, rate_func = linear),
        Uncreate(core_circle_bigp, run_time = cur_time, rate_func = linear),
        Succession(
            AnimationGroup(
                MoveAlongPath(photons[0], core_line1_1.copy().reverse_direction(), run_time = 2*beat_time, rate_func = linear),
                MoveAlongPath(photons[1], core_line2_1.copy().reverse_direction(), run_time = 2*beat_time, rate_func = linear),
                MoveAlongPath(photons[2], core_line3_1.copy().reverse_direction(), run_time = 2*beat_time, rate_func = linear),
                Uncreate(core_line1_1, run_time = 2*beat_time, rate_func = linear),
                Uncreate(core_line2_1, run_time = 2*beat_time, rate_func = linear),
                Uncreate(core_line3_1, run_time = 2*beat_time, rate_func = linear),
            ),
            AnimationGroup(
                MoveAlongPath(photons[0], core_arc_small1.copy().reverse_direction(), run_time = 2*beat_time, rate_func = linear),
                MoveAlongPath(photons[1], core_arc_small2.copy().reverse_direction(), run_time = 2*beat_time, rate_func = linear),
                MoveAlongPath(photons[2], core_arc_small3.copy().reverse_direction(), run_time = 2*beat_time, rate_func = linear),
                Uncreate(core_arc_small1, run_time = 2*beat_time, rate_func = linear),
                Uncreate(core_arc_small2, run_time = 2*beat_time, rate_func = linear),
                Uncreate(core_arc_small3, run_time = 2*beat_time, rate_func = linear),
            ),
            AnimationGroup(
                MoveAlongPath(photons[0], core_line2_2.copy().reverse_direction(), run_time = 2*beat_time, rate_func = linear),
                MoveAlongPath(photons[1], core_line3_2.copy().reverse_direction(), run_time = 2*beat_time, rate_func = linear),
                MoveAlongPath(photons[2], core_line1_2.copy().reverse_direction(), run_time = 2*beat_time, rate_func = linear),
                Uncreate(core_line1_2, run_time = 2*beat_time, rate_func = linear),
                Uncreate(core_line2_2, run_time = 2*beat_time, rate_func = linear),
                Uncreate(core_line3_2, run_time = 2*beat_time, rate_func = linear),
            ),
        )
    )

    cur_time = beat_time

    scene.play(
        scene.camera.frame.animate(run_time = cur_time, rate_func = rush_from).scale(1/0.075).shift((lightship_shift + lightship_shift2)*LEFT), # pyright: ignore[reportAttributeAccessIssue]
        photons[0].animate(rate_func = rush_from, run_time = cur_time).move_to(36*RIGHT + 10*UP).scale(3),
        photons[1].animate(rate_func = rush_from, run_time = cur_time).move_to(16*RIGHT + 10*DOWN).scale(3),
        photons[2].animate(rate_func = rush_from, run_time = cur_time).move_to(18*RIGHT + 11*UP).scale(3),
        photons[3].animate(rate_func = rush_from, run_time = cur_time).move_to(33*RIGHT + 12*DOWN).scale(3),
    )

    # Nucleus is gaining power

    shift = lightship_shift + lightship_shift2

    circles = [ Circle(radius = 1, color = YELLOW, stroke_width = 8).move_to(shift*RIGHT) for _ in range(4) ]
    domain_fade_circles = [ Circle(radius = black_domain_size, color = RED, stroke_width = 20, z_index = 10).move_to(black_domain_pos) for _ in range(4) ]

    cur_time = 3*beat_time

    scene.play(
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 10, run_time = .9*beat_time, rate_func = rush_from)) for circle in circles[0:3] ], lag_ratio = 1.0),
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)) for circle in domain_fade_circles[0:3] ], lag_ratio = 1.0),
        nucleus_dispersion_scale_tracker.animate(run_time = cur_time, rate_func = rush_from).increment_value(-0.00006),
        photons[0].animate(rate_func = rush_from, run_time = cur_time).scale(1.5),
        photons[1].animate(rate_func = rush_from, run_time = cur_time).scale(1.5),
        photons[2].animate(rate_func = rush_from, run_time = cur_time).scale(1.5),
        photons[3].animate(rate_func = rush_from, run_time = cur_time).scale(1.5),
    )

    cur_time = beat_time

    scene.play(
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 10, run_time = .9*beat_time, rate_func = rush_from)) for circle in circles[3:4] ], lag_ratio = 1.0),
        # Indicate(nucleus, color = YELLOW, scale_factor = 2, run_time = cur_time, rate_func = rush_from),
        Succession(FadeIn(domain_fade_circles[3], run_time = .1*beat_time, rate_func = rush_from), FadeOut(domain_fade_circles[3], scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)),
    )

    circles = [ Circle(radius = 1.4, color = YELLOW, stroke_width = 40).move_to(shift*RIGHT) for _ in range(4) ]
    domain_fade_circles = [ Circle(radius = black_domain_size, color = RED, stroke_width = 20, z_index = 10).move_to(black_domain_pos) for _ in range(4) ]

    cur_time = 3*beat_time

    scene.play(
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 10, run_time = .9*beat_time, rate_func = rush_from)) for circle in circles[0:3] ], lag_ratio = 1.0),
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)) for circle in domain_fade_circles[0:3] ], lag_ratio = 1.0),
        nucleus_dispersion_scale_tracker.animate(run_time = cur_time, rate_func = rush_from).increment_value(-0.00003),
        photons[0].animate(rate_func = rush_from, run_time = cur_time).shift(2*RIGHT + 2*UP).scale(1.5),
        photons[1].animate(rate_func = rush_from, run_time = cur_time).shift(-2*RIGHT -2*UP).scale(1.5),
        photons[2].animate(rate_func = rush_from, run_time = cur_time).shift(-2*RIGHT + 2*UP).scale(1.5),
        photons[3].animate(rate_func = rush_from, run_time = cur_time).shift(2*RIGHT - 2*UP).scale(1.5),
    )

    cur_time = beat_time

    scene.play(
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 10, run_time = .9*beat_time, rate_func = rush_from)) for circle in circles[3:4] ], lag_ratio = 1.0),
        # Indicate(nucleus, color = YELLOW, scale_factor = 2, run_time = cur_time, rate_func = rush_from),
        Succession(FadeIn(domain_fade_circles[3], run_time = .1*beat_time, rate_func = rush_from), FadeOut(domain_fade_circles[3], scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)),
    )

    circles = [ Circle(radius = 1.5, color = YELLOW, stroke_width = 40).move_to(shift*RIGHT) for _ in range(8) ]

    cur_time = 3*beat_time

    scene.play(
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 20, run_time =.9*beat_time, rate_func = rush_from)) for circle in circles[0:3] ], lag_ratio = 1.0),
        # LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)) for circle in domain_fade_circles ], lag_ratio = 1.0),
        nucleus_dispersion_scale_tracker.animate(run_time = cur_time, rate_func = rush_from).increment_value(-0.00003),
        # nucleus_dispersion_scale_tracker.animate(run_time = cur_time, rate_func = rush_into).increment_value(0.00007),
        # nucleus_scale_tracker.animate(run_time = cur_time, rate_func = rush_from).increment_value(150),
        photons[0].animate(rate_func = rush_from, run_time = cur_time).shift(2*RIGHT + 2*UP).scale(1.5),
        photons[1].animate(rate_func = rush_from, run_time = cur_time).shift(-1*RIGHT -2*UP).scale(1.5),
        photons[2].animate(rate_func = rush_from, run_time = cur_time).shift(-1*RIGHT + 2*UP).scale(1.5),
        photons[3].animate(rate_func = rush_from, run_time = cur_time).shift(2*RIGHT - 2*UP).scale(1.5),
    )

    cur_time = beat_time

    domain_fade_circles = [ Circle(radius = black_domain_size, color = RED, stroke_width = 20, z_index = 10).move_to(black_domain_pos) for _ in range(1) ]

    scene.play(
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 20, run_time = .9*beat_time, rate_func = rush_from)) for circle in circles[3:4] ], lag_ratio = 1.0),
        # Indicate(nucleus, color = YELLOW, scale_factor = 2, run_time = cur_time, rate_func = rush_from),
        Succession(FadeIn(domain_fade_circles[0], run_time = .1*beat_time, rate_func = rush_from), FadeOut(domain_fade_circles[0], scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)),
    )

    cur_time = 2*beat_time

    circles = [ Circle(radius = 1.75, color = YELLOW, stroke_width = 50).move_to(shift*RIGHT) for _ in range(7) ]
    domain_fade_circles = [ Circle(radius = black_domain_size/2, color = RED, stroke_width = 20, z_index = 10).move_to(black_domain_pos) for _ in range(2) ]

    scene.play(
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 20, run_time = .9*beat_time, rate_func = rush_from)) for circle in circles[0:2] ], lag_ratio = 1.0),
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)) for circle in domain_fade_circles[0:2] ], lag_ratio = 1.0),
        black_domain.animate(run_time = beat_time, rate_func = rush_into).scale(1/2),
        photons[0].animate(rate_func = rush_from, run_time = cur_time).shift(2*RIGHT + 2*UP).scale(1.5),
        photons[1].animate(rate_func = rush_from, run_time = cur_time).shift(-1*RIGHT -2*UP).scale(1.5),
        photons[2].animate(rate_func = rush_from, run_time = cur_time).shift(-1*RIGHT + 2*UP).scale(1.5),
        photons[3].animate(rate_func = rush_from, run_time = cur_time).shift(2*RIGHT - 2*UP).scale(1.5),
        nucleus_dispersion_scale_tracker.animate(run_time = cur_time, rate_func = rush_from).increment_value(-0.00003),
    )

    cur_time = 2*beat_time

    circles = [ Circle(radius = 1, color = YELLOW, stroke_width = 45).move_to(photons[i][0]) for i in range(len(photons)) ]
    scales = [20,15,25,30]
    domain_fade_circles = [ Circle(radius = black_domain_size/2, color = RED, stroke_width = 20, z_index = 10).move_to(black_domain_pos) for _ in range(2) ]

    scene.play(
        LaggedStart(
            *[
                Succession(
                    FadeIn(circles[i], scale = .5, run_time = .1*beat_time),
                    FadeOut(circles[i], scale = scales[i], run_time = .9*beat_time, rate_func = rush_from)
                )
                for i in [0,2,1,3]
            ],
            lag_ratio = .25
        ),
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)) for circle in domain_fade_circles ], lag_ratio = 1.0),
    )

    cur_time = 12*beat_time

    def get_angle (pos):
        return arctan2(pos[1], pos[0])

    def get_dist (pos):
        return sqrt(pos[0]**2 + pos[1]**2)

    def get_point (dist, angle):
        return dist*(cos(angle)*RIGHT + sin(angle)*UP)

    photon_angle_tracker0 = ValueTracker(get_angle(photons[0].get_center() - shift*RIGHT))
    photon_angle_tracker1 = ValueTracker(get_angle(photons[1].get_center() - shift*RIGHT))
    photon_angle_tracker2 = ValueTracker(get_angle(photons[2].get_center() - shift*RIGHT))
    photon_angle_tracker3 = ValueTracker(get_angle(photons[3].get_center() - shift*RIGHT))

    photon_dist_tracker0 = ValueTracker(get_dist(photons[0].get_center() - shift*RIGHT))
    photon_dist_tracker1 = ValueTracker(get_dist(photons[1].get_center() - shift*RIGHT))
    photon_dist_tracker2 = ValueTracker(get_dist(photons[2].get_center() - shift*RIGHT))
    photon_dist_tracker3 = ValueTracker(get_dist(photons[3].get_center() - shift*RIGHT))

    photons[0].add_updater(lambda m: m.move_to(shift*RIGHT + get_point(photon_dist_tracker0.get_value(), photon_angle_tracker0.get_value())))
    photons[1].add_updater(lambda m: m.move_to(shift*RIGHT + get_point(photon_dist_tracker1.get_value(), photon_angle_tracker1.get_value())))
    photons[2].add_updater(lambda m: m.move_to(shift*RIGHT + get_point(photon_dist_tracker2.get_value(), photon_angle_tracker2.get_value())))
    photons[3].add_updater(lambda m: m.move_to(shift*RIGHT + get_point(photon_dist_tracker3.get_value(), photon_angle_tracker3.get_value())))

    domain_position1 = black_domain_pos + (-domain_cage_dist)*RIGHT
    domain_position4 = black_domain_pos + (-domain_cage_dist)*dir(PI/4)
    domain_position3 = black_domain_pos + (-domain_cage_dist)*dir(PI/4+PI)
    domain_position2 = black_domain_pos + (-domain_cage_dist)*dir(PI/4+PI/2)
    domain_position5 = black_domain_pos + (-domain_cage_dist)*dir(-PI/4)
    domain_position9 = domain_position1
    domain_position6 = domain_position5
    domain_position8 = domain_position2
    domain_position7 = domain_position4
    domain_position10 = domain_position3
    domain_position11 = domain_position6
    domain_position12 = black_domain_pos

    domain_fade_circles = [ Circle(radius = black_domain_size/2, color = RED, stroke_width = 20, z_index = 10).move_to(black_domain_pos) for _ in range(6) ]

    scene.play(
        photon_dist_tracker0.animate(run_time = cur_time, rate_func = rush_into).set_value(0),
        photon_dist_tracker1.animate(run_time = cur_time, rate_func = rush_into).set_value(0),
        photon_dist_tracker2.animate(run_time = cur_time, rate_func = rush_into).set_value(0),
        photon_dist_tracker3.animate(run_time = cur_time, rate_func = rush_into).set_value(0),
        photon_angle_tracker0.animate(run_time = cur_time, rate_func = rush_into).increment_value(3*PI),
        photon_angle_tracker1.animate(run_time = cur_time, rate_func = rush_into).increment_value(3*PI),
        photon_angle_tracker2.animate(run_time = cur_time, rate_func = rush_into).increment_value(3*PI),
        photon_angle_tracker3.animate(run_time = cur_time, rate_func = rush_into).increment_value(3*PI),
        nucleus_dispersion_scale_tracker.animate(run_time = cur_time, rate_func = rush_into).increment_value(0.00008),
        nucleus_scale_tracker.animate(run_time = cur_time, rate_func = rush_into).increment_value(12),
        Rotate(cage_inner_group, 3*PI, run_time = cur_time, rate_func = rush_into),
        Rotate(cage_middle_group, -3*PI, run_time = cur_time, rate_func = rush_into),
        Rotate(cage_outer_group, 3*PI-PI/4, run_time = cur_time, rate_func = rush_into),
        LaggedStart(
            MoveAlongPath(black_domain, Line(domain_position1, domain_position2), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position2, domain_position3), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position3, domain_position4), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position4, domain_position5), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position5, domain_position6), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position6, domain_position7), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position7, domain_position8), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position8, domain_position9), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position9, domain_position10), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position10, domain_position11), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position11, domain_position12), run_time = beat_time/4, rate_func = rush_from),
            *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)) for circle in domain_fade_circles[0:3] ],
            MoveAlongPath(black_domain, Line(domain_position1, domain_position2), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position2, domain_position4), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position4, domain_position6), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position6, domain_position11), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position11, domain_position8), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position8, domain_position5), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position5, domain_position9), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position9, domain_position3), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position3, domain_position10), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position10, domain_position7), run_time = beat_time/4, rate_func = rush_from),
            MoveAlongPath(black_domain, Line(domain_position7, domain_position12), run_time = beat_time/4, rate_func = rush_from),
            *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)) for circle in domain_fade_circles[3:6] ],
            lag_ratio = 1.0
        ),
    )

    cur_time = 4*beat_time

    # nucleus.clear_updaters()

    for photon in photons:
        photon.clear_updaters()
        photon.set_z_index(1.9)

    scene.play(
        scene.camera.frame.animate(run_time = cur_time, rate_func = linear).scale(0.2).shift(black_domain_pos), # pyright: ignore[reportAttributeAccessIssue]
        nucleus_dispersion_scale_tracker.animate(run_time = cur_time, rate_func = rush_from).increment_value(0.00007),
        nucleus_scale_tracker.animate(run_time = cur_time, rate_func = rush_from).increment_value(-6),
        nucleus_shift_tracker.animate(run_time = cur_time, rate_func = rush_from).increment_value(-shift - black_domain_pos_num),
        *[ photon.animate(run_time = cur_time, rate_func = rush_from).scale(0.5).shift((shift+black_domain_pos_num)*LEFT) for photon in photons ],
        black_domain.animate(run_time = cur_time, rate_func = linear).scale(1.3),
    )

    # scene.play(
    #     nucleus_dispersion_scale_tracker.animate(run_time = cur_time, rate_func = rush_from).increment_value(-0.0001),
    #     nucleus_scale_tracker.animate(run_time = cur_time, rate_func = rush_from).increment_value(1),
    #     scene.camera.frame.animate(run_time = cur_time, rate_func = rush_from).scale(6), # pyright: ignore[reportAttributeAccessIssue]
    #     Flash(Dot(black_domain_pos), color = YELLOW, num_lines = 30, flash_radius = 1, line_length = cage_circle_size/sqrt(2) - 2),
    #     # Flash(cage_circle_inner, color = YELLOW, num_lines = 30, flash_radius = cage_circle_size/sqrt(2) + 1, line_length = s*sqrt(2) - cage_circle_size/sqrt(2) - 2, rate_func = rush_from),
    #     # Flash(cage_middle_circle1, color = YELLOW, num_lines = 30, flash_radius = s*sqrt(2) - cage_circle_size/sqrt(2) + 1, line_length = 0.8*(s-c), rate_func = rush_from),
    #     # Flash(cage_inner_group, run_time = cur_time, rate_func = rush_from),
    #     # Flash(cage_middle_group, run_time = cur_time, rate_func = rush_from),
    #     # Flash(cage_outer_group, run_time = cur_time, rate_func = rush_from),
    # )

    light_count = 95

    random.seed(10)

    angle_randoms = random.random(light_count)
    dist_randoms = random.random(light_count)
    angles = [ interpolate(0, 2*PI, r) for r in angle_randoms ]
    dists = [ interpolate(cage_circle_size*sqrt(2), 10*cage_circle_size, r) for r in dist_randoms ]

    light_dispersion = 1.08
    light_number = 100
    light_radius = 3.5
    light_opacity = 0.2
    lights = [ create_glow(Dot(get_point(dists[i], angles[i]) + black_domain_pos), rad = light_radius, num = light_number, dispersion = light_dispersion, col = YELLOW_C, z_index = 100, opacity = light_opacity) for i in range(light_count) ]

    scene.clear()
    scene.add(cage_inner_group, cage_middle_group, cage_outer_group, nucleus, black_domain)

    light_paths = [ TracedPath(light[0].get_center, dissipating_time = 0.3, stroke_width = 10, stroke_opacity = [1,1]).set_color(YELLOW_C) for light in lights ]
    scene.add(*light_paths)

    # group_scale_tracker = ValueTracker(1)
    # group_rotate_tracker = ValueTracker(12*PI)
    #
    # def update_group (m):
    #     m.restore()
    #     m.scale(group_scale_tracker.get_value())
    #     m.rotate(group_rotate_tracker.get_value())
    #
    # cage_inner_group.clear_updaters()
    # cage_inner_group.save_state()
    # cage_inner_group.add_updater(update_group)
    # cage_middle_group.clear_updaters()
    # cage_middle_group.save_state()
    # cage_middle_group.add_updater(update_group)
    # cage_outer_group.clear_updaters()
    # cage_outer_group.save_state()
    # cage_outer_group.add_updater(update_group)

    cur_time = 32.5*beat_time

    # def rush_into_p (f: float) -> float:
    #     return rush_into(f, inflection = 6)

    scene.play(
        FadeOut(black_domain, scale = 2, run_time = beat_time, rate_func = rush_from),
        Succession(
            ApplyMethod(scene.camera.frame.scale, 8, run_time = beat_time, rate_func = rush_from),
            ApplyMethod(scene.camera.frame.scale, 1.2, run_time = 15*beat_time, rate_func = linear),
            # ApplyMethod(scene.camera.frame.scale, 0.85, run_time = 16*beat_time, rate_func = linear),
        ),
        Succession(
            ApplyMethod(nucleus_dispersion_scale_tracker.increment_value, -0.00009, run_time = beat_time, rate_func = rush_from),
            Wait(15*beat_time),
            ApplyMethod(nucleus_dispersion_scale_tracker.increment_value, -0.00003, run_time = 16*beat_time, rate_func = rush_into),
        ),

        Rotate(cage_inner_group, 48*PI, run_time = cur_time, rate_func = rush_into),
        Rotate(cage_middle_group, -48*PI, run_time = cur_time, rate_func = rush_into),
        Rotate(cage_outer_group, 48*PI, run_time = cur_time, rate_func = rush_into),
        LaggedStart(
            *[ FadeIn(light, run_time = beat_time/3, rate_func = rush_from) for light in lights ],
            *[ light.animate(run_time = beat_time/3, rate_func = rush_into).move_to(black_domain_pos).scale(0.1) for light in lights ],
            lag_ratio = 0.5
        ),
    )

    circle = Circle(radius = 2, color = YELLOW_C, stroke_width = 85).move_to(black_domain_pos)
    cur_time = beat_time

    scene.remove(*lights)
    scene.remove(*light_paths)

    scene.play(
        FadeOut(cage_inner_group, scale = 2, run_time = cur_time, rate_func = rush_from),
        FadeOut(cage_middle_group, scale = 2, run_time = cur_time, rate_func = rush_from),
        FadeOut(cage_outer_group, scale = 2, run_time = cur_time, rate_func = rush_from),
        Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 20, run_time = .9*beat_time, rate_func = rush_from)),
        nucleus.animate(run_time = cur_time, rate_func = rush_from).set_color(YELLOW_C),
        nucleus_dispersion_scale_tracker.animate(run_time = beat_time, rate_func = rush_from).increment_value(0.0001),
        nucleus_scale_tracker.animate(run_time = beat_time, rate_func = rush_from).increment_value(200),
    )

    # scene.play(FadeIn(nucleus), run_time = beat_time, rate_func = rush_from)

    nucleus_path = TracedPath(nucleus[0].get_center, dissipating_time = 0.05, stroke_width = 100, stroke_opacity = [1,1]).set_color(YELLOW_C)
    scene.add(nucleus_path)

    nimb_rs = [15,15*sqrt(2),30]
    nucleus_nimbs = [ Circle(color = ManimColor((255,255,100)), radius = nimb_rs[i], stroke_width = 15, z_index = 100).move_to(black_domain_pos).rotate(PI/4 * i) for i in range(3) ]

    cur_time = 3*beat_time
    scene.play(
        LaggedStart(
            *[ Create(nimb, run_time = 2*beat_time, rate_func = rush_from) for nimb in nucleus_nimbs ],
            lag_ratio = 0.25,
        ),
    )

    nucleus.clear_updaters()

    def move_nucleus (path, run_time, rate_func, lag):
        ind_time = run_time/(1+3*lag)
        return LaggedStart(
            MoveAlongPath(nucleus, path, run_time = ind_time, rate_func = rate_func),
            *[ MoveAlongPath(nimb, path, run_time = ind_time, rate_func = rate_func) for nimb in nucleus_nimbs ],
            lag_ratio = lag
        )

    cur_time = 4*beat_time

    curve = CubicBezier(black_domain_pos, black_domain_pos + 31*LEFT +38*UP, black_domain_pos + 56*LEFT +43*DOWN, black_domain_pos + 30*RIGHT + 15*DOWN)
    # scene.add(curve)

    domain1_size = 30
    domain1_pos = black_domain_pos + 100*UP + 30*RIGHT
    domain1 = Circle(color = BLACK, fill_color = BLACK, fill_opacity = 1.0, radius = domain1_size, z_index = 101).shift(domain1_pos)

    domain2_size = 20
    domain2_pos = black_domain_pos + 30*UP + 140*LEFT
    domain2 = Circle(color = BLACK, fill_color = BLACK, fill_opacity = 1.0, radius = domain2_size, z_index = 101).shift(domain2_pos)

    domain3_size = 50
    domain3_pos = black_domain_pos + 120*DOWN + 30*LEFT
    domain3 = Circle(color = BLACK, fill_color = BLACK, fill_opacity = 1.0, radius = domain3_size, z_index = 101).shift(domain3_pos)

    domain4_size = 40
    domain4_pos = black_domain_pos + 110*DOWN + 50*RIGHT
    domain4 = Circle(color = BLACK, fill_color = BLACK, fill_opacity = 1.0, radius = domain4_size, z_index = 101).shift(domain4_pos)

    scene.add(domain1, domain2, domain3, domain4)

    scene.play(
        move_nucleus(curve, cur_time, rush_from, 0.01),
        scene.camera.frame.animate(run_time = cur_time, rate_func = smooth).scale(2),
        domain1.animate(run_time = cur_time, rate_func = smooth).shift(15*RIGHT+20*DOWN),
        domain2.animate(run_time = cur_time, rate_func = smooth).shift(50*RIGHT+10*DOWN),
        domain3.animate(run_time = cur_time, rate_func = smooth).shift(20*LEFT+35*UP),
        domain4.animate(run_time = cur_time, rate_func = smooth).shift(40*RIGHT+15*UP),
    )

    repeat_num = 6

    domain1_shift = 5*RIGHT + 25*DOWN
    domain1_fade_circles = [ Circle(radius = domain1_size, color = RED, stroke_width = 60, z_index = 110).move_to(domain1.get_center() + i/repeat_num*domain1_shift) for i in range(repeat_num) ]
    domain2_shift = 65*RIGHT + 5*DOWN
    domain2_fade_circles = [ Circle(radius = domain2_size, color = RED, stroke_width = 60, z_index = 110).move_to(domain2.get_center() + i/repeat_num*domain2_shift) for i in range(repeat_num) ]
    domain3_shift = 15*RIGHT + 30*UP
    domain3_fade_circles = [ Circle(radius = domain3_size, color = RED, stroke_width = 60, z_index = 110).move_to(domain3.get_center() + i/repeat_num*domain3_shift) for i in range(repeat_num) ]
    domain4_shift = 15*LEFT + 35*UP
    domain4_fade_circles = [ Circle(radius = domain4_size, color = RED, stroke_width = 60, z_index = 110).move_to(domain4.get_center() + i/repeat_num*domain4_shift) for i in range(repeat_num) ]

    shift = 5*LEFT + 10*UP
    circles = [ Circle(radius = 3, color = YELLOW, stroke_width = 120).move_to(nucleus[0].get_center() + i/4 * shift) for i in range(repeat_num // 2) ]

    cur_time = 6*beat_time

    # lag = 0.05
    # ind_time = cur_time/(1+3*lag)

    # pos = nucleus[0].get_center()

    scene.play(
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)) for circle in domain1_fade_circles ], lag_ratio = 1.0),
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)) for circle in domain2_fade_circles ], lag_ratio = 1.0),
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)) for circle in domain3_fade_circles ], lag_ratio = 1.0),
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)) for circle in domain4_fade_circles ], lag_ratio = 1.0),
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 15, run_time = 1.9*beat_time, rate_func = rush_from)) for circle in circles ], lag_ratio = 1.0),
        domain1.animate(run_time = cur_time, rate_func = linear).shift(domain1_shift),
        domain2.animate(run_time = cur_time, rate_func = linear).shift(domain2_shift),
        domain3.animate(run_time = cur_time, rate_func = linear).shift(domain3_shift),
        domain4.animate(run_time = cur_time, rate_func = linear).shift(domain4_shift),
        nucleus.animate(run_time = cur_time, rate_func = linear).shift(shift),
        *[ nimb.animate(run_time = cur_time, rate_func = linear).shift(shift) for nimb in nucleus_nimbs ],
        # LaggedStart(
        #     MoveAlongPath(nucleus, Line(pos, pos+shift), run_time = ind_time, rate_func = linear).scale(0.5).shift(shift),
        #     *[ nimb.animate(run_time = ind_time, rate_func = linear).shift(shift) for nimb in nucleus_nimbs ],
        #     lag_ratio = lag
        # )
    )

    num = 3
    circles = [ Circle(radius = 80, color = YELLOW, stroke_width = 50).set_fill(YELLOW, opacity = 0.01).move_to(nucleus[0].get_center()) for _ in range(num) ]
    cur_time = 2*beat_time
    domain1_fade_circles = [ Circle(radius = domain1_size, color = RED, stroke_width = 60, z_index = 110).move_to(domain1.get_center()) for i in range(2) ]
    domain2_fade_circles = [ Circle(radius = domain2_size, color = RED, stroke_width = 60, z_index = 110).move_to(domain2.get_center()) for i in range(2) ]
    domain3_fade_circles = [ Circle(radius = domain3_size, color = RED, stroke_width = 60, z_index = 110).move_to(domain3.get_center()) for i in range(2) ]
    domain4_fade_circles = [ Circle(radius = domain4_size, color = RED, stroke_width = 60, z_index = 110).move_to(domain4.get_center()) for i in range(2) ]

    ind_time = 1.5*beat_time

    scene.play(
        LaggedStart(
            *[
                Succession(
                    FadeIn(circle, run_time = .01*beat_time, rate_func = rush_from),
                    FadeOut(circle, scale = 0.03, run_time = ind_time - 0.01*beat_time, rate_func = rush_from)
                ) for circle in circles
            ], lag_ratio = (cur_time - ind_time)/((num-1) * ind_time)
        ),
        nucleus.animate(run_time = cur_time, rate_func = rush_from).scale(0.2),
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)) for circle in domain1_fade_circles ], lag_ratio = 1.0),
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)) for circle in domain2_fade_circles ], lag_ratio = 1.0),
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)) for circle in domain3_fade_circles ], lag_ratio = 1.0),
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)) for circle in domain4_fade_circles ], lag_ratio = 1.0),
    )

    nova_number = 200
    nova_radius = 50
    nova_dispersion = 1.045
    nova_opacity = 0.2

    nova1 = create_glow(Dot(domain1.get_center()), rad = nova_radius, num = nova_number, col = YELLOW_C, dispersion = nova_dispersion, opacity = nova_opacity, z_index = 3)
    nova2 = create_glow(Dot(domain2.get_center()), rad = nova_radius, num = nova_number, col = YELLOW_C, dispersion = nova_dispersion, opacity = nova_opacity, z_index = 3)
    nova3 = create_glow(Dot(domain3.get_center()), rad = nova_radius, num = nova_number, col = YELLOW_C, dispersion = nova_dispersion, opacity = nova_opacity, z_index = 3)
    nova4 = create_glow(Dot(domain4.get_center()), rad = nova_radius, num = nova_number, col = YELLOW_C, dispersion = nova_dispersion, opacity = nova_opacity, z_index = 3)

    cur_time = 3.5*beat_time

    scene.play(
        FadeOut(domain1, scale = 1.5, run_time = beat_time, rate_func = rush_from),
        FadeOut(domain2, scale = 1.5, run_time = beat_time, rate_func = rush_from),
        FadeOut(domain3, scale = 1.5, run_time = beat_time, rate_func = rush_from),
        FadeOut(domain4, scale = 1.5, run_time = beat_time, rate_func = rush_from),
        nucleus.animate(run_time = cur_time, rate_func = rush_from).scale(12),
        FadeIn(nova1, scale = 0.5, run_time = cur_time, rate_func = rush_from),
        FadeIn(nova2, scale = 0.5, run_time = cur_time, rate_func = rush_from),
        FadeIn(nova3, scale = 0.5, run_time = cur_time, rate_func = rush_from),
        FadeIn(nova4, scale = 0.5, run_time = cur_time, rate_func = rush_from),
    )

    nova1_path = TracedPath(nova1[0].get_center, dissipating_time = 0.5, stroke_width = 50, stroke_opacity = [1,1]).set_color(YELLOW_C)
    scene.add(nova1_path)
    nova2_path = TracedPath(nova2[0].get_center, dissipating_time = 0.5, stroke_width = 50, stroke_opacity = [1,1]).set_color(YELLOW_C)
    scene.add(nova2_path)
    nova3_path = TracedPath(nova3[0].get_center, dissipating_time = 0.5, stroke_width = 50, stroke_opacity = [1,1]).set_color(YELLOW_C)
    scene.add(nova3_path)
    nova4_path = TracedPath(nova4[0].get_center, dissipating_time = 0.5, stroke_width = 50, stroke_opacity = [1,1]).set_color(YELLOW_C)
    scene.add(nova4_path)

    cur_time = 2*beat_time
    ind_time = 0.8*cur_time

    cur_pos = nucleus[0].get_center()

    scene.play(
        LaggedStart(
            nova1.animate(run_time = ind_time, rate_func = rush_from).scale(0.3).move_to(cur_pos),
            nova2.animate(run_time = ind_time, rate_func = rush_from).scale(0.3).move_to(cur_pos),
            nova3.animate(run_time = ind_time, rate_func = rush_from).scale(0.3).move_to(cur_pos),
            nova4.animate(run_time = ind_time, rate_func = rush_from).scale(0.3).move_to(cur_pos),
            lag_ratio = (cur_time - ind_time)/((num-1) * ind_time)
        ),
        nucleus.animate(run_time = cur_time, rate_func = smooth).scale(1.1),
    )

    scene.remove(nova1, nova2, nova3, nova4)

    cur_time = 6.2*beat_time
    
    endpoint = cur_pos + 130*LEFT + 25*DOWN

    curve = CubicBezier(cur_pos, cur_pos + 111*RIGHT +168*UP, cur_pos + 146*RIGHT +153*DOWN, endpoint)

    # scene.add(curve)
    # scene.remove(nucleus)

    scene.play(
        move_nucleus(curve, cur_time, rush_from, 0.005),
        scene.camera.frame.animate(run_time = cur_time, rate_func = smooth).move_to(endpoint).scale(0.52)
    )

    cur_time = 3*beat_time
    stroke = 72

    scene.play(
        Succession(
            *[ Transform(nucleus_nimbs[i], Square(color = "#0289FE", stroke_width = stroke).move_to(endpoint).scale(nimb_rs[i]).rotate(PI/4 * i), run_time = cur_time/3) for i in range(3) ]
        ),
        Succession(
            ApplyMethod(nucleus.scale, 0.1, run_time = 2/3*cur_time, rate_func = smooth),
            FadeOut(nucleus, scale = 5, run_time = 1/3*cur_time, rate_func = rush_from),
        )
    )

    scene.play(
        FadeOut(VGroup(*nucleus_nimbs), scale = 0.9, shift = 10*DOWN, run_time = beat_time, rate_func = rush_from)
    )
