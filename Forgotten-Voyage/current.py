from common import *

def current (scene: Scene):
    scene.add_sound("./part-2p.mp3")
    # From 71 seconds

    s = 1.8144
    c = 0.864
    beat_time = 0.515
    # photon_radius = 2.5
    # photon_number = 1
    # photon_dispersion = 1.0015
    # center = [-235.04502097,38.32961899,0.0]
    center = ORIGIN
    # square_color = "#29E0FF"
    square_color = "#019DFF"
    circle_color = BLUE
    dots = [
        Dot().move_to(center+.2*RIGHT),
        Dot().move_to(center+.2*UP),
        Dot().move_to(center+.2*LEFT),
        Dot().move_to(center+.2*DOWN),
    ]
    number_of_squares = 3
    colors = [ "#0289FE", "#FC5105", "#B232D1", "#1FDEFF", "#FF4F04", "#B530D1", "#74CE03", "#29E0FF", "#FF0C41", "#019DFF", "#FF5007", "#FF1042", "#B630D3", "#75CF05", "#1EDFFE", "#FF0E43" ]
    rotating_squares = [ Square(z_index = -1, color = colors[0]).scale(s).rotate(i*PI/4) for i in range(number_of_squares) ]
    # photons = [create_glow(dot, rad = photon_radius, num = photon_number, dispersion = photon_dispersion, col = YELLOW_C) for dot in dots] # pyright: ignore[reportAttributeAccessIssue]
    photons = [dot.copy().set_color(YELLOW).set_z_index(100) for dot in dots] # pyright: ignore[reportAttributeAccessIssue]
    nucleus = create_glow(Dot(center), rad = 2.5, num = 20, dispersion = 1.004)

    line1 = Line(start = c*dir(-PI/6), end = c*dir(PI/2), color = ORANGE, z_index = -0.5)
    line2 = Line(start = c*dir(PI/2), end = c*dir(PI/2+2*PI/3), color = ORANGE, z_index = -0.5)
    line3 = Line(start = c*dir(PI/2+2*PI/3), end = c*dir(-PI/6), color = ORANGE, z_index = -0.5)
    core_triangle = VGroup(line1, line2, line3)
    core_circle_small = Circle(color = ORANGE, z_index = -0.1).scale(c/2).rotate(PI/6)
    core_circle_big = Circle(color = ORANGE, z_index = -1).scale(c).rotate(-2*PI/3-PI/6)
    core_layer = VGroup(core_circle_big, core_circle_small, core_triangle)

    inner_shell_circle = Circle(radius = s, color = circle_color, z_index = -0.5).rotate(-PI/4)
    middle_shell_octagon = RegularPolygon(8, color = circle_color, z_index = -1).scale(sqrt(2)*s)
    middle_shell_circle = Circle(radius = s*sqrt(2), color = circle_color, z_index = -1).rotate(PI/2+PI/4)
    middle_layer = VGroup(inner_shell_circle, rotating_squares[1], rotating_squares[0], middle_shell_octagon, middle_shell_circle)

    outer_arc1 = Arc(radius = 2*s, start_angle = PI, angle = -PI/2, color = circle_color)
    outer_arc2 = Arc(radius = 2*s, start_angle = 0, angle = -PI/2, color = circle_color)
    outer_layer = VGroup(outer_arc1, outer_arc2)

    lightship = VGroup(outer_layer, middle_layer, core_layer, nucleus)

    core_group = core_layer
    middle_group = middle_layer
    outer_group = outer_layer
    photon_group = VGroup(*photons)
    # outer_group.add(*photons)

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
    alpha_rate = 2

    def update_nucleus (m):
        m.restore()
        return m.scale(nucleus_scale_tracker.get_value()).shift(r_tracker_main.get_value()*dir(theta_tracker_main.get_value())).shift(x_tracker_main.get_value()*RIGHT + y_tracker_main.get_value()*UP)

    def update_core (m):
        m.restore()
        return m.rotate(alpha_tracker.get_value()/3, about_point = ORIGIN).shift(r_tracker_main.get_value()*dir(theta_tracker_main.get_value())).shift(x_tracker_main.get_value()*RIGHT + y_tracker_main.get_value()*UP)

    def update_middle (m):
        m.restore()
        return m.rotate(-alpha_tracker.get_value()/4, about_point = ORIGIN).scale(middle_scale_tracher.get_value()).shift(r_tracker_main.get_value()*dir(theta_tracker_main.get_value())).shift(x_tracker_main.get_value()*RIGHT + y_tracker_main.get_value()*UP)

    def update_outer (m):
        m.restore()
        return m.rotate(alpha_tracker.get_value(), about_point = ORIGIN).scale(command_scale_tracher.get_value()).shift(r_tracker_lag.get_value()*dir(theta_tracker_lag.get_value())).shift(x_tracker_lag.get_value()*RIGHT + y_tracker_lag.get_value()*UP)

    nucleus.save_state()
    nucleus.add_updater(update_nucleus)
    core_group.save_state()
    core_group.add_updater(update_core)
    middle_group.save_state()
    middle_group.add_updater(update_middle)
    outer_group.save_state()
    outer_group.add_updater(update_outer)
    photon_group.save_state()
    photon_group.add_updater(update_outer)

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


    scene.camera.frame.scale(2.5**2*1.1*1.6*0.8**3) # pyright: ignore[reportAttributeAccessIssue]

    # ---------------------------------------------------------------

    scene.add(*photons)
    scene.add(lightship)

    # Encountering the black domain

    black_domain_size = 9
    black_domain_pos_num = 20
    black_domain_pos = black_domain_pos_num*LEFT
    black_domain = Circle(color = BLACK, fill_color = BLACK, fill_opacity = 1.0, radius = black_domain_size).shift(black_domain_pos)

    scene.add(black_domain)

    domain_fade_circle = Circle(color = RED, radius = black_domain_size, z_index = 1).shift(black_domain_pos)

    cur_time = 3*beat_time
    scene.play(
        *move_lightship(run_time = cur_time),
        # nucleus_scale_tracker.animate(run_time = cur_time, rate_func = rush_into).set_value(3),
        scene.camera.frame.animate(run_time = cur_time, rate_func = linear).scale(0.85).shift(8*LEFT) # pyright: ignore[reportAttributeAccessIssue]
    )
    cur_time = beat_time
    scene.play(
        *move_lightship(run_time = cur_time, move_camera = False),
        FadeOut(Circle(color = YELLOW, radius = 0.2), scale = 100, rate_func = rush_from, run_time = cur_time)
    )
    for _ in range(3):
        scene.play(
            *move_lightship(run_time = cur_time, move_camera = False),
            # FadeOut(Circle(color = RED, radius = black_domain_size, z_index = 1).shift(black_domain_pos), scale = 0.05, rate_func = rush_from, run_time = cur_time)
            
            Succession(FadeIn(domain_fade_circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(domain_fade_circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)),
            # FadeOut(domain_fade_circle, scale = 0.05, rate_func = rush_from, run_time = cur_time)
        )

    # outer_group.remove(*[photons[i] for i in range(3)])
    # for i in range(3):
    #     photons[i].clear_updaters()
    # outer_group.remove(*photons)
    photon_group.clear_updaters()
    outer_group.clear_updaters()
    def update_outer_p (m):
        m.restore()
        return m.scale(command_scale_tracher.get_value()).shift(r_tracker_lag.get_value()*dir(theta_tracker_lag.get_value())).shift(x_tracker_lag.get_value()*RIGHT + y_tracker_lag.get_value()*UP)
    outer_group.save_state()
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

    cur_time = beat_time
    scene.play(
        *move_lightship(run_time = cur_time, move_camera = False),
        photons[0].animate(run_time = cur_time, rate_func = rush_from).move_to(black_domain_pos+black_domain_size*RIGHT+domain_cage_dist*RIGHT).scale(2),
        photons[1].animate(run_time = cur_time, rate_func = rush_from).move_to(black_domain_pos+black_domain_size*RIGHT+domain_cage_dist*RIGHT).scale(2),
        photons[2].animate(run_time = cur_time, rate_func = rush_from).move_to(bait_position1).scale(2),
        photons[3].animate(run_time = cur_time, rate_func = rush_from).move_to(bait_position1p).scale(2),
    )

    cage_arc1 = Arc(radius = black_domain_size+domain_cage_dist, start_angle = 0, angle = PI, color = YELLOW, stroke_width = 10).shift(black_domain_pos)
    cage_arc2 = Arc(radius = black_domain_size+domain_cage_dist, start_angle = 0, angle = -PI, color = YELLOW, stroke_width = 10).shift(black_domain_pos)
    cage_arc1_copy = cage_arc1.copy()
    cage_arc2_copy = cage_arc2.copy()

    cur_time = 4*beat_time

    arc_circles1 = [Circle(radius = .15, color = YELLOW).move_to(black_domain_pos+(black_domain_size+domain_cage_dist)*dir(i*PI/4)) for i in range(0,4)]
    arc_circles2 = [Circle(radius = .15, color = YELLOW).move_to(black_domain_pos+(black_domain_size+domain_cage_dist)*dir(-i*PI/4)) for i in range(0,4)]
    bait_circle1 = Circle(radius = .15, color = YELLOW).move_to(bait_position1)
    bait_circle2 = Circle(radius = .15, color = YELLOW).move_to(bait_position2)
    bait_circle3 = Circle(radius = .15, color = YELLOW).move_to(bait_position3)
    bait_circle1p = Circle(radius = .15, color = YELLOW).move_to(bait_position1p)
    bait_circle2p = Circle(radius = .15, color = YELLOW).move_to(bait_position2p)
    bait_circle3p = Circle(radius = .15, color = YELLOW).move_to(bait_position3p)

    alpha_rate = 1.6

    lightship_shift = domain_cage_dist+3

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

    outer_group.clear_updaters()

    scene.play(
        MoveAlongPath(photons[0], line_1_1, run_time = beat_time, rate_func = linear),
        Create(line_1_1_copy, run_time = beat_time, rate_func = linear),
        MoveAlongPath(photons[1], line_2_1, run_time = beat_time, rate_func = linear),
        Create(line_2_1_copy, run_time = beat_time, rate_func = linear),
        Succession( FadeIn(square_circles_1[0], run_time = .1*beat_time, rate_func = rush_from), FadeOut(square_circles_1[0], scale = 10, run_time = .9*beat_time, rate_func = rush_from)),
        Succession( FadeIn(square_circles_2[0], run_time = .1*beat_time, rate_func = rush_from), FadeOut(square_circles_2[0], scale = 10, run_time = .9*beat_time, rate_func = rush_from)),
        MoveAlongPath(black_domain, Line(black_domain_pos + domain_cage_dist*RIGHT, domain_position1), run_time = beat_time, rate_func = rush_from),
        Succession( FadeIn(bait_circle3, run_time = .1*beat_time, rate_func = rush_from), FadeOut(bait_circle3, scale = 10, run_time = .9*beat_time, rate_func = rush_from)),
        Succession( FadeIn(bait_circle3p, run_time = .1*beat_time, rate_func = rush_from), FadeOut(bait_circle3p, scale = 10, run_time = .9*beat_time, rate_func = rush_from)),
        *move_lightship(run_time = cur_time, move_camera = False),
        photons[2].animate(run_time = cur_time, rate_func = rush_from).move_to(lightship_shift*RIGHT + 2*s*dir(PI/2+PI/4)),
        photons[3].animate(run_time = cur_time, rate_func = rush_from).move_to(lightship_shift*RIGHT + 2*s*dir(-PI/4)),
        outer_arc1.animate(rate_func = linear, run_time = cur_time).set(stroke_width = 10),
        outer_arc2.animate(rate_func = linear, run_time = cur_time).set(stroke_width = 10),
    )

    scene.remove(cage_arc1, cage_arc2)
    scene.add(cage_circle)

    x_tracker = ValueTracker(lightship_shift)
    scale_tracker = ValueTracker(2*s)
    rotate_tracker = ValueTracker(-PI/4)

    # temp_group = VGroup(photons[2], photons[3])

    photons[2].add_updater(lambda m: m.move_to(x_tracker.get_value()*RIGHT + scale_tracker.get_value() * dir(rotate_tracker.get_value()+PI)))
    photons[3].add_updater(lambda m: m.move_to(x_tracker.get_value()*RIGHT + scale_tracker.get_value() * dir(rotate_tracker.get_value())))

    def update_temp (m):
        m.restore()
        return m.rotate(rotate_tracker.get_value()+PI/4).move_to(x_tracker.get_value()*RIGHT).scale(scale_tracker.get_value()/(2*s))

    # outer_group.clear_updaters()
    outer_group.set_z_index(2)
    outer_group.save_state()
    outer_group.add_updater(update_temp)

    # outer_group.clear_updaters()
    # outer_group.add_updater(lambda m: m.move_to(x_tracker.get_value()*RIGHT).rotate(rotate_tracker.get_value()+PI/4).scale(scale_tracker.get_value()/(2*s)))

    lightship_shift2 = 14

    cur_time = 3*beat_time

    alpha_rate = 0.8

    scene.play(
        LaggedStart(
            MoveAlongPath(photons[0], line_1_2, run_time = beat_time, rate_func = linear),
            MoveAlongPath(photons[0], Arc(radius = cage_circle_size, start_angle = 0, angle = PI/2+PI/4).shift(black_domain_pos), run_time = beat_time, rate_func = rush_into),
            MoveAlongPath(photons[0], Line(cage_circle_size*dir(PI/2+PI/4), cage_circle_size/sqrt(2)*dir(PI/2+PI/4)).shift(black_domain_pos), run_time = beat_time, rate_func = rush_into),
            lag_ratio = 1.0
        ),
        Create(line_1_2_copy, run_time = beat_time, rate_func = linear),
        LaggedStart(
            MoveAlongPath(photons[1], line_2_2, run_time = beat_time, rate_func = linear),
            MoveAlongPath(photons[1], Arc(radius = cage_circle_size, start_angle = 0, angle = -PI/4).shift(black_domain_pos), run_time = beat_time, rate_func = rush_into),
            MoveAlongPath(photons[1], Line(cage_circle_size*dir(-PI/4), cage_circle_size/sqrt(2)*dir(-PI/4)).shift(black_domain_pos), run_time = beat_time, rate_func = rush_into),
            lag_ratio = 1.0
        ),
        Create(line_2_2_copy, run_time = beat_time, rate_func = linear),
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

    cage_square1 = Square(color = square_color, z_index = 0.5, stroke_width = 10).scale(cage_circle_size/sqrt(2)).rotate(PI/4).shift(black_domain_pos)

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
        photons[2].animate(rate_func = rush_from, run_time = cur_time, path_arc = PI/3).move_to(black_domain_pos + cage_circle_size*RIGHT),
        photons[3].animate(rate_func = rush_from, run_time = cur_time, path_arc = PI/3).move_to(black_domain_pos + cage_circle_size*LEFT),
        # FadeOut(Circle(color = RED, radius = black_domain_size, z_index = 1).shift(black_domain_pos), scale = 0.05, rate_func = rush_from, run_time = cur_time),
        Succession(FadeIn(domain_fade_circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(domain_fade_circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)),
        # LaggedStart(
        #     FadeOut(Circle(color = RED, radius = black_domain_size, z_index = 1).shift(black_domain_pos), scale = 0.05, rate_func = rush_from, run_time = beat_time*2/3),
        #     lag_ratio = 0.5
        # ),
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
                # FadeOut(Circle(color = RED, radius = black_domain_size, z_index = 1).shift(black_domain_pos), scale = 0.05, rate_func = rush_from, run_time = beat_time),
                # Succession(FadeIn(domain_fade_circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(domain_fade_circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)),
            )
        ),
        MoveAlongPath(photons[2], Circle(radius = cage_circle_size).shift(black_domain_pos), run_time = cur_time, rate_func = linear),
        MoveAlongPath(photons[3], Circle(radius = cage_circle_size).shift(black_domain_pos).rotate(PI), run_time = cur_time, rate_func = linear),
        FadeOut(domain_fade_circle, scale = 0.05, run_time = beat_time, rate_func = rush_from),
        # Succession(
        #     # FadeIn(domain_fade_circle, run_time = .1*beat_time, rate_func = rush_from),
        #     # FadeOut(domain_fade_circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from),
        #     # FadeIn(domain_fade_circle_copy1, run_time = .1*beat_time, rate_func = rush_from),
        #     domain_fade_circle_copy1.animate(run_time = .1*beat_time, rate_func = rush_into).set_color(RED),
        #     FadeOut(domain_fade_circle_copy1, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from),
        #     # lag_ratio = 1.0
        # ),
        # *move_lightship(run_time = cur_time, move_camera = False),
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

    # domain_fade_circle_copy2 = domain_fade_circle.copy()
    # domain_fade_circle_copy3 = domain_fade_circle.copy()
    # domain_fade_circle_copy4 = domain_fade_circle.copy()

    cage_square_circles_1 = [Circle(radius = .25, color = YELLOW, stroke_width = 8).move_to(black_domain_pos+cage_circle_size/sqrt(2)*dir(-PI/2-i*PI/2)) for i in range(0,4)]
    cage_square_circles_2 = [Circle(radius = .25, color = YELLOW, stroke_width = 8).move_to(black_domain_pos+cage_circle_size*dir(PI-i*PI/2)) for i in range(0,4)]
    octagon_circles = [Circle(radius = .25, color = YELLOW, stroke_width = 8).move_to(black_domain_pos+cage_circle_size*dir(i*PI/4)) for i in range(0,8)]

    scene.play(
        # Create(domain_fade_circle, run_time = beat_time, rate_func = linear),
        # Succession(
        #     FadeIn(domain_fade_circle, run_time = .1*beat_time, rate_func = rush_from),
        #     FadeOut(domain_fade_circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from),
        #     FadeIn(domain_fade_circle_copy1, run_time = .1*beat_time, rate_func = rush_from),
        #     FadeOut(domain_fade_circle_copy1, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from),
        #     FadeIn(domain_fade_circle_copy2, run_time = .1*beat_time, rate_func = rush_from),
        #     FadeOut(domain_fade_circle_copy2, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from),
        #     FadeIn(domain_fade_circle_copy3, run_time = .1*beat_time, rate_func = rush_from),
        #     FadeOut(domain_fade_circle_copy3, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from),
        # ),
        FadeOut(domain_fade_circle, scale = 0.05, run_time = beat_time, rate_func = rush_from),
        Succession(
            AnimationGroup(
                MoveAlongPath(photons[3], line_1_1, run_time = beat_time, rate_func = rush_from),
                Create(line_1_1p, run_time = beat_time, rate_func = rush_from),
                # Succession(FadeIn(domain_fade_circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(domain_fade_circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)),
            ),
            AnimationGroup(
                MoveAlongPath(photons[3], line_1_2, run_time = beat_time, rate_func = rush_from),
                Create(line_1_2p, run_time = beat_time, rate_func = rush_from),
                # Succession(FadeIn(domain_fade_circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(domain_fade_circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)),
            ),
            AnimationGroup(
                MoveAlongPath(photons[3], line_2_2.reverse_direction(), run_time = beat_time, rate_func = rush_from),
                Create(line_2_2p, run_time = beat_time, rate_func = rush_from),
                # Succession(FadeIn(domain_fade_circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(domain_fade_circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)),
            ),
            AnimationGroup(
                MoveAlongPath(photons[3], line_2_1.reverse_direction(), run_time = beat_time, rate_func = rush_from),
                Create(line_2_1p, run_time = beat_time, rate_func = rush_from),
                # Succession(FadeIn(domain_fade_circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(domain_fade_circle, scale = 0.05, run_time = .9*beat_time, rate_func = rush_from)),
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
        scene.camera.frame.animate(run_time = cur_time, rate_func = rush_from).scale(0.15).shift(5*RIGHT + (lightship_shift + lightship_shift2)*RIGHT), # pyright: ignore[reportAttributeAccessIssue]
        alpha_tracker.animate(rate_func = rush_from, run_time = cur_time).set_value(0.0),
        photons[0].animate(rate_func = rush_from, run_time = cur_time).move_to((lightship_shift + lightship_shift2)*RIGHT + sqrt(2)*s*UP),
        photons[1].animate(rate_func = rush_from, run_time = cur_time).move_to((lightship_shift + lightship_shift2)*RIGHT + sqrt(2)*s*DOWN),
        photons[2].animate(rate_func = rush_from, run_time = cur_time).move_to((lightship_shift + lightship_shift2)*RIGHT + sqrt(2)*s*RIGHT),
        photons[3].animate(rate_func = rush_from, run_time = cur_time).move_to((lightship_shift + lightship_shift2)*RIGHT + sqrt(2)*s*LEFT),
    )

    cage_inner_octagon.set_color(circle_color)

    scene.remove(segment1_1_1, segment1_1_2, segment1_2_1, segment1_2_2, segment2_1_1, segment2_1_2, segment2_2_1, segment2_2_2)
    scene.remove(segment1_1_1_copy, segment1_1_2_copy, segment1_2_1_copy, segment1_2_2_copy, segment2_1_1_copy, segment2_1_2_copy, segment2_2_1_copy, segment2_2_2_copy)
    scene.remove(cage_arc1_1, cage_arc1_2, cage_arc2_1, cage_arc2_2)
    scene.remove(line_1_1p, line_1_2p, line_2_1p, line_2_2p)

    cur_time = 1*beat_time

    cage_middle_circle1 = inner_shell_circle.copy().set(color = YELLOW, z_index = 1, stroke_width = 7)
    cage_middle_circle2 = middle_shell_circle.copy().set(color = YELLOW, z_index = 1, stroke_width = 7)
    cage_middle_square1 = rotating_squares[0].copy().set(color = YELLOW, z_index = 1, stroke_width = 7)
    cage_middle_square2 = rotating_squares[1].copy().set(color = YELLOW, z_index = 1, stroke_width = 7)
    cage_middle_octagon = middle_shell_octagon.copy().set(color = YELLOW, z_index = 1, stroke_width = 7)

    cage_middle_group = VGroup(cage_middle_circle1, cage_middle_circle2, cage_middle_square1, cage_middle_square2, cage_middle_octagon)

    scene.play(
        *[ photon.animate(run_time = cur_time, rate_func = rush_into).scale(2) for photon in photons ],
        FadeIn(cage_middle_group, run_time = cur_time, rate_func = rush_into),
        # FadeIn(cage_middle_circle1, run_time = cur_time, rate_func = rush_into),
        # FadeIn(cage_middle_circle2, run_time = cur_time, rate_func = rush_into),
        # FadeIn(cage_middle_square1, run_time = cur_time, rate_func = rush_into),
        # FadeIn(cage_middle_square2, run_time = cur_time, rate_func = rush_into),
        # FadeIn(cage_middle_octagon, run_time = cur_time, rate_func = rush_into),
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
        x_tracker.animate(rate_func = linear, run_time = cur_time).set_value(-black_domain_pos_num),
        scale_tracker.animate(rate_func = linear, run_time = cur_time).set_value(cage_circle_size/s),
        rotate_tracker.animate(rate_func = linear, run_time = cur_time).increment_value(2*PI),
        # *move_lightship(x_delta = 0, x_rate_func = linear, run_time = cur_time, move_camera = False),
        scene.camera.frame.animate(run_time = cur_time, rate_func = rush_from).scale(1/0.15).shift((-lightship_shift-lightship_shift2)*RIGHT), # pyright: ignore[reportAttributeAccessIssue]
    )

    cage_middle_group.clear_updaters()
    photons[0].clear_updaters()
    photons[1].clear_updaters()
    photons[2].clear_updaters()
    photons[3].clear_updaters()

    cur_time = 1*beat_time

    scene.play(
        Rotate(outer_group, angle = -PI/2, about_point = black_domain_pos, run_time = cur_time, rate_func = rush_from),
        Rotate(cage_inner_group, angle = PI/4, about_point = black_domain_pos, run_time = cur_time, rate_func = rush_from),
        cage_middle_circle1.animate(rate_func = linear, run_time = cur_time).set_color(circle_color),
        cage_middle_circle2.animate(rate_func = linear, run_time = cur_time).set_color(square_color),
        cage_middle_square1.animate(rate_func = linear, run_time = cur_time).set_color(square_color),
        cage_middle_square2.animate(rate_func = linear, run_time = cur_time).set_color(square_color),
        cage_middle_octagon.animate(rate_func = linear, run_time = cur_time).set_color(square_color),
    )

    cur_time = beat_time

    scene.play(
        scene.camera.frame.animate(run_time = cur_time, rate_func = rush_from).scale(0.075).shift((lightship_shift + lightship_shift2)*RIGHT), # pyright: ignore[reportAttributeAccessIssue]
        photons[0].animate(rate_func = rush_from, run_time = cur_time).move_to((lightship_shift + lightship_shift2)*RIGHT + c*dir(-PI/6)).scale(.1),
        photons[1].animate(rate_func = rush_from, run_time = cur_time).move_to((lightship_shift + lightship_shift2)*RIGHT + c*dir(PI/2)).scale(.1),
        photons[2].animate(rate_func = rush_from, run_time = cur_time).move_to((lightship_shift + lightship_shift2)*RIGHT + c*dir(PI+PI/6)).scale(.1),
        photons[3].animate(rate_func = rush_from, run_time = cur_time).move_to((lightship_shift + lightship_shift2)*RIGHT + c*dir(PI-PI/6)).scale(.1),
    )

    # line1 = Line(start = c*dir(-PI/6), end = c*dir(PI/2), color = ORANGE, z_index = -0.5)
    # line2 = Line(start = c*dir(PI/2), end = c*dir(PI/2+2*PI/3), color = ORANGE, z_index = -0.5)
    # line3 = Line(start = c*dir(PI/2+2*PI/3), end = c*dir(-PI/6), color = ORANGE, z_index = -0.5)
    # core_triangle = VGroup(line1, line2, line3)
    # core_circle_small = Circle(color = ORANGE, z_index = -0.1).scale(c/2).rotate(PI/6)
    # core_circle_big = Circle(color = ORANGE, z_index = -1).scale(c).rotate(-2*PI/3-PI/6)

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
    # core_circle_bigp_copy = core_circle_bigp.copy()

    scene.add(core_arc_small1, core_arc_small2, core_arc_small3)
    scene.add(core_line1_1, core_line1_2, core_line2_1, core_line2_2, core_line3_1, core_line3_2)
    scene.add(core_circle_bigp)
    scene.remove(core_group)
    # scene.remove(core_circle_small)
    # scene.remove(line1, line2, line3)

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
                MoveAlongPath(photons[0], core_line1_2.copy().reverse_direction(), run_time = 2*beat_time, rate_func = linear),
                MoveAlongPath(photons[1], core_line2_2.copy().reverse_direction(), run_time = 2*beat_time, rate_func = linear),
                MoveAlongPath(photons[2], core_line3_2.copy().reverse_direction(), run_time = 2*beat_time, rate_func = linear),
                Uncreate(core_line1_2, run_time = 2*beat_time, rate_func = linear),
                Uncreate(core_line2_2, run_time = 2*beat_time, rate_func = linear),
                Uncreate(core_line3_2, run_time = 2*beat_time, rate_func = linear),
            ),
        )
    )

    cur_time = beat_time

    scene.play(
        scene.camera.frame.animate(run_time = cur_time, rate_func = rush_from).scale(1/0.075).shift((lightship_shift + lightship_shift2)*LEFT), # pyright: ignore[reportAttributeAccessIssue]
        photons[0].animate(rate_func = rush_from, run_time = cur_time).move_to(6*RIGHT + 6*UP),
        photons[1].animate(rate_func = rush_from, run_time = cur_time).move_to(7*RIGHT + 3*DOWN),
        photons[2].animate(rate_func = rush_from, run_time = cur_time).move_to(5*RIGHT + 7*UP),
        photons[3].animate(rate_func = rush_from, run_time = cur_time).move_to(9*RIGHT + 10*DOWN),
    )

    scene.wait(3)
