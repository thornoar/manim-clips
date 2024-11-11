from common import *

def current (scene: Scene):
    scene.add_sound("./part-2.mp3")

    s = 1.8144
    c = 0.864
    beat_time = 0.515
    photon_radius = 2.5
    photon_number = 1
    photon_dispersion = 1.0015
    # center = [-235.04502097,38.32961899,0.0]
    center = ORIGIN
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
    photons = [create_glow(dot, rad = photon_radius, num = photon_number, dispersion = photon_dispersion, col = YELLOW_C) for dot in dots] # pyright: ignore[reportAttributeAccessIssue]
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
    command_group = outer_layer
    command_group.add(*photons)

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


    # ----------------------------------------------------------


    scene.camera.frame.scale(2.5**2*1.1*1.6*0.8**3) # pyright: ignore[reportAttributeAccessIssue]
    scene.add(*photons)
    scene.add(lightship)

    # Encountering the black domain

    black_domain_size = 10
    black_domain_pos = 20*LEFT
    black_domain = Circle(color = RED, fill_color = BLACK, fill_opacity = 1.0, radius = black_domain_size).shift(black_domain_pos)

    scene.add(black_domain)

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
            FadeOut(Circle(color = RED, radius = black_domain_size, z_index = 1).shift(black_domain_pos), scale = 0.05, rate_func = rush_from, run_time = cur_time)
        )

    command_group.remove(*[photons[i] for i in range(3)])
    # command_group.clear_updaters()

    domain_cage_dist = 4

    bait_position1 = 1*LEFT+10*UP
    bait_position2 = 2*RIGHT+8*DOWN
    bait_position3 = 5*RIGHT+2*UP

    cur_time = beat_time
    scene.play(
        *move_lightship(run_time = cur_time, move_camera = False),
        photons[0].animate(run_time = cur_time, rate_func = rush_from).move_to(black_domain_pos+black_domain_size*RIGHT+domain_cage_dist*RIGHT).scale(3),
        photons[1].animate(run_time = cur_time, rate_func = rush_from).move_to(black_domain_pos+black_domain_size*RIGHT+domain_cage_dist*RIGHT).scale(3),
        photons[2].animate(run_time = cur_time, rate_func = rush_from).move_to(bait_position1).scale(3),
    )

    cage_arc1 = Arc(radius = black_domain_size+domain_cage_dist, start_angle = 0, angle = PI, color = YELLOW).shift(black_domain_pos)
    cage_arc1_copy = cage_arc1.copy()
    cage_arc2 = Arc(radius = black_domain_size+domain_cage_dist, start_angle = 0, angle = -PI, color = YELLOW).shift(black_domain_pos)
    cage_arc2_copy = cage_arc1.copy()

    cur_time = 4*beat_time

    arc_circles1 = [Circle(radius = .15, color = YELLOW).move_to(black_domain_pos+(black_domain_size+domain_cage_dist)*dir(i*PI/4)) for i in range(0,4)]
    arc_circles2 = [Circle(radius = .15, color = YELLOW).move_to(black_domain_pos+(black_domain_size+domain_cage_dist)*dir(-i*PI/4)) for i in range(0,4)]
    bait_circle1 = Circle(radius = .15, color = YELLOW).move_to(bait_position1)
    bait_circle2 = Circle(radius = .15, color = YELLOW).move_to(bait_position2)

    scene.play(
        MoveAlongPath(photons[0], cage_arc1_copy, run_time = cur_time, rate_func = linear),
        MoveAlongPath(photons[1], cage_arc2_copy, run_time = cur_time, rate_func = linear),
        Create(cage_arc1, run_time = cur_time, rate_func = linear),
        Create(cage_arc2, run_time = cur_time, rate_func = linear),
        black_domain.animate(run_time = cur_time, rate_func = rush_into).shift(domain_cage_dist*RIGHT),
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 10, run_time = .9*beat_time, rate_func = rush_from)) for circle in arc_circles1 ], lag_ratio = 1.0),
        LaggedStart( *[ Succession( FadeIn(circle, run_time = .1*beat_time, rate_func = rush_from), FadeOut(circle, scale = 10, run_time = .9*beat_time, rate_func = rush_from)) for circle in arc_circles2 ], lag_ratio = 1.0),
        LaggedStart(
            Succession( FadeIn(bait_circle1, run_time = .1*beat_time, rate_func = rush_from), FadeOut(bait_circle1, scale = 10, run_time = .9*beat_time, rate_func = rush_from)),
            MoveAlongPath(photons[2], Line(bait_position1, bait_position2), run_time = beat_time, rate_func = rush_from),
            Succession( FadeIn(bait_circle2, run_time = .1*beat_time, rate_func = rush_from), FadeOut(bait_circle2, scale = 10, run_time = .9*beat_time, rate_func = rush_from)),
            # MoveAlongPath(photons[2], Line(bait_position2, bait_position3), run_time = beat_time, rate_func = rush_from),
            lag_ratio = 1.0
        ),
        *move_lightship(x_delta = domain_cage_dist+3, x_rate_func = rush_into, move_camera = False, run_time = cur_time),
    )

    scene.play(
        *move_lightship(run_time = cur_time, move_camera = False),
        photons[2].animate(run_time = cur_time, rate_func = rush_from).move_to(bait_position2).scale(3),
    )

    scene.wait(3)
