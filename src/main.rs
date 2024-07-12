use std::collections::HashMap;

use clunky::{
    lost_code::{is_pressed, FixedUpdate, FpsTracker, MaxSubsteps},
    math::{remap, Matrix4},
    physics::{
        physics_3d::{
            aabb::AabbCentredOrigin,
            //bodies::{Body, ImmovableCuboid},
            solver::{self, CpuSolver},
        },
        PhysicsSimulation,
    },
    shaders::{
        instanced_simple_lit_colour_3d::{self, Camera},
        instanced_text_sdf, instanced_unlit_uv_2d_stretch,
    },
};
use gilrs::{EventType, Gilrs};
use input::{Bindings, GameActions, InputManager, RealityActions};
use menus::{Menu, MenuManager};
use renderer::{Camera3D, Renderer, WindowConfig, WindowVariety};
use vulkano::swapchain::PresentMode;
use vulkano_util::window::WindowDescriptor;
use winit::{
    dpi::PhysicalPosition,
    event::{DeviceEvent, Event, KeyboardInput, MouseButton, WindowEvent},
    event_loop::{ControlFlow, EventLoop, EventLoopWindowTarget},
    window::{Fullscreen, WindowId},
};

use body::Body;

use creature_types::{Burgle, CreatureType};

mod body;
mod creature_types;
mod input;
mod menus;
mod renderer;

type Physics = CpuSolver<f32, Body>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct CreatureIndex(usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct BodyIndex(usize);

const GRID_MIN: [isize; 3] = [-200, -3, -200];
const GRID_MAX: [isize; 3] = [200, 10, 200];
const GRID_SUBDIVISIONS: [usize; 3] = [15, 1, 15];

const FIXED_DELTA_TIME: f32 = 0.03;

struct CreaturesManager {
    creature_controlled_by_window: HashMap<WindowId, CreatureIndex>,

    creatures: Vec<CreatureType>,
    captured_creatures: Vec<CreatureIndex>,

    creature_selection_window: WindowId,
}

struct Settings {
    mouse_sensitivity: f32,
}

struct Reality {
    creatures_manager: CreaturesManager,

    physics_simulation: Physics,
    physics_fixed_update: Option<FixedUpdate<f32>>,

    actions: RealityActions,
}

impl Reality {
    fn new(game: &mut Game, event_loop: &EventLoopWindowTarget<()>) -> Self {
        let selection_window = game.renderer.create_window(
            &event_loop,
            &WindowConfig {
                variety: WindowVariety::Selection,
                window_descriptor: WindowDescriptor {
                    ..Default::default()
                },
                swapchain_create_info_modify: |_| {},
            },
        );

        let physics_config = solver::Config {
            ..solver::Config::size_from_min_max_with_subdivisions(
                GRID_MIN,
                GRID_MAX,
                GRID_SUBDIVISIONS,
            )
        };

        let mut reality = Self {
            creatures_manager: CreaturesManager {
                creature_controlled_by_window: HashMap::new(),
                creature_selection_window: selection_window,

                creatures: vec![],
                captured_creatures: vec![],
            },

            physics_simulation: CpuSolver::new(physics_config),
            physics_fixed_update: Some(FixedUpdate::new(
                FIXED_DELTA_TIME,
                MaxSubsteps::WarnAt(100),
            )),

            actions: Default::default(),
        };

        game.renderer.selection_menu_uv_instances_mut().push(
            instanced_unlit_uv_2d_stretch::Instance::new(
                [0.0, 0.0],
                0.0,
                glam::Affine2::from_translation([1.0, 0.0].into()),
            ),
        );

        game.renderer
            .selection_menu_text_instances_mut()
            .push(instanced_text_sdf::Instance::new(
                [0.0, 0.0],
                [1.0, 0.0, 1.0, 1.0],
                0.01,
                0.2,
                glam::Affine2::from_translation([0.0, 0.0].into())
                    * glam::Affine2::from_scale([0.25, 0.25].into()),
            ));
        //text_rendering::blah();

        let test_burgle_window = game.renderer.create_window(
            &event_loop,
            &WindowConfig {
                variety: WindowVariety::Creature(Camera3D {
                    ..Default::default()
                }),
                window_descriptor: WindowDescriptor {
                    present_mode: PresentMode::Fifo,
                    transparent: true,
                    decorations: false,
                    ..Default::default()
                },
                swapchain_create_info_modify: |_| {},
            },
        );

        reality
            .creatures_manager
            .creatures
            .push(CreatureType::Burgle(Burgle::new(
                &mut game.renderer,
                &mut reality.physics_simulation.bodies,
                [0.0; 3],
                [0.5, 1.0, 0.5],
                [1.5; 3],
                [1.0, 0.0, 1.0, 1.0],
                CreatureIndex(reality.creatures_manager.creatures.len()),
            )));

        reality
            .creatures_manager
            .captured_creatures
            .push(CreatureIndex(0));

        reality
            .creatures_manager
            .creature_controlled_by_window
            .insert(test_burgle_window, CreatureIndex(0));

        // 2 player test
        reality
            .creatures_manager
            .creatures
            .push(CreatureType::Burgle(Burgle::new(
                &mut game.renderer,
                &mut reality.physics_simulation.bodies,
                [0.0; 3],
                [0.5, 1.0, 0.5],
                [1.5; 3],
                [1.0, 1.0, 0.0, 1.0],
                CreatureIndex(reality.creatures_manager.creatures.len()),
            )));

        /*
        game.creatures_manager
            .captured_creatures
            .push(CreatureIndex(1));
        */

        game.renderer
            .add_cuboid_colour(instanced_simple_lit_colour_3d::Instance::new(
                [1.0; 4],
                Matrix4::from_translation([
                    GRID_MIN[0] as f32,
                    GRID_MIN[1] as f32,
                    GRID_MIN[2] as f32,
                ]),
            ));

        game.renderer
            .add_cuboid_colour(instanced_simple_lit_colour_3d::Instance::new(
                [1.0; 4],
                Matrix4::from_translation([
                    GRID_MAX[0] as f32,
                    GRID_MAX[1] as f32,
                    GRID_MAX[2] as f32,
                ]),
            ));

        game.renderer
            .add_cuboid_colour(instanced_simple_lit_colour_3d::Instance::new(
                [1.0; 4],
                Matrix4::from_translation([2.0, 0.0, 0.0]),
            ));

        /*
        let second_player_window =
            starting_renderer.create_window(WindowConfig::default(), &temp_event_loop);

        game.creatures_manager
            .creature_controlled_by_window
            .insert(second_player_window, CreatureIndex(1));
        */

        let floor = AabbCentredOrigin {
            position: [0.0, 1.0, 0.0],
            half_size: [
                (GRID_MAX[0] - GRID_MIN[0]) as f32 * 0.5,
                0.5,
                (GRID_MAX[2] - GRID_MIN[2]) as f32 * 0.5,
            ],
        };
        reality
            .physics_simulation
            .bodies
            .push(Body::ImmovableCuboid(floor.clone()));

        game.renderer
            .add_cuboid_colour_from_aabb(floor, [1.0, 0.0, 1.0, 1.0]);

        reality
    }
}

struct Game {
    renderer: Renderer,

    fps: FpsTracker<f32>,

    settings: Settings,

    window_focused: Option<WindowId>,

    actions: GameActions,
    input_manager: InputManager,

    reality: Option<Reality>,

    menu_manager: MenuManager,
}

impl Game {
    fn new() -> (Self, EventLoop<()>) {
        let (renderer, event_loop) = Renderer::new();

        let mut game = Game {
            renderer,

            fps: FpsTracker::new(),

            settings: Settings {
                mouse_sensitivity: 1.0,
            },

            window_focused: None,

            actions: Default::default(),
            input_manager: InputManager {
                gilrs: Gilrs::new().unwrap(),

                active_bindings: 0,
                all_bindings: vec![Bindings::default()],
            },

            reality: None,

            menu_manager: MenuManager::new(),
        };

        game.menu_manager
            .set_menu(Menu::Main, &mut game.renderer, &event_loop);

        (game, event_loop)
    }
}

fn main() {
    println!("TODO:\nStore which grid actually have collision in them, so you don't have to loop over them all.");
    println!("TODO:\nUse ahash.");

    menus::blah();

    let (mut game, event_loop) = Game::new();

    // Temp:
    game.input_manager.all_bindings[0].add_australian_keyboard_default();

    //game.reality = Some(Reality::new(&mut game, &event_loop));

    event_loop.run(move |event, event_loop, control_flow| {
        // Sometimes window events that happen just before a window gets deleted, will only be recieved after the deletion.
        if let Event::WindowEvent { window_id, .. } = event {
            if matches!(game.renderer.window_specifics.get(&window_id), None) {
                println!("Window event for a non-existant window.");
                return;
            }
        }

        // When a window gets removed, window_focused needs to be set to none.
        // Surely there is a better way than this?
        if let Some(window_focused) = game.window_focused {
            if matches!(game.renderer.window_specifics.get(&window_focused), None) {
                game.window_focused = None;
            }
        }

        handle_event(event, event_loop, control_flow, &mut game)
    })
}

fn handle_event(
    event: Event<()>,
    event_loop: &EventLoopWindowTarget<()>,
    control_flow: &mut ControlFlow,
    game: &mut Game,
) {
    match event {
        Event::WindowEvent {
            window_id,
            event: WindowEvent::CloseRequested,
            ..
        } => {
            let window_specific = game.renderer.window_specifics.get(&window_id).unwrap();

            match window_specific.variety {
                WindowVariety::Creature(_) => {
                    game.reality
                        .as_mut()
                        .unwrap()
                        .creatures_manager
                        .creature_controlled_by_window
                        .remove(&window_id)
                        .unwrap();
                }
                WindowVariety::Selection => {
                    todo!("display an are you sure window?");
                }
                WindowVariety::Menu => {
                    todo!("Depends on what menu.")
                }
            }
        }

        Event::WindowEvent {
            event: WindowEvent::Resized(..) | WindowEvent::ScaleFactorChanged { .. },
            window_id,
        } => {
            let extent: [f32; 2] = game
                .renderer
                .windows_manager
                .get_window(window_id)
                .unwrap()
                .inner_size()
                .into();

            game.renderer.correct_window_size(window_id);

            if !matches!(game.menu_manager.menu(), Menu::None) {
                game.menu_manager.resize(extent, &mut game.renderer);
            }

            if let Some(reality) = &game.reality {
                if window_id == reality.creatures_manager.creature_selection_window {
                    menus::TextManager::on_selection_menu_resize(
                        game.renderer
                            .windows_manager
                            .get_renderer(window_id)
                            .unwrap()
                            .window()
                            .inner_size()
                            .into(),
                        game.renderer.selection_menu_text_instances_mut(),
                        &reality.creatures_manager,
                    )
                }
            }
        }

        Event::MainEventsCleared => {
            if matches!(&mut game.reality, Some(_)) {
                let mut physics_fixed_update = game
                    .reality
                    .as_mut()
                    .unwrap()
                    .physics_fixed_update
                    .take()
                    .unwrap();
                physics_fixed_update.update(|| fixed_update(game));
                game.reality.as_mut().unwrap().physics_fixed_update = Some(physics_fixed_update);
            }

            on_update(game, event_loop);

            let bodies: Option<&[Body]> = if let Some(reality) = &game.reality {
                Some(&reality.physics_simulation.bodies)
            } else {
                None
            };

            game.renderer.render(bodies);
            game.fps.update();
        }

        Event::WindowEvent {
            event: WindowEvent::KeyboardInput { input, .. },
            window_id,
        } => {
            game.input_manager.all_bindings[game.input_manager.active_bindings]
                .modify_game_actions_with_keyboard_input(&mut game.actions, input);

            if let Some(reality) = &mut game.reality {
                game.input_manager.all_bindings[game.input_manager.active_bindings]
                    .modify_reality_actions_with_keyboard_input(&mut reality.actions, input);

                if window_id == reality.creatures_manager.creature_selection_window {
                    todo!()
                } else if let Some(creature_id) = reality
                    .creatures_manager
                    .creature_controlled_by_window
                    .get(&window_id)
                {
                    let creature_actions =
                        reality.creatures_manager.creatures[creature_id.0].actions_mut();
                    game.input_manager.all_bindings[game.input_manager.active_bindings]
                        .modify_creature_actions_with_keyboard_input(creature_actions, input);
                }
            }
        }

        Event::WindowEvent {
            window_id,
            event: WindowEvent::Focused(focus),
            ..
        } => {
            if focus {
                game.window_focused = Some(window_id);
            } else {
                game.window_focused = None;
            }
        }

        // Replace with mouse bindings struct.
        Event::WindowEvent {
            window_id,
            event: WindowEvent::MouseInput { state, button, .. },
            ..
        } => {
            let window_specific = game.renderer.window_specifics.get(&window_id).unwrap();

            match window_specific.variety {
                WindowVariety::Creature(_) => match button {
                    MouseButton::Left => {
                        if is_pressed(state) {
                            let focused_creature_id = *game
                                .reality
                                .as_ref()
                                .unwrap()
                                .creatures_manager
                                .creature_controlled_by_window
                                .get(&window_id)
                                .unwrap();
                            game.reality.as_mut().unwrap().creatures_manager.creatures
                                [focused_creature_id.0]
                                .actions_mut()
                                .primary_interact = true;
                        }
                    }

                    _ => (),
                },
                WindowVariety::Selection => {
                    println!("To do!");
                }
                WindowVariety::Menu => {
                    if !is_pressed(state) {
                        game.menu_manager.on_click();
                    }
                }
            }
        }

        Event::WindowEvent {
            event: WindowEvent::CursorMoved { position, .. },
            window_id,
        } => {
            //println!("{:?}", position);
            let window = game.renderer.windows_manager.get_window(window_id).unwrap();
            let window_size: [f32; 2] = window.inner_size().into();
            // somehow convert from physical position to fragment position

            let window_specific = game.renderer.window_specifics.get(&window_id).unwrap();

            match window_specific.variety {
                WindowVariety::Menu => {
                    let fragment_position = [
                        remap(position.x as f32, 0.0..window_size[0], -1.0..1.0),
                        remap(position.y as f32, 0.0..window_size[1], -1.0..1.0),
                    ];
                    //println!("fragment position: {:?}", fragment_position);

                    game.menu_manager
                        .on_cursor_moved(fragment_position, &mut game.renderer);
                }
                WindowVariety::Selection => {
                    // Todo
                }
                _ => (),
            }
        }

        Event::DeviceEvent {
            event: DeviceEvent::MouseMotion { delta },
            ..
        } => {
            let Some(window_focused) = game.window_focused else {
                return;
            };

            let window_renderer = game
                .renderer
                .windows_manager
                .get_renderer_mut(window_focused)
                .unwrap();

            let window_specific = game.renderer.window_specifics.get(&window_focused).unwrap();

            let delta = [
                delta.1 as f32 * game.settings.mouse_sensitivity,
                delta.0 as f32 * game.settings.mouse_sensitivity,
            ];

            match window_specific.variety {
                WindowVariety::Creature(_) => {
                    let reality = game.reality.as_mut().unwrap();
                    if reality.actions.paused {
                        window_renderer.window().set_cursor_visible(true);
                        return;
                    }

                    let creature_id = reality
                        .creatures_manager
                        .creature_controlled_by_window
                        .get(&window_focused)
                        .unwrap();
                    let creature = &mut reality.creatures_manager.creatures[creature_id.0];

                    creature.on_mouse_motion(delta);

                    let window_extent = window_renderer.window_size();

                    window_renderer
                        .window()
                        .set_cursor_position(PhysicalPosition::new(
                            window_extent[0] / 2.0,
                            window_extent[1] / 2.0,
                        ))
                        .unwrap();
                    window_renderer.window().set_cursor_visible(false);
                }
                _ => (),
            }
        }

        _ => (),
    }
}

fn fixed_update(game: &mut Game) {
    let reality = game.reality.as_mut().unwrap();

    if let Some(window_focused) = game.window_focused {
        if let Some(creature) = reality
            .creatures_manager
            .creature_controlled_by_window
            .get(&window_focused)
        {
            let creature = &mut reality.creatures_manager.creatures[creature.0];

            creature.on_physics_fixed_update_before_physics_tick_when_focused(
                &mut reality.physics_simulation.bodies,
            );
        }
    }

    reality.physics_simulation.update(FIXED_DELTA_TIME);
}

fn on_update(game: &mut Game, event_loop: &EventLoopWindowTarget<()>) {
    if matches!(game.menu_manager.menu(), Menu::Main) {
        if game.menu_manager.buttons()[0].pressed {
            game.menu_manager
                .set_menu(Menu::None, &mut game.renderer, event_loop);
            game.reality = Some(Reality::new(game, event_loop));
        }
    }

    while let Some(event) = game.input_manager.gilrs.next_event() {
        //println!("event: {:?}", event);
    }

    if let Some(focused_window) = game.window_focused {
        if game.actions.full_screen {
            game.actions.full_screen = false;

            let window = game
                .renderer
                .windows_manager
                .get_window(focused_window)
                .unwrap();

            window.set_fullscreen(if matches!(window.fullscreen(), None) {
                Some(Fullscreen::Borderless(None))
            } else {
                None
            });
        }

        if game.actions.log_fps {
            game.actions.log_fps = false;
            println!("fps: {}", game.fps.average_fps());
        }
    }

    let Some(reality) = &mut game.reality else {
        return;
    };

    // ((capture, capture's body index), spreader)
    let mut capture_attempts = vec![];
    for creature_index in &reality.creatures_manager.captured_creatures {
        let creature = &mut reality.creatures_manager.creatures[creature_index.0];

        creature.update(
            &reality.physics_simulation.bodies,
            &reality.creatures_manager.captured_creatures,
            &mut capture_attempts,
        );
    }
    for ((capture_index, body_index), spreader_index) in capture_attempts {
        // Wrangle the borrow checker into lettings us mutably attempt_capture on the capture creature and mutably give it the spreader.
        // This feels like it could be simplified.
        let (capture, spreader) = if spreader_index > capture_index {
            let (lhs, rhs) = reality
                .creatures_manager
                .creatures
                .split_at_mut(spreader_index.0);

            (&mut lhs[capture_index.0], &mut rhs[0])
        } else {
            let (lhs, rhs) = reality
                .creatures_manager
                .creatures
                .split_at_mut(capture_index.0);

            (&mut rhs[0], &mut lhs[spreader_index.0])
        };

        if capture.attempt_capture(spreader, body_index, &mut reality.physics_simulation.bodies) {
            reality
                .creatures_manager
                .captured_creatures
                .push(capture_index);

            //TODO: Setting as to whether we should auto open newly captured creatures?
            let new_window = game
                .renderer
                .create_window(&event_loop, &WindowConfig::default());

            reality
                .creatures_manager
                .creature_controlled_by_window
                .insert(new_window, capture_index);
        }
    }

    for (window_id, creature_index) in &reality.creatures_manager.creature_controlled_by_window {
        let creature = &reality.creatures_manager.creatures[creature_index.0];

        let WindowVariety::Creature(camera) = &mut game
            .renderer
            .window_specifics
            .get_mut(&window_id)
            .unwrap()
            .variety
        else {
            unreachable!()
        };

        creature.update_camera(camera, &reality.physics_simulation.bodies);

        camera.light_position[0] = camera.position[0];
        camera.light_position[2] = camera.position[2];

        //println!("camera: {:?}", camera);
    }
}

#[inline]
fn wasd_to_movement(wasd: [bool; 4]) -> [f32; 2] {
    match wasd {
        [true, false, false, false] => [0.0, -1.0],
        [false, false, true, false] => [0.0, 1.0],
        [false, false, false, true] => [-1.0, 0.0],
        [false, true, false, false] => [1.0, 0.0],

        [true, true, false, false] => [0.7, -0.7],
        [true, false, false, true] => [-0.7, -0.7],

        [false, true, true, false] => [0.7, 0.7],
        [false, false, true, true] => [-0.7, 0.7],

        _ => [0.0, 0.0],
    }
}

#[inline]
fn rotate_2d(movement: [f32; 2], theta: f32) -> [f32; 2] {
    let theta = theta.to_radians();
    let theta_cos = theta.cos();
    let theta_sin = theta.sin();

    [
        movement[0] * theta_cos - movement[1] * theta_sin,
        movement[1] * theta_cos + movement[0] * theta_sin,
    ]
}

// Direction? Clockwise? Anticlockwise? I don't know!
fn rotate_about_y(vector: [f32; 3], theta: f32) -> [f32; 3] {
    let theta = theta.to_radians();
    let theta_cos = theta.cos();
    let theta_sin = theta.sin();

    [
        vector[2] * theta_sin + vector[0] * theta_cos,
        vector[1],
        vector[2] * theta_cos - vector[0] * theta_sin,
    ]
}

fn rotate_about_z(vector: [f32; 3], theta: f32) -> [f32; 3] {
    let theta = theta.to_radians();
    let theta_cos = theta.cos();
    let theta_sin = theta.sin();

    [
        vector[0] * theta_cos - vector[1] * theta_sin,
        vector[0] * theta_sin + vector[1] * theta_cos,
        vector[2],
    ]
}

fn rotate_about_x(vector: [f32; 3], theta: f32) -> [f32; 3] {
    let theta = theta.to_radians();
    let theta_cos = theta.cos();
    let theta_sin = theta.sin();

    [
        vector[0],
        vector[1] * theta_cos + vector[2] * theta_sin,
        vector[1] * theta_sin + vector[2] * theta_cos,
    ]
}
