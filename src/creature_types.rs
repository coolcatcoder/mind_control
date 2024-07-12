use clunky::{
    math::{add_3d, normalise_2d, remap},
    physics::physics_3d::{aabb::AabbCentredOrigin, verlet::Particle},
};
use rand::{thread_rng, Rng};
use winit::event::KeyboardInput;

use crate::{
    body::{Body, Creature as CreatureBody},
    input::{ActionState, CreatureActions},
    renderer::{Camera3D, Renderer},
    rotate_about_x, rotate_about_y, BodyIndex, CreatureIndex,
};

pub enum CreatureType {
    Burgle(Burgle),
}

impl CreatureType {
    pub fn attempt_capture(
        &mut self,
        spreader: &mut CreatureType,
        body_index: BodyIndex,
        bodies: &mut Vec<Body>,
    ) -> bool {
        match self {
            CreatureType::Burgle(burgle) => true,
        }
    }

    pub fn accelerate(&mut self, acceleration: [f32; 3], bodies: &mut Vec<Body>) {
        match self {
            CreatureType::Burgle(burgle) => {
                let Body::Creature(body) = &mut bodies[burgle.body.0] else {
                    unreachable!()
                };

                body.particle.accelerate(acceleration);
            }
        }
    }

    pub fn on_physics_fixed_update_before_physics_tick_when_focused(
        &mut self,
        bodies: &mut Vec<Body>,
    ) {
        match self {
            CreatureType::Burgle(burgle) => {
                burgle.on_physics_fixed_update_before_physics_tick_when_focused(bodies);
            }
        }
    }

    pub fn on_keyboard_input(&mut self, input: KeyboardInput) {
        panic!("This shouldn't ever be called.")
        // match self {
        //     CreatureType::Burgle(burgle) => match input.virtual_keycode.unwrap() {
        //         VirtualKeyCode::W => burgle.wasd_held[0] = is_pressed(input.state),
        //         VirtualKeyCode::A => burgle.wasd_held[1] = is_pressed(input.state),
        //         VirtualKeyCode::S => burgle.wasd_held[2] = is_pressed(input.state),
        //         VirtualKeyCode::D => burgle.wasd_held[3] = is_pressed(input.state),
        //         VirtualKeyCode::Space => burgle.jump_held = is_pressed(input.state),

        //         VirtualKeyCode::F => {
        //             if is_pressed(input.state) {
        //                 burgle.sprinting = !burgle.sprinting;
        //             }
        //         }

        //         VirtualKeyCode::K => {
        //             if is_pressed(input.state) {
        //                 println!("{:?}", burgle)
        //             }
        //         }
        //         _ => (),
        //     },
        // }
    }

    pub fn on_mouse_motion(&mut self, delta: [f32; 2]) {
        match self {
            CreatureType::Burgle(burgle) => {
                burgle.rotation[0] -= delta[0];
                burgle.rotation[1] -= delta[1];
            }
        }
    }

    pub fn update_camera(&self, camera: &mut Camera3D, bodies: &[Body]) {
        match self {
            CreatureType::Burgle(burgle) => {
                let Body::Creature(body) = &bodies[burgle.body.0] else {
                    unreachable!()
                };

                camera.position = add_3d(body.particle.position, [0.0, -1.0, 0.0]);
                camera.rotation = burgle.rotation;
            }
        }
    }

    pub fn get_bodies_within_capture_range(&self, bodies: &[Body]) -> Vec<BodyIndex> {
        match self {
            CreatureType::Burgle(burgle) => {
                let Body::TriggerImmovableCuboid { collisions, .. } = &bodies[burgle.body.0 + 1]
                else {
                    unreachable!()
                };

                collisions.iter().map(|index| BodyIndex(*index)).collect()
            }
        }
    }

    pub fn actions_mut<'a>(&'a mut self) -> &'a mut CreatureActions {
        match self {
            CreatureType::Burgle(burgle) => &mut burgle.actions,
        }
    }

    pub fn update(
        &mut self,
        bodies: &[Body],
        captured_creatures: &[CreatureIndex],
        capture_attempts: &mut Vec<((CreatureIndex, BodyIndex), CreatureIndex)>,
    ) {
        match self {
            CreatureType::Burgle(burgle) => {
                burgle.update(bodies, captured_creatures, capture_attempts)
            }
        }
    }
}

#[derive(Debug)]
pub struct Burgle {
    creature_index: CreatureIndex,

    pub body: BodyIndex,

    actions: CreatureActions,

    // I know euler angles are bad, but it is fine for now...
    rotation: [f32; 3],

    walk_speed: f32,
    run_speed: f32,
    jump_acceleration: f32,
    // TODO: add flight like wyvern, and energy
    glide: bool,

    capture_index_selected: Option<(usize, BodyIndex)>,
    uncaptured_bodies_within_capture_range: Option<Vec<usize>>,
}

impl Burgle {
    pub fn new(
        renderer: &mut Renderer,
        bodies: &mut Vec<Body>,
        position: [f32; 3],
        half_size: [f32; 3],
        trigger_half_size: [f32; 3],
        colour: [f32; 4],

        index: CreatureIndex,
    ) -> Burgle {
        let body_index = bodies.len();

        bodies.push(Body::Creature(CreatureBody {
            particle: Particle::from_position(position),
            half_size,

            mass: 1.0,
            dampening: [0.9, 1.0, 0.9],

            grounded: false,

            owner: index,
        }));
        bodies.push(Body::TriggerImmovableCuboid {
            aabb: AabbCentredOrigin {
                position,
                half_size: trigger_half_size,
            },
            collisions: vec![],
        });

        //TODO: store for deletion.
        renderer.add_removable_cuboid_colour_from_body_index(body_index, colour);

        let mut rng = thread_rng();

        let walk_speed = rng.gen_range(25.0..50.0);

        Burgle {
            creature_index: index,

            body: BodyIndex(body_index),

            actions: Default::default(),

            rotation: [0.0; 3],

            walk_speed,
            run_speed: walk_speed + rng.gen_range(5.0..100.0),
            jump_acceleration: rng.gen_range(-1000.0..-300.0),

            glide: false,

            capture_index_selected: None,
            uncaptured_bodies_within_capture_range: None,
        }
    }

    fn on_physics_fixed_update_before_physics_tick_when_focused(&mut self, bodies: &mut Vec<Body>) {
        let Body::Creature(body) = &mut bodies[self.body.0] else {
            unreachable!()
        };

        let mut motion = normalise_2d([self.actions.movement[0], self.actions.movement[2]]);

        if body.grounded {
            self.glide = false;
        }

        if self.glide {
            body.particle.accelerate([0.0, -45.0, 0.0]); // -45

            if self.actions.movement[1] == -1.0 {
                body.particle.accelerate([0.0, -10.0, 0.0]);
            }

            let speed_multiplier = remap(
                self.actions.speed_modifier,
                0.0..1.0,
                // Temporary.
                self.walk_speed..self.run_speed,
            );

            motion[0] *= speed_multiplier;
            motion[1] *= speed_multiplier;

            let mut motion = [-motion[0], 0.0, -motion[1]];

            motion = rotate_about_x(motion, self.rotation[0]); // idk if this works. Don't think it does.
            motion = rotate_about_y(motion, -self.rotation[1]);

            //motion = rotate_about_z(motion, self.rotation[2]); // idk if this works. Don't think it does.
            // Consider rotating displacement.
            // consider changing order of everything.

            body.particle.accelerate(motion);

            println!("Motion: {:?}", motion);

            return;
        }

        let speed_multiplier = remap(
            self.actions.speed_modifier,
            0.0..1.0,
            self.walk_speed..self.run_speed,
        );
        //println!("walk: {},\nrun: {},\nspeed_multiplier: {},", self.walk_speed, self.run_speed, speed_multiplier);

        motion[0] *= speed_multiplier;
        motion[1] *= speed_multiplier;

        //motion = rotate_2d(motion, self.rotation[1]);
        motion = {
            let temp = rotate_about_y([-motion[0], 0.0, -motion[1]], -self.rotation[1]);
            [temp[0], temp[2]]
        };

        if self.actions.movement[1] == -1.0 {
            if body.grounded {
                body.particle.accelerate([0.0, self.jump_acceleration, 0.0]);
            }
        }

        body.particle.accelerate([motion[0], 0.0, motion[1]]);
    }

    fn update(
        &mut self,
        bodies: &[Body],
        captured_creatures: &[CreatureIndex],
        capture_attempts: &mut Vec<((CreatureIndex, BodyIndex), CreatureIndex)>,
    ) {
        if self.actions.move_selection != 0 {
            self.move_capture_selection();
            self.actions.move_selection = 0;
        }

        if self.actions.primary_interact {
            self.actions.primary_interact = false;

            let Body::Creature(body) = &bodies[self.body.0] else {
                unreachable!()
            };

            if !body.grounded {
                self.glide = !self.glide;
                println!("Glide!")
            }
        }

        match self.actions.capture {
            ActionState::Do => {
                let Body::TriggerImmovableCuboid { collisions, .. } = &bodies[self.body.0 + 1]
                else {
                    unreachable!()
                };

                let mut uncaptured_bodies_within_capture_range =
                    Vec::with_capacity(collisions.len());

                for body_index in collisions {
                    if !captured_creatures.contains(&bodies[*body_index].owner().unwrap()) {
                        uncaptured_bodies_within_capture_range.push(*body_index)
                    }
                }

                uncaptured_bodies_within_capture_range.sort();

                // Check if the index.0 is invalid in any way, then always search to see if index.1 is still in the bodies, and if so, correct index.0
                if let Some(capture_index_selected) = self.capture_index_selected {
                    let mut invalid_index = false;

                    if capture_index_selected.0 >= uncaptured_bodies_within_capture_range.len() {
                        invalid_index = true;
                    } else if uncaptured_bodies_within_capture_range[capture_index_selected.0]
                        != capture_index_selected.1 .0
                    {
                        invalid_index = true;
                    }

                    if invalid_index {
                        if let Ok(new_index) = uncaptured_bodies_within_capture_range
                            .binary_search(&capture_index_selected.1 .0)
                        {
                            self.capture_index_selected =
                                Some((new_index, capture_index_selected.1));
                        } else {
                            self.capture_index_selected = None;
                        }
                    }
                }

                self.uncaptured_bodies_within_capture_range =
                    Some(uncaptured_bodies_within_capture_range);
            }
            ActionState::End => {
                println!("Capture key released!");
                println!("capture_index_selected: {:?}", self.capture_index_selected);

                if let Some(capture_index_selected) = self.capture_index_selected {
                    let capture_creature_index =
                        bodies[capture_index_selected.1 .0].owner().unwrap();

                    println!("owner: {:?}", capture_creature_index);
                    println!("self index: {:?}", self.creature_index);

                    capture_attempts.push((
                        (capture_creature_index, capture_index_selected.1),
                        self.creature_index,
                    ));
                }

                self.capture_index_selected = None;
                self.uncaptured_bodies_within_capture_range = None;
                self.actions.capture = ActionState::None;
            }
            ActionState::None => (),
        }
    }

    fn move_capture_selection(&mut self) {
        let Some(uncaptured_bodies_within_capture_range) =
            &self.uncaptured_bodies_within_capture_range
        else {
            println!("test");
            return;
        };
        println!(
            "bodies_within_capture_range: {:?}",
            uncaptured_bodies_within_capture_range
        );

        if uncaptured_bodies_within_capture_range.len() != 0 {
            if let Some(ref mut capture_index_selected) = &mut self.capture_index_selected {
                // Use isize temporarily to handle negative selection moves.
                let mut capture_index_selected_as_isize = capture_index_selected.0 as isize;
                capture_index_selected_as_isize += self.actions.move_selection as isize;
                //println!("capture_index_selected_as_isize:{}", capture_index_selected_as_isize);
                capture_index_selected_as_isize = capture_index_selected_as_isize
                    .rem_euclid(uncaptured_bodies_within_capture_range.len() as isize + 1);
                //println!("capture_index_selected_as_isize:{}", capture_index_selected_as_isize);
                capture_index_selected.0 = capture_index_selected_as_isize as usize;

                if capture_index_selected.0 == uncaptured_bodies_within_capture_range.len() {
                    self.capture_index_selected = None;
                } else {
                    capture_index_selected.1 =
                        BodyIndex(uncaptured_bodies_within_capture_range[capture_index_selected.0]);
                }
            } else {
                self.capture_index_selected =
                    Some((0, BodyIndex(uncaptured_bodies_within_capture_range[0])));
            }

            println!("capture_index_selected: {:?}", self.capture_index_selected);
        }
    }
}
