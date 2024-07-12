use clunky::{
    math::{direction_3d_to_signed_number_3d, mul_3d, mul_3d_by_1d, neg_3d, Direction},
    physics::physics_3d::{
        aabb::AabbCentredOrigin, bodies, calculate_collision_impulse,
        calculate_collision_impulse_with_immovable_rhs, verlet::Particle,
    },
};

use crate::CreatureIndex;

// TODO: Keep track of which grid cells actually have something in them, only check those.
#[derive(Debug, Clone)]
pub enum Body {
    Creature(Creature),
    ImmovableCuboid(AabbCentredOrigin<f32>),
    TriggerImmovableCuboid {
        aabb: AabbCentredOrigin<f32>,
        collisions: Vec<usize>,
    },
    None,
}

impl Body {
    pub fn owner(&self) -> Result<CreatureIndex, &'static str> {
        match self {
            Body::Creature(creature) => Ok(creature.owner),
            _ => Err("This body does not have an owner"),
        }
    }
}

impl bodies::Body<f32> for Body {
    #[inline]
    fn update(&mut self, gravity: [f32; 3], _dampening: [f32; 3], delta_time: f32) {
        match self {
            Body::Creature(creature) => creature.update(gravity, delta_time),
            Body::TriggerImmovableCuboid { collisions, .. } => collisions.clear(),
            _ => {}
        }
    }

    #[inline]
    fn position_unchecked(&self) -> [f32; 3] {
        match self {
            Body::Creature(creature) => creature.particle.position,
            Body::ImmovableCuboid(immovable_cuboid) => immovable_cuboid.position,
            Body::TriggerImmovableCuboid { aabb, .. } => aabb.position,
            Body::None => unreachable!(),
        }
    }

    #[inline]
    fn half_size_unchecked(&self) -> [f32; 3] {
        match self {
            Body::Creature(creature) => creature.half_size,
            Body::ImmovableCuboid(immovable_cuboid) => immovable_cuboid.half_size,
            Body::TriggerImmovableCuboid { aabb, .. } => aabb.half_size,
            Body::None => unreachable!(),
        }
    }

    #[inline]
    fn is_none(&self) -> bool {
        match self {
            Body::None => true,
            _ => false,
        }
    }

    // terribly named, but very useful.
    #[inline]
    fn collide_with_others(&self) -> bool {
        match self {
            Body::Creature(_) => true,
            Body::ImmovableCuboid(_) | Body::TriggerImmovableCuboid { .. } => false,
            Body::None => unreachable!(),
        }
    }

    #[inline]
    fn detect_collision(&self, other: &Body) -> bool {
        let colliding_bodies = (self, other);
        match colliding_bodies {
            // CREATURE
            (Body::Creature(lhs_creature), Body::Creature(rhs_creature)) => lhs_creature
                .aabb()
                .is_intersected_by_aabb(rhs_creature.aabb()),
            (Body::Creature(lhs_creature), Body::ImmovableCuboid(rhs_immovable_cuboid)) => {
                lhs_creature
                    .aabb()
                    .is_intersected_by_aabb(*rhs_immovable_cuboid)
            }
            (Body::Creature(lhs_creature), Body::TriggerImmovableCuboid { aabb, .. }) => {
                lhs_creature.aabb().is_intersected_by_aabb(*aabb)
            }

            _ => unreachable!(),
        }
    }

    #[inline]
    fn respond_to_collision(
        &mut self,
        other: &mut Body,
        lhs_index: usize,
        rhs_index: usize,
        delta_time: f32,
    ) {
        let colliding_bodies = (self, other);
        match colliding_bodies {
            // CREATURE
            (Body::Creature(lhs_creature), Body::Creature(rhs_creature)) => {
                let (collision_normal, penetration) = lhs_creature
                    .aabb()
                    .get_collision_normal_and_penetration(&rhs_creature.aabb());
                let collision_normal_signed_number =
                    direction_3d_to_signed_number_3d(collision_normal);
                let collision_translation =
                    mul_3d_by_1d(collision_normal_signed_number, -penetration * 0.5);

                lhs_creature
                    .particle
                    .apply_uniform_position_change(collision_translation);
                rhs_creature
                    .particle
                    .apply_uniform_position_change(neg_3d(collision_translation));

                if Direction::Positive == collision_normal[1] {
                    lhs_creature.grounded = true;
                }

                let impulse = calculate_collision_impulse(
                    lhs_creature.particle.calculate_velocity(delta_time),
                    lhs_creature.mass,
                    rhs_creature.particle.calculate_velocity(delta_time),
                    rhs_creature.mass,
                    collision_normal_signed_number,
                    0.5,
                );
                lhs_creature.particle.apply_impulse(impulse, delta_time);
                rhs_creature
                    .particle
                    .apply_impulse(neg_3d(impulse), delta_time);
            }
            (Body::Creature(lhs_creature), Body::ImmovableCuboid(rhs_immovable_cuboid)) => {
                let (collision_normal, penetration) = lhs_creature
                    .aabb()
                    .get_collision_normal_and_penetration(rhs_immovable_cuboid);
                let collision_normal_signed_number =
                    direction_3d_to_signed_number_3d(collision_normal);
                let collision_translation =
                    mul_3d_by_1d(collision_normal_signed_number, -penetration);

                lhs_creature
                    .particle
                    .apply_uniform_position_change(collision_translation);

                if Direction::Positive == collision_normal[1] {
                    lhs_creature.grounded = true;
                }

                // TODO: investigate stepping up onto small ledges. This could react unpredictably with collision_normal and penetration.
                /*
                let step_up = ((lhs_player.particle.position[1] + lhs_player.half_size[1])
                    - (rhs_immovable_cuboid.aabb.position[1]
                        - rhs_immovable_cuboid.aabb.half_size[1]))
                    < T::from_f32(0.5);
                */

                let impulse = calculate_collision_impulse_with_immovable_rhs(
                    lhs_creature.particle.calculate_velocity(delta_time),
                    lhs_creature.mass,
                    collision_normal_signed_number,
                    0.5,
                );
                //println!("impulse: {:?}", impulse);
                lhs_creature.particle.apply_impulse(impulse, delta_time);
            }
            (Body::Creature(_), Body::TriggerImmovableCuboid { collisions, .. }) => {
                collisions.push(lhs_index);
            }
            _ => unreachable!(),
        }
    }
}

// TODO: List of common shaps I want to include here. But first, a naming scheme. No rotation should be by default. Axis aligned should be the default. At least 1 particle should be the default. As such "cuboid" should refer to an axis aligned cuboid with a single particle that can't rotate.
// List: Cuboid, ImmovableCuboid, Sphere, ImmovableSphere, Player, Cylinder, ImmovableCylinder

#[derive(Debug, Clone)]
pub struct Creature {
    pub particle: Particle<f32>,
    pub half_size: [f32; 3],

    pub mass: f32,
    pub dampening: [f32; 3],

    pub grounded: bool,

    pub owner: CreatureIndex,
}

impl Creature {
    #[inline]
    fn update(&mut self, gravity: [f32; 3], delta_time: f32) {
        self.particle.accelerate(gravity);
        self.particle.update(
            delta_time,
            mul_3d(self.particle.calculate_displacement(), self.dampening),
        );

        self.grounded = false;
    }

    #[inline]
    pub fn aabb(&self) -> AabbCentredOrigin<f32> {
        AabbCentredOrigin {
            position: self.particle.position,
            half_size: self.half_size,
        }
    }
}
