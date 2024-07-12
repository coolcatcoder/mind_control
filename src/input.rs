use clunky::{lost_code::is_pressed, math::remap};
use gilrs::{EventType, Gilrs};
use winit::event::KeyboardInput;
// Up to the bindings to use stuff as press and hold, or toggle.

pub struct InputManager {
    pub gilrs: Gilrs,

    pub active_bindings: usize,
    // Multiple so people can easily switch between bindings.
    pub all_bindings: Vec<Bindings>,
    //TODO: add default bindings when switching to controllers or keyboards, with specific names.
    // Can search through them with a menu.
}

// A new bindings manager, that has actions, and then a vec for each. This vec contains the bindings allowed.
// For (Binding, bool) it means that it is optionally toggleable
#[derive(Default)]
pub struct Bindings {
    // GAME ACTIONS
    full_screen: Vec<Binding>,

    log_fps: Vec<Binding>,

    // REALITY ACTIONS
    pause: Vec<Binding>,

    // CREATURE ACTIONS
    movement_central: Vec<ButtonsOrAxis>,
    movement_sideways: Vec<ButtonsOrAxis>,
    movement_vertical: Vec<ButtonsOrAxis>,

    capture: Vec<BindingWithOptionalToggle>,

    speed_modifier: Vec<BindingWithOptionalToggle>,

    primary_interact: Vec<BindingWithOptionalToggle>,
    secondary_interact: Vec<BindingWithOptionalToggle>,
    // TODO: Something to do with moving selection?
}

impl Bindings {
    pub fn modify_game_actions_with_keyboard_input(
        &self,
        actions: &mut GameActions,
        input: KeyboardInput,
    ) {
        let state = input.state;
        let code = input.scancode;
        println!("Keyboard code: {code}");

        if self.full_screen.contains(&Binding::KeyboardBinding(code)) {
            if is_pressed(state) {
                actions.full_screen = true;
            }
        } else if self.log_fps.contains(&Binding::KeyboardBinding(code)) {
            if is_pressed(state) {
                actions.log_fps = true;
            }
        }
    }

    pub fn modify_reality_actions_with_keyboard_input(
        &self,
        actions: &mut RealityActions,
        input: KeyboardInput,
    ) {
        let state = input.state;
        let code = input.scancode;

        if self.pause.contains(&Binding::KeyboardBinding(code)) {
            if is_pressed(state) {
                actions.paused = !actions.paused;
            }
        }
    }

    pub fn modify_creature_actions_with_keyboard_input(
        &self,
        actions: &mut CreatureActions,
        input: KeyboardInput,
    ) {
        let state = input.state;
        let code = input.scancode;

        for binding in &self.movement_central {
            if let ButtonsOrAxis::Buttons {
                button_negative,
                button_positive,
                toggle,
            } = binding
            {
                if *button_negative == Binding::KeyboardBinding(code) {
                    if *toggle {
                        if is_pressed(state) {
                            actions.movement[2] = if actions.movement[2] == 0.0 {
                                -1.0
                            } else {
                                0.0
                            };
                        }
                    } else {
                        actions.movement[2] = if is_pressed(state) { -1.0 } else { 0.0 }
                    }

                    return;
                } else if *button_positive == Binding::KeyboardBinding(code) {
                    if *toggle {
                        if is_pressed(state) {
                            actions.movement[2] =
                                if actions.movement[2] == 0.0 { 1.0 } else { 0.0 };
                        }
                    } else {
                        actions.movement[2] = if is_pressed(state) { 1.0 } else { 0.0 }
                    }

                    return;
                }
            }
        }

        for binding in &self.movement_sideways {
            if let ButtonsOrAxis::Buttons {
                button_negative,
                button_positive,
                toggle,
            } = binding
            {
                if *button_negative == Binding::KeyboardBinding(code) {
                    if *toggle {
                        if is_pressed(state) {
                            actions.movement[0] = if actions.movement[0] == 0.0 {
                                -1.0
                            } else {
                                0.0
                            };
                        }
                    } else {
                        actions.movement[0] = if is_pressed(state) { -1.0 } else { 0.0 }
                    }

                    return;
                } else if *button_positive == Binding::KeyboardBinding(code) {
                    if *toggle {
                        if is_pressed(state) {
                            actions.movement[0] =
                                if actions.movement[0] == 0.0 { 1.0 } else { 0.0 };
                        }
                    } else {
                        actions.movement[0] = if is_pressed(state) { 1.0 } else { 0.0 }
                    }

                    return;
                }
            }
        }
    }

    // Makes all the vecs take up as little room as possible.
    pub fn shrink_to_fit(&mut self) {
        self.full_screen.shrink_to_fit();

        println!("Todo shrink to fit properly.");
    }

    pub fn add_australian_keyboard_default(&mut self) {
        self.full_screen.push(Binding::KeyboardBinding(43)); // \

        self.log_fps.push(Binding::KeyboardBinding(27)); // ]

        self.pause.push(Binding::KeyboardBinding(1)); // escape

        self.movement_central.push(ButtonsOrAxis::Buttons {
            button_negative: Binding::KeyboardBinding(31), // s
            button_positive: Binding::KeyboardBinding(17), // w
            toggle: false,
        });

        self.movement_sideways.push(ButtonsOrAxis::Buttons {
            button_negative: Binding::KeyboardBinding(30), // a
            button_positive: Binding::KeyboardBinding(32), // d
            toggle: false,
        });

        self.movement_sideways.push(ButtonsOrAxis::Buttons {
            button_negative: Binding::KeyboardBinding(57), // space
            button_positive: Binding::KeyboardBinding(u32::MAX),
            toggle: false,
        });

        self.shrink_to_fit();
    }
}

#[derive(Debug, PartialEq)]
enum Binding {
    KeyboardBinding(u32),
}

#[derive(Debug, PartialEq)]
enum BindingWithOptionalToggle {
    KeyboardBinding(u32, bool),
}

#[derive(Default, Debug)]
pub struct GameActions {
    pub full_screen: bool,

    // Debug:
    pub log_fps: bool,
}

#[derive(Default, Debug)]
pub struct RealityActions {
    pub paused: bool,
}

#[derive(Default, Debug)]
pub struct CreatureActions {
    // Local space. Vertical should only be -1.0 or 1.0.
    pub movement: [f32; 3],

    pub capture: ActionState,

    // from 0-1
    pub speed_modifier: f32,

    pub primary_interact: bool,
    pub secondary_interact: bool,

    pub move_selection: i8,
}

#[derive(Default, Debug)]
pub struct MenuActions {
    // For navigating the menu.
    movement: [bool; 2],

    confirm: bool,
    // Can also be used as a back button.
    deny: bool,
}

/// None, is no action needs to be done.
/// Do, is to do the action.
/// End, is to stop doing the action, perhaps perform cleanup, and then return to None.
#[derive(Default, Debug)]
pub enum ActionState {
    #[default]
    None,
    Do,
    End,
}

// Could be some buttons, or it could be an axis on a joystick.
pub enum ButtonsOrAxis {
    Buttons {
        button_negative: Binding,
        button_positive: Binding,
        toggle: bool,
    },
    Axis(u32),
}

// So inflexible. If only there was a better way...
pub enum GamepadMovement<T> {
    Stick([T; 2]),
    Buttons([T; 4]),
}

pub struct GamepadCodeBindings {
    horizontal_movement: GamepadMovement<u32>,

    // Should be stick or buttons.
    up_movement: u32,
    down_movement: u32,

    capture: (u32, bool),

    speed_modifier: (u32, bool),

    primary_interact: u32,
    secondary_interact: u32,

    // Should be stick or buttons.
    positive_move_selection: u32,
    negative_move_selection: u32,

    full_screen: u32,
}

impl GamepadCodeBindings {
    #[rustfmt::skip]
    pub fn modify_creature_actions_with_gamepad_event_type(
        &mut self,
        input: EventType,
        creature_actions: &mut CreatureActions,
    ) {
        match input {
            EventType::ButtonPressed(_, code) => {
                let code = code.into_u32();
                println!("ButtonPressed({})", code);

                if code == self.up_movement {
                    creature_actions.movement[1] = -1.0;
                }

                else if code == self.capture.0 {
                    if self.capture.1 {
                        creature_actions.capture = match creature_actions.capture {
                            ActionState::Do => ActionState::End,
                            ActionState::None => ActionState::Do,
                            ActionState::End => {
                                println!("End should always be used. This should never happen.");
                                ActionState::End
                            }
                        };
                    } else {
                        creature_actions.capture = ActionState::Do;
                    }
                }

                else if code == self.speed_modifier.0 {
                    if self.speed_modifier.1 {
                        creature_actions.speed_modifier = if creature_actions.speed_modifier == 1.0 {
                            0.0
                        } else {
                            1.0
                        };
                    } else {
                        creature_actions.speed_modifier = 1.0;
                    }
                }

                else if code == self.positive_move_selection {
                    creature_actions.move_selection = 1;
                } else if code == self.negative_move_selection {
                    creature_actions.move_selection = -1;
                }
            }
            EventType::ButtonReleased(_, code) => {
                let code = code.into_u32();
                println!("ButtonReleased({})", code);

                if code == self.up_movement {
                    creature_actions.movement[1] = 0.0;
                }

                else if code == self.capture.0 && !self.capture.1 {
                    creature_actions.capture = ActionState::End;
                }

                else if code == self.speed_modifier.0 && !self.speed_modifier.1 {
                    creature_actions.speed_modifier = 0.0;
                }
            }

            EventType::AxisChanged(_, value, code) => {
                let code = code.into_u32();
                println!("AxisChanged(code:{},value:{})", code, value);

                if code == self.speed_modifier.0 && !self.speed_modifier.1 {
                    creature_actions.speed_modifier = remap(-value, -1.0..1.0, 0.0..1.0);
                }

                else if let GamepadMovement::Stick(horizontal_movement) = self.horizontal_movement {
                    if code == horizontal_movement[0] {
                        creature_actions.movement[0] = -value;
                    } else if code == horizontal_movement[1] {
                        creature_actions.movement[2] = -value;
                    }
                }
            }
            _ => (),
        }
    }

    pub fn default_attack() -> Self {
        Self {
            horizontal_movement: GamepadMovement::Stick([196608, 196609]),

            up_movement: 65824,
            down_movement: 0,

            capture: (65826, true),

            speed_modifier: (196610, false),
            //speed_modifier: (65832, true),
            primary_interact: 0,
            secondary_interact: 0,

            positive_move_selection: 65828,
            negative_move_selection: 65827,

            full_screen: 0,
        }
    }
}

impl Default for GamepadCodeBindings {
    fn default() -> Self {
        Self {
            horizontal_movement: GamepadMovement::Stick([196608, 196609]),

            up_movement: 65824,
            down_movement: 0,

            capture: (65826, true),

            speed_modifier: (196610, false),
            //speed_modifier: (65832, true),
            primary_interact: 0,
            secondary_interact: 0,

            positive_move_selection: 65828,
            negative_move_selection: 65827,

            full_screen: 0,
        }
    }
}

pub struct ScanCodeBindings {
    forwards_movement: u32,
    backwards_movement: u32,
    left_movement: u32,
    right_movement: u32,
    up_movement: u32,
    down_movement: u32,

    capture: u32,

    speed_modifier: u32,

    primary_interact: u32,
    secondary_interact: u32,

    positive_move_selection: u32,
    negative_move_selection: u32,

    full_screen: u32,
    pause: u32,

    log_fps: u32,
}

impl ScanCodeBindings {
    #[rustfmt::skip]
    pub fn modify_creature_actions_with_keyboard_input(
        &mut self,
        input: KeyboardInput,
        creature_actions: &mut CreatureActions,
    ) {
        //println!("scancode: {}", input.scancode);

        if input.scancode == self.forwards_movement {
            if is_pressed(input.state) {
                creature_actions.movement[2] = 1.0;
            } else {
                creature_actions.movement[2] = 0.0;
            }
        } else if input.scancode == self.backwards_movement {
            if is_pressed(input.state) {
                creature_actions.movement[2] = -1.0;
            } else {
                creature_actions.movement[2] = 0.0;
            }
        } else if input.scancode == self.left_movement {
            if is_pressed(input.state) {
                creature_actions.movement[0] = -1.0;
            } else {
                creature_actions.movement[0] = 0.0;
            }
        } else if input.scancode == self.right_movement {
            if is_pressed(input.state) {
                creature_actions.movement[0] = 1.0;
            } else {
                creature_actions.movement[0] = 0.0;
            }
        } else if input.scancode == self.up_movement {
            if is_pressed(input.state) {
                creature_actions.movement[1] = -1.0;
            } else {
                creature_actions.movement[1] = 0.0;
            }
        } else if input.scancode == self.down_movement {
            if is_pressed(input.state) {
                creature_actions.movement[1] = 1.0;
            } else {
                creature_actions.movement[1] = 0.0;
            }
        }

        else if input.scancode == self.capture {
            if is_pressed(input.state) {
                creature_actions.capture = ActionState::Do;
            } else {
                creature_actions.capture = ActionState::End;
            }
        }

        else if input.scancode == self.positive_move_selection {
            if is_pressed(input.state) {
                creature_actions.move_selection = 1;
            }
        } else if input.scancode == self.negative_move_selection {
            if is_pressed(input.state) {
                creature_actions.move_selection = -1;
            }
        }
    }
}

impl Default for ScanCodeBindings {
    fn default() -> Self {
        Self {
            forwards_movement: 17,
            backwards_movement: 31,
            left_movement: 30,
            right_movement: 32,
            up_movement: 57,
            down_movement: 42,

            capture: 46,

            speed_modifier: 33,

            primary_interact: 19,
            secondary_interact: 20,

            positive_move_selection: 16,
            negative_move_selection: 18,

            full_screen: 43,
            pause: 1,

            log_fps: 25,
        }
    }
}
