use std::collections::HashMap;

use clunky::{
    physics::physics_2d::aabb::AabbCentredOrigin,
    shaders::{instanced_text_sdf::Instance as LetterInstance, instanced_unlit_uv_2d_stretch},
};
use vulkano_util::window::WindowDescriptor;
use winit::{event_loop::EventLoopWindowTarget, window::WindowId};

use crate::{
    renderer::{Renderer, WindowConfig, WindowVariety},
    CreaturesManager, Game,
};

use serde::Deserialize;
use serde::Serialize;

use ahash::{AHasher, RandomState};

const BOOK_FONT_MSDF: &[u8] = include_bytes!("fonts/book.png");
const BOOK_FONT_JSON: &[u8] = include_bytes!("fonts/book.json");

pub struct MenuManager {
    pub window: Option<WindowId>,

    menu: Menu,

    buttons: Vec<Button>,
}

impl MenuManager {
    pub fn new() -> Self {
        Self {
            window: None,

            menu: Menu::None,

            buttons: Vec::new(),
        }
    }

    pub fn menu(&self) -> Menu {
        self.menu
    }

    pub fn set_menu(
        &mut self,
        menu: Menu,
        renderer: &mut Renderer,
        event_loop: &EventLoopWindowTarget<()>,
    ) {
        self.buttons.clear();
        renderer.buffers.menu_uv_instances.clear();
        renderer.buffers.menu_text_instances.clear();

        // It will never go from Menu::None to Menu::None, so this is absolutely fine.
        if matches!(self.window, None) {
            self.window = Some(renderer.create_window(
                &event_loop,
                &WindowConfig {
                    variety: WindowVariety::Menu,
                    window_descriptor: WindowDescriptor {
                        ..Default::default()
                    },
                    swapchain_create_info_modify: |_| {},
                },
            ));
        }

        match menu {
            Menu::None => {
                renderer.remove_window(self.window.unwrap());
                self.window = None;
            }
            Menu::Main => {
                renderer
                    .buffers
                    .menu_text_instances
                    .push(LetterInstance::new(
                        [0.0, 0.0],
                        [1.0, 0.0, 1.0, 1.0],
                        0.01,
                        0.2,
                        glam::Affine2::IDENTITY,
                    ));

                renderer.buffers.menu_uv_instances.push(
                    instanced_unlit_uv_2d_stretch::Instance::new(
                        [0.0, 0.0],
                        0.0,
                        glam::Affine2::IDENTITY,
                    ),
                );

                const TEMP_START_AABB: AabbCentredOrigin<f32> = AabbCentredOrigin {
                    position: [0.0, 0.7],
                    half_size: [0.5, 0.25],
                };

                self.buttons.push(Button {
                    aabb: TEMP_START_AABB,

                    on_start_hover: |renderer| {
                        renderer.buffers.menu_uv_instances[1].set_model_to_world(
                            glam::Affine2::from_translation(TEMP_START_AABB.position.into())
                                * glam::Affine2::from_scale(glam::Vec2::from(
                                    TEMP_START_AABB.half_size,
                                )),
                        );
                    },
                    on_end_hover: |renderer| {
                        renderer.buffers.menu_uv_instances[1].set_model_to_world(
                            glam::Affine2::from_translation(TEMP_START_AABB.position.into())
                                * glam::Affine2::from_scale(
                                    glam::Vec2::from(TEMP_START_AABB.half_size) * 2.0,
                                ),
                        );
                    },
                    ..Default::default()
                });

                renderer.buffers.menu_uv_instances.push(
                    instanced_unlit_uv_2d_stretch::Instance::new(
                        [0.0, 0.0],
                        0.0,
                        glam::Affine2::IDENTITY,
                    ),
                );

                (self.buttons[0].on_end_hover)(renderer);
            }
        }

        self.menu = menu;
        renderer.buffers.menu_text_instances.shrink_to_fit();
        renderer.buffers.menu_uv_instances.shrink_to_fit();
        self.buttons.shrink_to_fit();

        if matches!(menu, Menu::None) {
            return;
        }

        let extent = renderer
            .windows_manager
            .get_window(self.window.unwrap())
            .unwrap()
            .inner_size()
            .into();

        self.resize(extent, renderer);
    }

    /// Call when the menu window resizes.
    pub fn resize(&mut self, extent: [f32; 2], renderer: &mut Renderer) {
        match self.menu {
            Menu::None => unreachable!(),
            Menu::Main => renderer.buffers.menu_text_instances[0].set_model_to_world(
                // These are constants... These should not change on resize. It will always be -1,-1 to 1,1.
                // Get rid of this. Only put stuff that uses the aspect ratio and suchlike in here.
                glam::Affine2::from_translation([0.0, 0.0].into())
                    * glam::Affine2::from_scale([0.25, 0.25].into()),
            ),
        }
    }

    /// Position should be from -1,-1 to 1,1, I think?
    pub fn on_cursor_moved(&mut self, position: [f32; 2], renderer: &mut Renderer) {
        for button in &mut self.buttons {
            if button.aabb.is_intersected_by_point(position) {
                if !button.hovered {
                    button.hovered = true;
                    println!("on start hover");
                    (button.on_start_hover)(renderer);
                }
            } else if button.hovered {
                button.hovered = false;
                println!("on end hover");
                (button.on_end_hover)(renderer);
            }
        }

        // Temp
        renderer.buffers.menu_uv_instances[0].set_model_to_world(
            glam::Affine2::from_translation(position.into())
                * glam::Affine2::from_scale([0.25, 0.25].into()),
        )
    }

    pub fn on_click(&mut self) {
        for button in &mut self.buttons {
            if button.hovered {
                println!("pressed button");
                // Most buttons will immediately use the pressed bool to do something.
                // Some buttons can use it as a toggle though.
                button.pressed = !button.pressed;
            }
        }
    }

    pub fn buttons(&self) -> &[Button] {
        &self.buttons
    }
}

// Current plan: the main window should have the main menu, settings, pause menu everything like that.
// Once you boot a reality, it closes, and you have a selection menu instead, but by pausing, or clicking a settings icon, you can reopen the menu.

pub fn blah() {
    let book_font = SdfFont::new(BOOK_FONT_JSON);

    //println!("v,a kerning {}", book_font.get_kerning('v', 'a'));
}

pub struct TextManager {}

impl TextManager {
    pub fn new() -> Self {
        TextManager {}
    }

    pub fn on_selection_menu_resize(
        extent: [f32; 2],
        letters: &mut Vec<LetterInstance>,
        creatures_manager: &CreaturesManager,
    ) {
        letters.clear();

        let aspect_ratio = extent[0] / extent[1];

        let mut y = -1.0 / aspect_ratio;

        for creature_index in &creatures_manager.captured_creatures {
            y += 0.3 * aspect_ratio;

            letters.push(LetterInstance::new(
                [0.0, 0.0],
                [1.0, 0.0, 1.0, 1.0],
                0.01,
                0.2,
                glam::Affine2::from_translation([0.0, y].into())
                    * glam::Affine2::from_scale([0.25, 0.25].into()),
            ))
        }

        letters.shrink_to_fit();
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Menu {
    None,
    Main,
}

pub struct Button {
    aabb: AabbCentredOrigin<f32>,

    pub hovered: bool,
    // This will probably be set by letting go, while hovered over the button, rather than when pressed.
    // That way, the user can press, hold, and drag the mouse off the button, to cancel the press.
    pub pressed: bool,

    on_start_hover: fn(renderer: &mut Renderer),
    on_end_hover: fn(renderer: &mut Renderer),
}

impl Default for Button {
    fn default() -> Self {
        Self {
            aabb: AabbCentredOrigin {
                position: [0.0, 0.0],
                half_size: [0.5, 0.5],
            },

            hovered: false,
            pressed: false,

            on_start_hover: |_| {},
            on_end_hover: |_| {},
        }
    }
}

pub struct SdfFont {
    pub json: SdfFontJson,

    kerning_map: HashMap<(u32, u32), f32, RandomState>,
}

impl SdfFont {
    pub fn new(json_bytes: &[u8]) -> Self {
        let json: SdfFontJson = serde_json::from_slice(json_bytes).unwrap();

        let mut kerning_map = HashMap::default();

        for kerning in &json.kerning {
            // println!(
            //     "\nunicode1 ({},{})",
            //     kerning.unicode1,
            //     char::from_u32(kerning.unicode1.try_into().unwrap()).unwrap()
            // );
            // println!(
            //     "unicode2 ({},{})",
            //     kerning.unicode2,
            //     char::from_u32(kerning.unicode2.try_into().unwrap()).unwrap()
            // );
            // println!("advance {}", kerning.advance);

            kerning_map.insert((kerning.unicode1, kerning.unicode2), kerning.advance);
        }

        kerning_map.shrink_to_fit();

        Self { json, kerning_map }
    }

    pub fn get_kerning(&self, char1: char, char2: char) -> f32 {
        match self.kerning_map.get(&(char1 as u32, char2 as u32)) {
            Some(advance) => *advance,
            None => todo!("Should be some default kerning."),
        }
    }
}

// generated by https://transform.tools/json-to-rust-serde

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SdfFontJson {
    pub atlas: Atlas,
    pub metrics: Metrics,
    pub glyphs: Vec<Glyph>,
    pub kerning: Vec<Kerning>,
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Atlas {
    #[serde(rename = "type")]
    pub type_field: String,
    pub distance_range: i64,
    pub distance_range_middle: i64,
    pub size: f64,
    pub width: i64,
    pub height: i64,
    pub y_origin: String,
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Metrics {
    pub em_size: i64,
    pub line_height: f64,
    pub ascender: f64,
    pub descender: f64,
    pub underline_y: f64,
    pub underline_thickness: f64,
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Glyph {
    pub unicode: i64,
    pub advance: f64,
    pub plane_bounds: Option<PlaneBounds>,
    pub atlas_bounds: Option<AtlasBounds>,
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PlaneBounds {
    pub left: f64,
    pub bottom: f64,
    pub right: f64,
    pub top: f64,
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AtlasBounds {
    pub left: f64,
    pub bottom: f64,
    pub right: f64,
    pub top: f64,
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Kerning {
    pub unicode1: u32,
    pub unicode2: u32,
    pub advance: f32,
}
