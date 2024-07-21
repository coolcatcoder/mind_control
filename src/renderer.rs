use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
    slice::SliceIndex,
    sync::Arc,
};

use clunky::{
    math::{mul_3d_by_1d, Matrix4, Radians},
    meshes,
    physics::physics_3d::{aabb::AabbCentredOrigin, bodies::Body as BodyTrait},
    shaders::{
        instanced_simple_lit_colour_3d, instanced_simple_lit_uv_3d, instanced_text_sdf,
        instanced_unlit_uv_2d_stretch, simple_lit_colour_3d,
    },
};
use png::ColorType;
use rayon::iter::{IntoParallelRefIterator, ParallelExtend, ParallelIterator};
use vulkano::{
    buffer::{
        allocator::{SubbufferAllocator, SubbufferAllocatorCreateInfo},
        Buffer, BufferContents, BufferCreateInfo, BufferUsage, Subbuffer,
    },
    command_buffer::{
        allocator::StandardCommandBufferAllocator, AutoCommandBufferBuilder, CommandBufferUsage,
        CopyBufferInfo, CopyBufferToImageInfo, PrimaryAutoCommandBuffer,
        PrimaryCommandBufferAbstract, RenderPassBeginInfo,
    },
    descriptor_set::{
        allocator::StandardDescriptorSetAllocator, PersistentDescriptorSet, WriteDescriptorSet,
    },
    device::Device,
    format::{ClearValue, Format},
    image::{
        sampler::{Sampler, SamplerCreateInfo},
        view::ImageView,
        Image, ImageCreateInfo, ImageType, ImageUsage,
    },
    memory::allocator::{AllocationCreateInfo, MemoryTypeFilter, StandardMemoryAllocator},
    pipeline::{
        graphics::{
            input_assembly::{InputAssemblyState, PrimitiveTopology},
            multisample::MultisampleState,
            rasterization::{CullMode, FrontFace, RasterizationState},
            viewport::{Viewport, ViewportState},
            GraphicsPipelineCreateInfo,
        },
        DynamicState, GraphicsPipeline, Pipeline, PipelineBindPoint,
    },
    render_pass::{Framebuffer, FramebufferCreateInfo, RenderPass, Subpass},
    swapchain::SwapchainCreateInfo,
    sync::GpuFuture,
    DeviceSize,
};
use vulkano_util::{
    context::{VulkanoConfig, VulkanoContext},
    window::{VulkanoWindows, WindowDescriptor},
};
use winit::{
    event_loop::{EventLoop, EventLoopWindowTarget},
    window::WindowId,
};

use crate::body::Body;

procedural_macros::generate_gltf_consts!();

const DEPTH_FORMAT: Format = Format::D32_SFLOAT;
const BACKGROUND_COLOUR: [f32; 4] = [0.0, 0.0, 1.0, 1.0];
//const BACKGROUND_COLOUR: [f32; 4] = [0.0, 0.0, 0.0, 0.0];

const CUBOID_COLOUR_INSTANCES_STARTING_CAPACITY: usize = 50;
const POTENTIAL_CUBOID_COLOUR_INSTANCES_STARTING_CAPACITY: usize = 1000;

const BOOKMAN_OLD_STYLE_GLYPH_SIZE: [f32; 2] = [1.0, 1.0]; // TODO: make this correct

#[derive(Clone, Debug)]
pub struct Camera3D {
    pub position: [f32; 3],
    pub rotation: [f32; 3],

    pub ambient_strength: f32,
    pub specular_strength: f32,
    pub light_colour: [f32; 3],
    pub light_position: [f32; 3],

    pub near_distance: f32,
    pub far_distance: f32,
    pub aspect_ratio: f32,
    pub fov_y: Radians<f32>,
}

impl Camera3D {
    // Inline?
    pub fn to_instanced_simple_lit_uv_3d_camera(&self) -> instanced_simple_lit_uv_3d::Camera {
        instanced_simple_lit_uv_3d::Camera {
            position: self.position,
            rotation: self.rotation,

            ambient_strength: self.ambient_strength,
            specular_strength: self.specular_strength,
            light_colour: self.light_colour,
            light_position: self.light_position,

            near_distance: self.near_distance,
            far_distance: self.far_distance,
            aspect_ratio: self.aspect_ratio,
            fov_y: self.fov_y,
        }
    }

    pub fn to_instanced_simple_lit_colour_3d_camera(
        &self,
    ) -> instanced_simple_lit_colour_3d::Camera {
        instanced_simple_lit_colour_3d::Camera {
            position: self.position,
            rotation: self.rotation,

            ambient_strength: self.ambient_strength,
            specular_strength: self.specular_strength,
            light_colour: self.light_colour,
            light_position: self.light_position,

            near_distance: self.near_distance,
            far_distance: self.far_distance,
            aspect_ratio: self.aspect_ratio,
            fov_y: self.fov_y,
        }
    }
}

impl Default for Camera3D {
    fn default() -> Self {
        Self {
            position: [0.0; 3],
            rotation: [0.0; 3],

            ambient_strength: 0.3,
            specular_strength: 0.5,
            light_colour: [0.5; 3],
            light_position: [0.0, -10.0, 0.0],

            near_distance: 0.01,
            far_distance: 250.0,
            aspect_ratio: 1.0,
            fov_y: Radians(std::f32::consts::FRAC_PI_2),
        }
    }
}

/// This is important window configuration. Vsync is PresentMode.
pub struct WindowConfig {
    pub variety: WindowVariety,
    pub window_descriptor: WindowDescriptor,
    pub swapchain_create_info_modify: fn(&mut SwapchainCreateInfo),
}

impl Default for WindowConfig {
    fn default() -> Self {
        Self {
            variety: WindowVariety::Creature(Default::default()),
            window_descriptor: Default::default(),
            swapchain_create_info_modify: |_| {},
        }
    }
}

#[derive(Clone)]
pub enum WindowVariety {
    Creature(Camera3D),
    Selection,
    Menu,
}

pub struct WindowSpecific {
    pub viewport: Viewport,
    pub variety: WindowVariety,
}

pub struct Allocators {
    // This could be gotten via VulkanoContext, but it is more intuitive to get it from here.
    pub memory_allocator: Arc<StandardMemoryAllocator>,
    pub command_buffer_allocator: StandardCommandBufferAllocator,
    pub subbuffer_allocator: SubbufferAllocator,
    pub descriptor_set_allocator: StandardDescriptorSetAllocator,
}

impl Allocators {
    fn new(context: &VulkanoContext) -> Self {
        Self {
            memory_allocator: context.memory_allocator().clone(),
            command_buffer_allocator: StandardCommandBufferAllocator::new(
                context.device().clone(),
                Default::default(),
            ),
            //TODO: I'm interested in whether we actually want PREFER_DEVICE | HOST_SEQUENTIAL_WRITE. May we instead want staging buffers? Uncertain. Should profile.
            subbuffer_allocator: SubbufferAllocator::new(
                context.memory_allocator().clone(),
                SubbufferAllocatorCreateInfo {
                    buffer_usage: BufferUsage::UNIFORM_BUFFER | BufferUsage::VERTEX_BUFFER,
                    memory_type_filter: MemoryTypeFilter::PREFER_DEVICE
                        | MemoryTypeFilter::HOST_SEQUENTIAL_WRITE,
                    ..Default::default()
                },
            ),
            descriptor_set_allocator: StandardDescriptorSetAllocator::new(
                context.device().clone(),
                Default::default(),
            ),
        }
    }
}

pub struct Renderer {
    context: VulkanoContext,

    allocators: Allocators,

    render_pass: Arc<RenderPass>,
    pipelines: Pipelines,

    pub buffers: Buffers,
    pub proc_buffers: ProcBuffers,
    images_and_samplers: ImagesAndSamplers,

    pub windows_manager: VulkanoWindows,
    // Consider faster hash, like ahash?
    pub window_specifics: HashMap<WindowId, WindowSpecific>,
}

impl Renderer {
    pub fn new() -> (Self, EventLoop<()>) {
        let event_loop = EventLoop::new();

        let context = VulkanoContext::new(VulkanoConfig::default());

        let mut windows_manager = VulkanoWindows::default();

        //TODO: find a better way of getting the correct format.
        let temp =
            windows_manager.create_window(&event_loop, &context, &Default::default(), |_| {});
        let format = windows_manager
            .get_primary_renderer()
            .unwrap()
            .swapchain_format();
        windows_manager.remove_renderer(temp);

        let render_pass = vulkano::single_pass_renderpass!(
            context.device().clone(),
            attachments: {
                color: {
                    format: format,
                    samples: 1,
                    load_op: Clear,
                    store_op: Store,
                },
                depth: {
                    format: DEPTH_FORMAT,
                    samples: 1,
                    load_op: Clear,
                    store_op: Store,
                }
            },
            pass: {
                color: [color],
                depth_stencil: {depth},
            },
        )
        .unwrap();

        let allocators = Allocators::new(&context);

        let pipelines = Pipelines::new(context.device(), &render_pass);

        let buffers = Buffers::new(&allocators, &context);
        let proc_buffers = ProcBuffers::new(&allocators, &context);
        let images_and_samplers = ImagesAndSamplers::new(&context, &allocators, &pipelines);

        let renderer = Self {
            context,

            allocators,

            render_pass,
            pipelines,

            buffers,
            proc_buffers,
            images_and_samplers,

            windows_manager,
            window_specifics: Default::default(),
        };

        (renderer, event_loop)
    }

    pub fn correct_window_size(&mut self, window_id: WindowId) {
        let window_specific = self.window_specifics.get_mut(&window_id).unwrap();

        let new_viewport_extent = self
            .windows_manager
            .get_window(window_id)
            .unwrap()
            .inner_size()
            .into();

        window_specific.viewport.extent = new_viewport_extent;

        if let WindowVariety::Creature(camera) = &mut window_specific.variety {
            camera.aspect_ratio = new_viewport_extent[0] / new_viewport_extent[1];
        }
    }

    pub fn proc_render(&mut self, bodies: Option<&[Body]>) {
        // add to buffers here

        for (window_id, window_specific) in &mut self.window_specifics {
            let window_renderer = self.windows_manager.get_renderer_mut(*window_id).unwrap();

            let future = window_renderer.acquire().unwrap();

            let mut command_buffer_builder = AutoCommandBufferBuilder::primary(
                &self.allocators.command_buffer_allocator,
                self.context.graphics_queue().queue_family_index(),
                CommandBufferUsage::OneTimeSubmit,
            )
            .unwrap();

            //TODO: Creating a depth buffer and a frame buffer every frame for every window is very very bad. Not avoidable until next vulkano version.

            let depth_buffer_view = ImageView::new_default(
                Image::new(
                    self.context.memory_allocator().clone(),
                    ImageCreateInfo {
                        image_type: ImageType::Dim2d,
                        format: Format::D32_SFLOAT,
                        extent: window_renderer.swapchain_image_view().image().extent(),
                        usage: ImageUsage::TRANSIENT_ATTACHMENT
                            | ImageUsage::DEPTH_STENCIL_ATTACHMENT,
                        ..Default::default()
                    },
                    AllocationCreateInfo::default(),
                )
                .unwrap(),
            )
            .unwrap();

            let framebuffer = Framebuffer::new(
                self.render_pass.clone(),
                FramebufferCreateInfo {
                    attachments: vec![window_renderer.swapchain_image_view(), depth_buffer_view],
                    ..Default::default()
                },
            )
            .unwrap();

            command_buffer_builder
                .begin_render_pass(
                    RenderPassBeginInfo {
                        clear_values: vec![
                            // Sets background colour.
                            Some(ClearValue::Float(BACKGROUND_COLOUR)),
                            Some(ClearValue::Depth(1.0)),
                        ],
                        ..RenderPassBeginInfo::framebuffer(framebuffer)
                    },
                    Default::default(),
                )
                .unwrap()
                .set_viewport(0, [window_specific.viewport.clone()].into_iter().collect())
                .unwrap();

            match &window_specific.variety {
                WindowVariety::Creature(camera) => {
                    let camera_uniform = self
                        .allocators
                        .subbuffer_allocator
                        .allocate_sized()
                        .unwrap();
                    *camera_uniform.write().unwrap() = camera
                        .to_instanced_simple_lit_colour_3d_camera()
                        .to_uniform();

                    procedural_macros::command_list!(creature_window);
                }
                WindowVariety::Selection => {}
                WindowVariety::Menu => {}
            }

            command_buffer_builder
                .end_render_pass(Default::default())
                .unwrap();
            let command_buffer = command_buffer_builder.build().unwrap();
            window_renderer.present(
                future
                    .then_execute(self.context.graphics_queue().clone(), command_buffer)
                    .unwrap()
                    .boxed(),
                false,
            );
        }

        // remove from buffers here
    }

    // MARK: Render
    pub fn render(&mut self, bodies: Option<&[Body]>) {
        self.buffers.before_rendering(bodies);
        for (window_id, window_specific) in &mut self.window_specifics {
            let window_renderer = self.windows_manager.get_renderer_mut(*window_id).unwrap();

            let future = window_renderer.acquire().unwrap();

            let mut command_buffer_builder = AutoCommandBufferBuilder::primary(
                &self.allocators.command_buffer_allocator,
                self.context.graphics_queue().queue_family_index(),
                CommandBufferUsage::OneTimeSubmit,
            )
            .unwrap();

            //TODO: Creating a depth buffer and a frame buffer every frame for every window is very very bad. Not avoidable until next vulkano version.

            let depth_buffer_view = ImageView::new_default(
                Image::new(
                    self.context.memory_allocator().clone(),
                    ImageCreateInfo {
                        image_type: ImageType::Dim2d,
                        format: Format::D32_SFLOAT,
                        extent: window_renderer.swapchain_image_view().image().extent(),
                        usage: ImageUsage::TRANSIENT_ATTACHMENT
                            | ImageUsage::DEPTH_STENCIL_ATTACHMENT,
                        ..Default::default()
                    },
                    AllocationCreateInfo::default(),
                )
                .unwrap(),
            )
            .unwrap();

            let framebuffer = Framebuffer::new(
                self.render_pass.clone(),
                FramebufferCreateInfo {
                    attachments: vec![window_renderer.swapchain_image_view(), depth_buffer_view],
                    ..Default::default()
                },
            )
            .unwrap();

            command_buffer_builder
                .begin_render_pass(
                    RenderPassBeginInfo {
                        clear_values: vec![
                            // Sets background colour.
                            Some(ClearValue::Float(BACKGROUND_COLOUR)),
                            Some(ClearValue::Depth(1.0)),
                        ],
                        ..RenderPassBeginInfo::framebuffer(framebuffer)
                    },
                    Default::default(),
                )
                .unwrap()
                .set_viewport(0, [window_specific.viewport.clone()].into_iter().collect())
                .unwrap();

            match &window_specific.variety {
                WindowVariety::Creature(camera) => {
                    let camera_uniform = self
                        .allocators
                        .subbuffer_allocator
                        .allocate_sized()
                        .unwrap();
                    *camera_uniform.write().unwrap() = camera
                        .to_instanced_simple_lit_colour_3d_camera()
                        .to_uniform();

                    self.pipelines.bind_instanced_simple_lit_colour_3d(
                        &mut command_buffer_builder,
                        &self.allocators,
                        camera_uniform,
                    );

                    let cuboid_colour_instance_buffer = self
                        .allocators
                        .subbuffer_allocator
                        .from_slice(&self.buffers.cuboid_colour_instances);

                    command_buffer_builder
                        .bind_vertex_buffers(
                            0,
                            (
                                self.buffers
                                    .cuboid_colour_vertices_and_indices
                                    .vertices
                                    .clone(),
                                cuboid_colour_instance_buffer,
                            ),
                        )
                        .unwrap()
                        .bind_index_buffer(
                            self.buffers
                                .cuboid_colour_vertices_and_indices
                                .indices
                                .clone(),
                        )
                        .unwrap()
                        .draw_indexed(
                            self.buffers
                                .cuboid_colour_vertices_and_indices
                                .indices
                                .len() as u32,
                            self.buffers.cuboid_colour_instances.len() as u32,
                            0,
                            0,
                            0,
                        )
                        .unwrap();
                }
                WindowVariety::Selection => {
                    let font_uniform = self
                        .allocators
                        .subbuffer_allocator
                        .allocate_sized()
                        .unwrap();
                    *font_uniform.write().unwrap() = instanced_text_sdf::FontUniform {
                        glyph_size: BOOKMAN_OLD_STYLE_GLYPH_SIZE,
                        aspect_ratio: window_specific.viewport.extent[0]
                            / window_specific.viewport.extent[1],
                    };

                    // UV UI
                    self.pipelines.bind_instanced_unlit_uv_2d_stretch(
                        &mut command_buffer_builder,
                        &self
                            .images_and_samplers
                            .descriptor_set_menu_sampler_with_testing_image,
                    );

                    let selection_menu_uv_instances_buffer =
                        self.allocators
                            .subbuffer_allocator
                            .allocate_slice(
                                self.buffers.selection_menu_uv_instances.len() as DeviceSize
                            )
                            .unwrap();
                    selection_menu_uv_instances_buffer
                        .write()
                        .unwrap()
                        .copy_from_slice(&self.buffers.selection_menu_uv_instances);

                    command_buffer_builder
                        .bind_vertex_buffers(
                            0,
                            (
                                self.buffers
                                    .squaroid_uv_vertices_and_indices
                                    .vertices
                                    .clone(),
                                selection_menu_uv_instances_buffer,
                            ),
                        )
                        .unwrap()
                        .bind_index_buffer(
                            self.buffers
                                .squaroid_uv_vertices_and_indices
                                .indices
                                .clone(),
                        )
                        .unwrap()
                        .draw_indexed(
                            self.buffers.squaroid_uv_vertices_and_indices.indices.len() as u32,
                            self.buffers.selection_menu_uv_instances.len() as u32,
                            0,
                            0,
                            0,
                        )
                        .unwrap();

                    // TEXT
                    self.pipelines.bind_instanced_text_sdf(
                        &mut command_buffer_builder,
                        &self.allocators,
                        &self
                            .images_and_samplers
                            .descriptor_set_text_sdf_sampler_with_testing_text_sdf_image,
                        font_uniform,
                    );

                    let selection_menu_text_instances_buffer = self
                        .allocators
                        .subbuffer_allocator
                        .allocate_slice(
                            self.buffers.selection_menu_text_instances.len() as DeviceSize
                        )
                        .unwrap();
                    selection_menu_text_instances_buffer
                        .write()
                        .unwrap()
                        .copy_from_slice(&self.buffers.selection_menu_text_instances);

                    command_buffer_builder
                        .bind_vertex_buffers(
                            0,
                            (
                                self.buffers
                                    .squaroid_uv_vertices_and_indices
                                    .vertices
                                    .clone(),
                                selection_menu_text_instances_buffer,
                            ),
                        )
                        .unwrap()
                        .bind_index_buffer(
                            self.buffers
                                .squaroid_uv_vertices_and_indices
                                .indices
                                .clone(),
                        )
                        .unwrap()
                        .draw_indexed(
                            self.buffers.squaroid_uv_vertices_and_indices.indices.len() as u32,
                            self.buffers.selection_menu_text_instances.len() as u32,
                            0,
                            0,
                            0,
                        )
                        .unwrap();
                }
                WindowVariety::Menu => {
                    let font_uniform = self
                        .allocators
                        .subbuffer_allocator
                        .allocate_sized()
                        .unwrap();
                    *font_uniform.write().unwrap() = instanced_text_sdf::FontUniform {
                        glyph_size: BOOKMAN_OLD_STYLE_GLYPH_SIZE,
                        aspect_ratio: window_specific.viewport.extent[0]
                            / window_specific.viewport.extent[1],
                    };

                    // UV UI
                    if self.buffers.menu_uv_instances.len() != 0 {
                        self.pipelines.bind_instanced_unlit_uv_2d_stretch(
                            &mut command_buffer_builder,
                            &self
                                .images_and_samplers
                                .descriptor_set_menu_sampler_with_testing_image,
                        );

                        let menu_uv_instances_buffer = self
                            .allocators
                            .subbuffer_allocator
                            .allocate_slice(self.buffers.menu_uv_instances.len() as DeviceSize)
                            .unwrap();
                        menu_uv_instances_buffer
                            .write()
                            .unwrap()
                            .copy_from_slice(&self.buffers.menu_uv_instances);

                        command_buffer_builder
                            .bind_vertex_buffers(
                                0,
                                (
                                    self.buffers
                                        .squaroid_uv_vertices_and_indices
                                        .vertices
                                        .clone(),
                                    menu_uv_instances_buffer,
                                ),
                            )
                            .unwrap()
                            .bind_index_buffer(
                                self.buffers
                                    .squaroid_uv_vertices_and_indices
                                    .indices
                                    .clone(),
                            )
                            .unwrap()
                            .draw_indexed(
                                self.buffers.squaroid_uv_vertices_and_indices.indices.len() as u32,
                                self.buffers.menu_uv_instances.len() as u32,
                                0,
                                0,
                                0,
                            )
                            .unwrap();
                    }

                    // TEXT
                    if self.buffers.menu_text_instances.len() != 0 {
                        self.pipelines.bind_instanced_text_sdf(
                            &mut command_buffer_builder,
                            &self.allocators,
                            &self
                                .images_and_samplers
                                .descriptor_set_text_sdf_sampler_with_testing_text_sdf_image,
                            font_uniform,
                        );

                        let menu_text_instances_buffer = self
                            .allocators
                            .subbuffer_allocator
                            .allocate_slice(self.buffers.menu_text_instances.len() as DeviceSize)
                            .unwrap();
                        menu_text_instances_buffer
                            .write()
                            .unwrap()
                            .copy_from_slice(&self.buffers.menu_text_instances);

                        command_buffer_builder
                            .bind_vertex_buffers(
                                0,
                                (
                                    self.buffers
                                        .squaroid_uv_vertices_and_indices
                                        .vertices
                                        .clone(),
                                    menu_text_instances_buffer,
                                ),
                            )
                            .unwrap()
                            .bind_index_buffer(
                                self.buffers
                                    .squaroid_uv_vertices_and_indices
                                    .indices
                                    .clone(),
                            )
                            .unwrap()
                            .draw_indexed(
                                self.buffers.squaroid_uv_vertices_and_indices.indices.len() as u32,
                                self.buffers.menu_text_instances.len() as u32,
                                0,
                                0,
                                0,
                            )
                            .unwrap();
                    }
                }
            }

            command_buffer_builder
                .end_render_pass(Default::default())
                .unwrap();
            let command_buffer = command_buffer_builder.build().unwrap();
            window_renderer.present(
                future
                    .then_execute(self.context.graphics_queue().clone(), command_buffer)
                    .unwrap()
                    .boxed(),
                false,
            );
        }
        self.buffers.after_rendering();
    }

    /// Creates a new window!
    pub fn create_window(
        &mut self,
        event_loop: &EventLoopWindowTarget<()>,
        config: &WindowConfig,
    ) -> WindowId {
        let id = self.windows_manager.create_window(
            event_loop,
            &self.context,
            &config.window_descriptor,
            config.swapchain_create_info_modify,
        );

        self.window_specifics.insert(
            id,
            WindowSpecific {
                viewport: Viewport {
                    offset: [0.0, 0.0],
                    // Consider getting from window or window descriptor?
                    extent: [1.0, 1.0],
                    depth_range: 0.0..=1.0,
                },
                variety: config.variety.clone(),
            },
        );

        id
    }

    pub fn remove_window(&mut self, id: WindowId) {
        self.window_specifics.remove(&id);
        self.windows_manager.remove_renderer(id);
    }

    // inline?
    /// Allows you to add a cuboid colour that will have its instance generated from a body.
    /// Removable.
    pub fn add_removable_cuboid_colour_from_body_index(
        &mut self,
        body_index: usize,
        colour: [f32; 4],
    ) -> usize {
        self.buffers
            .cuboid_colour_potential_instances
            .push(PotentialCuboidColourInstance::PhysicsWithColour { body_index, colour });
        self.buffers.cuboid_colour_potential_instances.len() - 1
    }

    // Adds a cuboid colour instance.
    // Not removable.
    pub fn add_cuboid_colour(&mut self, instance: instanced_simple_lit_colour_3d::Instance) {
        self.buffers.cuboid_colour_instances.push(instance);
    }

    /// Allows you to add a cuboid colour instance created from an AabbCentredOrigin.
    /// Not removable.
    pub fn add_cuboid_colour_from_aabb(&mut self, aabb: AabbCentredOrigin<f32>, colour: [f32; 4]) {
        self.buffers
            .cuboid_colour_instances
            .push(instanced_simple_lit_colour_3d::Instance::new(
                colour,
                Matrix4::from_translation(aabb.position)
                    * Matrix4::from_scale(mul_3d_by_1d(aabb.half_size, 2.0)),
            ));
    }

    /// Raw access to the instances is useful for ui. For now.
    pub fn selection_menu_uv_instances_mut(
        &mut self,
    ) -> &mut Vec<instanced_unlit_uv_2d_stretch::Instance> {
        &mut self.buffers.selection_menu_uv_instances
    }

    /// Raw access to the instances is useful for ui. For now.
    pub fn selection_menu_text_instances_mut(&mut self) -> &mut Vec<instanced_text_sdf::Instance> {
        &mut self.buffers.selection_menu_text_instances
    }
}

//TODO: implement a shrink to fit function when the amount of Nones pile up in potentials, to keep memory use low.
// To do this, we should have a none counter that gets incremented every time a none is added.
// Don't do the silly and slow option of constantly checking a very long vec for nones.
// But how would we deal with indices suddenly being wrong?
// Perhaps when I know a None index, we just move the next instance into it?
// MARK: Buffers
pub struct Buffers {
    //TODO: Get this working with u8 indices.
    cuboid_colour_vertices_and_indices:
        DeviceVerticesAndIndices<instanced_simple_lit_colour_3d::Vertex, u16>,
    cuboid_colour_instances: Vec<instanced_simple_lit_colour_3d::Instance>,
    cuboid_colour_potential_instances: Vec<PotentialCuboidColourInstance>,
    // This is only valid during rendering.
    cuboid_colour_drain_start_index: usize,

    // I imagine that this will be temporary.
    //terrain_colour_vertices_and_indices:
    //DeviceVerticesAndIndices<simple_lit_colour_3d::Vertex, u32>,

    // These can be used for all uv squaroids, as they all have the same layout.
    squaroid_uv_vertices_and_indices:
        DeviceVerticesAndIndices<instanced_unlit_uv_2d_stretch::Vertex, u16>,

    pub selection_menu_uv_instances: Vec<instanced_unlit_uv_2d_stretch::Instance>,
    pub selection_menu_text_instances: Vec<instanced_text_sdf::Instance>,

    pub menu_uv_instances: Vec<instanced_unlit_uv_2d_stretch::Instance>,
    pub menu_text_instances: Vec<instanced_text_sdf::Instance>,
}

impl Buffers {
    // This only happens once, so I'm considering making this more expensive by reading it a file's bytes (json perhaps?), and simplifying my process.
    // Why a file though? I should probably just use actual config structs.
    fn new(allocators: &Allocators, context: &VulkanoContext) -> Self {
        let mut command_buffer_builder = AutoCommandBufferBuilder::primary(
            &allocators.command_buffer_allocator,
            context.graphics_queue().queue_family_index(),
            CommandBufferUsage::OneTimeSubmit,
        )
        .unwrap();

        let cuboid_colour_vertices =
            instanced_simple_lit_colour_3d::Vertex::get_array_from_gltf(meshes::CUBE_GLTF, 0);
        let cuboid_colour_indices = meshes::get_indices_from_gltf(meshes::CUBE_GLTF, 0);

        //let terrain_colour_vertices =
        //simple_lit_colour_3d::Vertex::get_array_from_gltf(TERRAIN_GLTF, 0);
        //let terrain_colour_indices = meshes::get_indices_from_gltf(TERRAIN_GLTF, 0);

        let selection_menu_uv_vertices =
            instanced_unlit_uv_2d_stretch::Vertex::get_array_from_gltf(SQUAROID_GLTF, 0);
        let selection_menu_uv_indices = meshes::get_indices_from_gltf(SQUAROID_GLTF, 0);

        let buffers = Self {
            cuboid_colour_vertices_and_indices: DeviceVerticesAndIndices::new(
                cuboid_colour_vertices,
                cuboid_colour_indices,
                &allocators.memory_allocator,
                &mut command_buffer_builder,
            ),
            cuboid_colour_instances: Vec::with_capacity(CUBOID_COLOUR_INSTANCES_STARTING_CAPACITY),
            cuboid_colour_potential_instances: Vec::with_capacity(
                POTENTIAL_CUBOID_COLOUR_INSTANCES_STARTING_CAPACITY,
            ),
            cuboid_colour_drain_start_index: 0,

            // terrain_colour_vertices_and_indices: DeviceVerticesAndIndices::new(
            //     terrain_colour_vertices,
            //     terrain_colour_indices,
            //     &allocators.memory_allocator,
            //     &mut command_buffer_builder,
            // ),
            squaroid_uv_vertices_and_indices: DeviceVerticesAndIndices::new(
                selection_menu_uv_vertices,
                selection_menu_uv_indices,
                &allocators.memory_allocator,
                &mut command_buffer_builder,
            ),

            selection_menu_uv_instances: Vec::with_capacity(30),
            selection_menu_text_instances: Vec::with_capacity(100),

            menu_uv_instances: Vec::with_capacity(30),
            menu_text_instances: Vec::with_capacity(100),
        };

        command_buffer_builder
            .build()
            .unwrap()
            .execute(context.graphics_queue().clone())
            .unwrap()
            .then_signal_fence_and_flush()
            .unwrap()
            .wait(None)
            .unwrap();

        buffers
    }

    /// Turns all potential instances into real instances.
    fn before_rendering(&mut self, bodies: Option<&[Body]>) {
        self.cuboid_colour_drain_start_index = self.cuboid_colour_instances.len();

        let Some(bodies) = bodies else {
            if self.cuboid_colour_potential_instances.len() != 0 {
                unreachable!("If bodies is none, then there should not be any potential instances for rendering.")
            }

            return;
        };

        self.cuboid_colour_instances.par_extend(
            self.cuboid_colour_potential_instances
                .par_iter()
                .filter_map(|potential_cuboid_colour_instance| {
                    potential_cuboid_colour_instance.to_instance(bodies)
                }),
        );
    }

    /// Removes all the realised potential instances from the instances.
    fn after_rendering(&mut self) {
        self.cuboid_colour_instances
            .drain(self.cuboid_colour_drain_start_index..);
        debug_assert!(self.cuboid_colour_drain_start_index == self.cuboid_colour_instances.len());
    }
}

#[derive(Clone)]
pub enum PotentialCuboidColourInstance {
    None,
    // This is so expensive, due to the large instance size, that if I need this, I should probably just have another enum with only this.
    //Instance(instanced_simple_lit_colour_3d::Instance),
    PhysicsWithColour { body_index: usize, colour: [f32; 4] },
}

impl PotentialCuboidColourInstance {
    #[inline]
    fn to_instance(&self, bodies: &[Body]) -> Option<instanced_simple_lit_colour_3d::Instance> {
        match self {
            Self::PhysicsWithColour { body_index, colour } => {
                let body = &bodies[*body_index];
                Some(instanced_simple_lit_colour_3d::Instance::new(
                    *colour,
                    Matrix4::from_translation(body.position_unchecked())
                        * Matrix4::from_scale(mul_3d_by_1d(body.half_size_unchecked(), 2.0)),
                ))
            }
            Self::None => None,
        }
    }
}

/// Not accessible to the host. These will be on the device only.
///
/// Because it copies from a staging buffer to a device local buffer, you will need to use a command buffer before first use.
struct DeviceVerticesAndIndices<V, I> {
    vertices: Subbuffer<[V]>,
    indices: Subbuffer<[I]>,
}

impl<V, I> DeviceVerticesAndIndices<V, I> {
    fn new<VI, II>(
        vertices: VI,
        indices: II,
        memory_allocator: &Arc<StandardMemoryAllocator>,
        command_buffer_builder: &mut AutoCommandBufferBuilder<PrimaryAutoCommandBuffer>,
    ) -> Self
    where
        V: BufferContents,
        I: BufferContents,
        VI: IntoIterator<Item = V>,
        VI::IntoIter: ExactSizeIterator,
        II: IntoIterator<Item = I>,
        II::IntoIter: ExactSizeIterator,
    {
        // Go to Vulkano's Buffer documentation. I basically copied the staging example.

        // VERTICES
        let vertices_iter = vertices.into_iter();
        let vertices_len = vertices_iter.len();

        let vertices_staging = Buffer::from_iter(
            memory_allocator.clone(),
            BufferCreateInfo {
                usage: BufferUsage::TRANSFER_SRC,
                ..Default::default()
            },
            AllocationCreateInfo {
                memory_type_filter: MemoryTypeFilter::PREFER_HOST
                    | MemoryTypeFilter::HOST_SEQUENTIAL_WRITE,
                ..Default::default()
            },
            vertices_iter,
        )
        .unwrap();

        let vertices_device = Buffer::new_slice::<V>(
            memory_allocator.clone(),
            BufferCreateInfo {
                usage: BufferUsage::VERTEX_BUFFER | BufferUsage::TRANSFER_DST,
                ..Default::default()
            },
            AllocationCreateInfo {
                memory_type_filter: MemoryTypeFilter::PREFER_DEVICE,
                ..Default::default()
            },
            vertices_len as DeviceSize,
        )
        .unwrap();

        command_buffer_builder
            .copy_buffer(CopyBufferInfo::buffers(
                vertices_staging,
                vertices_device.clone(),
            ))
            .unwrap();

        // INDICES
        let indices_iter = indices.into_iter();
        let indices_len = indices_iter.len();

        let indices_staging = Buffer::from_iter(
            memory_allocator.clone(),
            BufferCreateInfo {
                usage: BufferUsage::TRANSFER_SRC,
                ..Default::default()
            },
            AllocationCreateInfo {
                memory_type_filter: MemoryTypeFilter::PREFER_HOST
                    | MemoryTypeFilter::HOST_SEQUENTIAL_WRITE,
                ..Default::default()
            },
            indices_iter,
        )
        .unwrap();

        let indices_device = Buffer::new_slice::<I>(
            memory_allocator.clone(),
            BufferCreateInfo {
                usage: BufferUsage::INDEX_BUFFER | BufferUsage::TRANSFER_DST,
                ..Default::default()
            },
            AllocationCreateInfo {
                memory_type_filter: MemoryTypeFilter::PREFER_DEVICE,
                ..Default::default()
            },
            indices_len as DeviceSize,
        )
        .unwrap();

        command_buffer_builder
            .copy_buffer(CopyBufferInfo::buffers(
                indices_staging,
                indices_device.clone(),
            ))
            .unwrap();

        Self {
            indices: indices_device,
            vertices: vertices_device,
        }
    }
}

struct ImagesAndSamplers {
    menu_sampler: Arc<Sampler>,
    text_sdf_sampler: Arc<Sampler>,

    testing_image: Arc<ImageView>,
    testing_text_sdf_image: Arc<ImageView>,

    // Related descriptor sets.
    // Should these go in a descriptor sets struct? I don't think so.
    descriptor_set_menu_sampler_with_testing_image: Arc<PersistentDescriptorSet>,
    descriptor_set_text_sdf_sampler_with_testing_text_sdf_image: Arc<PersistentDescriptorSet>,
}

impl ImagesAndSamplers {
    fn new(context: &VulkanoContext, allocators: &Allocators, pipelines: &Pipelines) -> Self {
        let mut command_buffer_builder = AutoCommandBufferBuilder::primary(
            &allocators.command_buffer_allocator,
            context.graphics_queue().queue_family_index(),
            CommandBufferUsage::OneTimeSubmit,
        )
        .unwrap();

        let menu_sampler = Sampler::new(
            context.device().clone(),
            SamplerCreateInfo {
                ..SamplerCreateInfo::simple_repeat_linear()
            },
        )
        .unwrap();

        let text_sdf_sampler = Sampler::new(
            context.device().clone(),
            SamplerCreateInfo {
                ..SamplerCreateInfo::simple_repeat_linear()
            },
        )
        .unwrap();

        let testing_image = image_from_png(
            include_bytes!("images/test.png"),
            //include_bytes!("images/image_img.png"),
            &allocators.memory_allocator,
            &mut command_buffer_builder,
        );
        let testing_text_sdf_image = image_from_png(
            include_bytes!("images/font.png"),
            &allocators.memory_allocator,
            &mut command_buffer_builder,
        );

        let images_and_samplers = Self {
            menu_sampler: menu_sampler.clone(),
            text_sdf_sampler: text_sdf_sampler.clone(),

            testing_image: testing_image.clone(),
            testing_text_sdf_image: testing_text_sdf_image.clone(),

            descriptor_set_menu_sampler_with_testing_image: PersistentDescriptorSet::new(
                &allocators.descriptor_set_allocator,
                pipelines
                    .instanced_unlit_uv_2d_stretch
                    .layout()
                    .set_layouts()[0]
                    .clone(),
                [
                    WriteDescriptorSet::sampler(0, menu_sampler),
                    WriteDescriptorSet::image_view(1, testing_image),
                ],
                [],
            )
            .unwrap(),
            descriptor_set_text_sdf_sampler_with_testing_text_sdf_image:
                PersistentDescriptorSet::new(
                    &allocators.descriptor_set_allocator,
                    pipelines.instanced_text_sdf.layout().set_layouts()[1].clone(),
                    [
                        WriteDescriptorSet::sampler(0, text_sdf_sampler),
                        WriteDescriptorSet::image_view(1, testing_text_sdf_image),
                    ],
                    [],
                )
                .unwrap(),
        };

        command_buffer_builder
            .build()
            .unwrap()
            .execute(context.graphics_queue().clone())
            .unwrap()
            .then_signal_fence_and_flush()
            .unwrap()
            .wait(None)
            .unwrap();

        images_and_samplers
    }
}

fn image_from_png(
    png_bytes: &[u8],
    memory_allocator: &Arc<StandardMemoryAllocator>,
    command_buffer_builder: &mut AutoCommandBufferBuilder<PrimaryAutoCommandBuffer>,
) -> Arc<ImageView> {
    let decoder = png::Decoder::new(png_bytes);
    let mut reader = decoder.read_info().unwrap();
    let info = reader.info();

    let bytes_per_pixel = info.bytes_per_pixel();
    println!("bytes_per_pixel: {:?}", bytes_per_pixel);

    println!("color_type: {:?}", info.color_type);
    let format = match info.color_type {
        ColorType::Rgba => Format::R8G8B8A8_SRGB,
        ColorType::Rgb => Format::R8G8B8_SRGB,
        _ => todo!("Currently unsupported image type."),
    };

    let extent = [info.width, info.height, 1];

    let upload_buffer = Buffer::new_slice(
        memory_allocator.clone(),
        BufferCreateInfo {
            usage: BufferUsage::TRANSFER_SRC,
            ..Default::default()
        },
        AllocationCreateInfo {
            memory_type_filter: MemoryTypeFilter::PREFER_HOST
                | MemoryTypeFilter::HOST_SEQUENTIAL_WRITE,
            ..Default::default()
        },
        (info.width * info.height * bytes_per_pixel as u32) as DeviceSize,
    )
    .unwrap();

    reader
        .next_frame(&mut upload_buffer.write().unwrap())
        .unwrap();

    let image = Image::new(
        memory_allocator.clone(),
        ImageCreateInfo {
            image_type: ImageType::Dim2d,
            format,
            extent,
            usage: ImageUsage::TRANSFER_DST | ImageUsage::SAMPLED,
            ..Default::default()
        },
        AllocationCreateInfo::default(),
    )
    .unwrap();

    command_buffer_builder
        .copy_buffer_to_image(CopyBufferToImageInfo::buffer_image(
            upload_buffer,
            image.clone(),
        ))
        .unwrap();

    ImageView::new_default(image).unwrap()
}

// MARK: Pipelines
procedural_macros::generate_pipelines_struct!();

impl Pipelines {
    fn new(device: &Arc<Device>, render_pass: &Arc<RenderPass>) -> Self {
        let subpass = Subpass::from(render_pass.clone(), 0).unwrap();

        procedural_macros::generate_pipelines_struct_impl_new!()
    }

    procedural_macros::generate_pipelines_struct_impl_bind_functions!();

    fn bind_instanced_simple_lit_colour_3d(
        &self,
        command_buffer_builder: &mut AutoCommandBufferBuilder<PrimaryAutoCommandBuffer>,
        allocators: &Allocators,
        camera_uniform: Subbuffer<instanced_simple_lit_colour_3d::CameraUniform>,
    ) {
        command_buffer_builder
            .bind_pipeline_graphics(self.instanced_simple_lit_colour_3d.clone())
            .unwrap()
            .bind_descriptor_sets(
                PipelineBindPoint::Graphics,
                self.instanced_simple_lit_colour_3d.layout().clone(),
                0,
                //TODO: Shouldn't this be persistent, as the name implies? Why is this being created every frame?
                PersistentDescriptorSet::new(
                    &allocators.descriptor_set_allocator,
                    self.instanced_simple_lit_colour_3d.layout().set_layouts()[0].clone(),
                    [WriteDescriptorSet::buffer(0, camera_uniform)],
                    [],
                )
                .unwrap(),
            )
            .unwrap();
    }

    fn bind_simple_lit_colour_3d(
        &self,
        command_buffer_builder: &mut AutoCommandBufferBuilder<PrimaryAutoCommandBuffer>,
        allocators: &Allocators,
        // Same layout as instanced version, so this is fine.
        camera_uniform: Subbuffer<instanced_simple_lit_colour_3d::CameraUniform>,
    ) {
        command_buffer_builder
            .bind_pipeline_graphics(self.simple_lit_colour_3d.clone())
            .unwrap()
            .bind_descriptor_sets(
                PipelineBindPoint::Graphics,
                self.simple_lit_colour_3d.layout().clone(),
                0,
                //TODO: Shouldn't this be persistent, as the name implies? Why is this being created every frame?
                PersistentDescriptorSet::new(
                    &allocators.descriptor_set_allocator,
                    self.simple_lit_colour_3d.layout().set_layouts()[0].clone(),
                    [WriteDescriptorSet::buffer(0, camera_uniform)],
                    [],
                )
                .unwrap(),
            )
            .unwrap();
    }

    fn bind_instanced_unlit_uv_2d_stretch(
        &self,
        command_buffer_builder: &mut AutoCommandBufferBuilder<PrimaryAutoCommandBuffer>,
        sampler_and_image_descriptor_set: &Arc<PersistentDescriptorSet>,
    ) {
        command_buffer_builder
            .bind_pipeline_graphics(self.instanced_unlit_uv_2d_stretch.clone())
            .unwrap()
            .bind_descriptor_sets(
                PipelineBindPoint::Graphics,
                self.instanced_unlit_uv_2d_stretch.layout().clone(),
                0,
                sampler_and_image_descriptor_set.clone(),
            )
            .unwrap();
    }

    fn bind_instanced_text_sdf(
        &self,
        command_buffer_builder: &mut AutoCommandBufferBuilder<PrimaryAutoCommandBuffer>,
        allocators: &Allocators,
        sampler_and_image_descriptor_set: &Arc<PersistentDescriptorSet>,
        font_uniform: Subbuffer<instanced_text_sdf::FontUniform>,
    ) {
        command_buffer_builder
            .bind_pipeline_graphics(self.instanced_text_sdf.clone())
            .unwrap()
            .bind_descriptor_sets(
                PipelineBindPoint::Graphics,
                self.instanced_text_sdf.layout().clone(),
                0,
                (
                    PersistentDescriptorSet::new(
                        &allocators.descriptor_set_allocator,
                        self.instanced_text_sdf.layout().set_layouts()[0].clone(),
                        [WriteDescriptorSet::buffer(0, font_uniform)],
                        [],
                    )
                    .unwrap(),
                    sampler_and_image_descriptor_set.clone(),
                ),
            )
            .unwrap();
    }
}

procedural_macros::generate_buffers_struct! {}

impl ProcBuffers {
    fn new(allocators: &Allocators, context: &VulkanoContext) -> Self {
        let mut command_buffer_builder = AutoCommandBufferBuilder::primary(
            &allocators.command_buffer_allocator,
            context.graphics_queue().queue_family_index(),
            CommandBufferUsage::OneTimeSubmit,
        )
        .unwrap();

        procedural_macros::generate_buffers_struct_impl_new!();

        command_buffer_builder
            .build()
            .unwrap()
            .execute(context.graphics_queue().clone())
            .unwrap()
            .then_signal_fence_and_flush()
            .unwrap()
            .wait(None)
            .unwrap();

        buffers
    }
}

/// Don't forget to build and execute the command buffer.
fn new_device_buffer<I, E>(
    source: I,
    memory_allocator: &Arc<StandardMemoryAllocator>,
    command_buffer_builder: &mut AutoCommandBufferBuilder<PrimaryAutoCommandBuffer>,
) -> Subbuffer<[E]>
where
    E: BufferContents,
    I: IntoIterator<Item = E>,
    I::IntoIter: ExactSizeIterator,
{
    // Go to Vulkano's Buffer documentation. I basically copied the staging example.

    let iter = source.into_iter();
    let len = iter.len();

    let staging = Buffer::from_iter(
        memory_allocator.clone(),
        BufferCreateInfo {
            usage: BufferUsage::TRANSFER_SRC,
            ..Default::default()
        },
        AllocationCreateInfo {
            memory_type_filter: MemoryTypeFilter::PREFER_HOST
                | MemoryTypeFilter::HOST_SEQUENTIAL_WRITE,
            ..Default::default()
        },
        iter,
    )
    .unwrap();

    let device = Buffer::new_slice::<E>(
        memory_allocator.clone(),
        BufferCreateInfo {
            usage: BufferUsage::VERTEX_BUFFER | BufferUsage::TRANSFER_DST,
            ..Default::default()
        },
        AllocationCreateInfo {
            memory_type_filter: MemoryTypeFilter::PREFER_DEVICE,
            ..Default::default()
        },
        len as DeviceSize,
    )
    .unwrap();

    command_buffer_builder
        .copy_buffer(CopyBufferInfo::buffers(staging, device.clone()))
        .unwrap();

    device
}

#[derive(Debug)]
pub enum CuboidColourPotentialInstance {
    PhysicsWithColour { body_index: usize, colour: [f32; 4] },
}

impl CuboidColourPotentialInstance {
    #[inline]
    fn to_instance(&self, bodies: &[Body]) -> Option<instanced_simple_lit_colour_3d::Instance> {
        match self {
            Self::PhysicsWithColour { body_index, colour } => {
                let body = &bodies[*body_index];
                Some(instanced_simple_lit_colour_3d::Instance::new(
                    *colour,
                    Matrix4::from_translation(body.position_unchecked())
                        * Matrix4::from_scale(mul_3d_by_1d(body.half_size_unchecked(), 2.0)),
                ))
            }
        }
    }
}

// MARK: Hotel
/// A hotel, you can request a room, and you will be given a key.
/// When you leave, you give the hotel the key, and now someone else can use that room.
///
/// As soon as I realised that this was like a hotel, I searched the internet and found a crate already exists: https://crates.io/crates/hotel
/// I have used that to improve this struct. Hopefully they won't mind.
#[derive(Clone, Debug, PartialEq)]
pub struct Hotel<T> {
    pub vec: Vec<Option<T>>,
    pub dead_indices: Vec<usize>,
}

impl<T> Hotel<T> {
    pub fn new(vec: Vec<Option<T>>, dead_indices_starting_capacity: usize) -> Self {
        Self {
            vec,
            dead_indices: Vec::with_capacity(dead_indices_starting_capacity),
        }
    }

    /// Pushes value into the first empty room, and returns the key.
    /// If no rooms are empty, then it adds a new room to the vec.
    #[inline]
    pub fn push(&mut self, value: T) -> usize {
        match self.dead_indices.pop() {
            Some(index) => {
                self.vec[index] = Some(value);
                index
            }
            None => {
                let index = self.vec.len();
                self.vec.push(Some(value));
                index
            }
        }
    }

    #[inline]
    pub fn remove(&mut self, index: usize) -> T {
        self.dead_indices.push(index);
        self.vec[index]
            .take()
            .expect("Index should have been given by the hotel.")
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.vec.len() - self.dead_indices.len()
    }
}

impl<T> Index<usize> for Hotel<T> {
    type Output = T;

    #[inline]
    fn index(&self, index: usize) -> &Self::Output {
        self.vec[index].as_ref().unwrap()
    }
}

impl<T> IndexMut<usize> for Hotel<T> {
    #[inline]
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.vec[index].as_mut().unwrap()
    }
}

// MARK: Subbuffer
trait SubbufferAllocatorExtension {
    fn from_slice<T>(&self, source: &[T]) -> Subbuffer<[T]>
    where
        T: Copy + BufferContents;
}

impl SubbufferAllocatorExtension for SubbufferAllocator {
    fn from_slice<T>(&self, source: &[T]) -> Subbuffer<[T]>
    where
        T: Copy + BufferContents,
    {
        let buffer = self.allocate_slice(source.len() as DeviceSize).unwrap();
        buffer.write().unwrap().copy_from_slice(source);
        buffer
    }
}
