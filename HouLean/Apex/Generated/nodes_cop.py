@[apex_node "cop::LayersToGeo"]
opaque cop_LayersToGeo {numlayers: Nat} (layers : VariadicArg ImageLayer numlayers) {numgeos: Nat} (geos : VariadicArg Geometry numgeos) {numvdbs: Nat} (vdbs : VariadicArg NanoVDB numvdbs) : Geometry
@[apex_node "cop::autostereogram" has_rundata]
opaque cop_autostereogram (source_0 : ImageLayer) (depth_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::blend<0>" has_rundata]
opaque cop_blend0 (bg_0 : ImageLayer) (fg_1 : ImageLayer) (mask_2 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::blend<1>" has_rundata]
opaque cop_blend1 (bg_0 : NanoVDB) (fg_1 : NanoVDB) (mask_2 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB
@[apex_node "cop::blocktogeo" has_rundata]
opaque cop_blocktogeo (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : Geometry
@[apex_node "cop::blur<0>" has_rundata]
opaque cop_blur0 (source_0 : ImageLayer) (size_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::blur<1>" has_rundata]
opaque cop_blur1 (source_0 : NanoVDB) (size_1 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB
@[apex_node "cop::bright<0>" has_rundata]
opaque cop_bright0 (source_0 : ImageLayer) (bright_1 : ImageLayer) (shift_2 : ImageLayer) (mask_3 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::bright<1>" has_rundata]
opaque cop_bright1 (source_0 : NanoVDB) (bright_1 : NanoVDB) (shift_2 : NanoVDB) (mask_3 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB
@[apex_node "cop::cameraimport" has_rundata]
opaque cop_cameraimport (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::cameraproperties" has_rundata]
opaque cop_cameraproperties (source_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::channelextract<0>" has_rundata]
opaque cop_channelextract0 (channels_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::channelextract<1>" has_rundata]
opaque cop_channelextract1 (channels_0 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB
@[apex_node "cop::channeljoin<0>" has_rundata]
opaque cop_channeljoin0 (red_0 : ImageLayer) (green_1 : ImageLayer) (blue_2 : ImageLayer) (alpha_3 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::channeljoin<1>" has_rundata]
opaque cop_channeljoin1 (red_0 : NanoVDB) (green_1 : NanoVDB) (blue_2 : NanoVDB) (alpha_3 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB
@[apex_node "cop::channelsplit<0>" has_rundata]
opaque cop_channelsplit0 (rgba_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer×ImageLayer×ImageLayer×ImageLayer
@[apex_node "cop::channelsplit<1>" has_rundata]
opaque cop_channelsplit1 (rgba_0 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB×NanoVDB×NanoVDB×NanoVDB
@[apex_node "cop::channelswap<0>" has_rundata]
opaque cop_channelswap0 (channels_0 : ImageLayer) (mask_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::channelswap<1>" has_rundata]
opaque cop_channelswap1 (channels_0 : NanoVDB) (mask_1 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB
@[apex_node "cop::checkerboard" has_rundata]
opaque cop_checkerboard (size_ref_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::clamp<0>" has_rundata]
opaque cop_clamp0 (source_0 : ImageLayer) (lower_1 : ImageLayer) (upper_2 : ImageLayer) (mask_3 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::clamp<1>" has_rundata]
opaque cop_clamp1 (source_0 : NanoVDB) (lower_1 : NanoVDB) (upper_2 : NanoVDB) (mask_3 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB
@[apex_node "cop::constant<0>" has_rundata]
opaque cop_constant0 (source_0 : ImageLayer) (mask_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::constant<1>" has_rundata]
opaque cop_constant1 (source_0 : NanoVDB) (mask_1 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB
@[apex_node "cop::crop" has_rundata]
opaque cop_crop (source_0 : ImageLayer) (size_ref_1 : ImageLayer) (bounds_2 : Geometry) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::denoiseai" has_rundata]
opaque cop_denoiseai (source_0 : ImageLayer) (normal_1 : ImageLayer) (albedo_2 : ImageLayer) (motionvec_3 : ImageLayer) (prevframe_4 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::derivative" has_rundata]
opaque cop_derivative (source_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer×ImageLayer
@[apex_node "cop::distort" has_rundata]
opaque cop_distort (source_0 : ImageLayer) (dir_1 : ImageLayer) (scale_2 : ImageLayer) (mask_3 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::dot<0>" has_rundata]
opaque cop_dot0 (a_0 : ImageLayer) (b_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::dot<1>" has_rundata]
opaque cop_dot1 (a_0 : NanoVDB) (b_1 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB
@[apex_node "cop::eikonal" has_rundata]
opaque cop_eikonal (initial_dist_0 : ImageLayer) (speed_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::error<0>" has_rundata]
opaque cop_error0 (input1_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::error<1>" has_rundata]
opaque cop_error1 (input1_0 : Geometry) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : Geometry
@[apex_node "cop::error<2>" has_rundata]
opaque cop_error2 (input1_0 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB
@[apex_node "cop::flip" has_rundata]
opaque cop_flip (source_0 : ImageLayer) (mask_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::function<0>" has_rundata]
opaque cop_function0 (source_0 : ImageLayer) (mask_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::function<1>" has_rundata]
opaque cop_function1 (source_0 : NanoVDB) (mask_1 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB
@[apex_node "cop::geotolayer" has_rundata]
opaque cop_geotolayer (geometry_0 : Geometry) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::hextile" has_rundata]
opaque cop_hextile (direction_0 : ImageLayer) (texcoord_1 : ImageLayer) (weight_2 : ImageLayer) (textotile_3 : ImageLayer) (size_4 : ImageLayer) (scale_5 : ImageLayer) (rot_6 : ImageLayer) (contrast_7 : ImageLayer) (contrast_falloff_8 : ImageLayer) (weightexp_9 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer×ImageLayer
@[apex_node "cop::hsv" has_rundata]
opaque cop_hsv (source_0 : ImageLayer) (hueshift_1 : ImageLayer) (saturation_2 : ImageLayer) (value_3 : ImageLayer) (mask_4 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::idtorgb<0>" has_rundata]
opaque cop_idtorgb0 (id_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::idtorgb<1>" has_rundata]
opaque cop_idtorgb1 (id_0 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB
@[apex_node "cop::illpixel" has_rundata]
opaque cop_illpixel (source_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::invert<0>" has_rundata]
opaque cop_invert0 (source_0 : ImageLayer) (mask_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::invert<1>" has_rundata]
opaque cop_invert1 (source_0 : NanoVDB) (mask_1 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB
@[apex_node "cop::julia" has_rundata]
opaque cop_julia (size_ref_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::layer" has_rundata]
opaque cop_layer (size_ref_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::layerattribcreate" has_rundata]
opaque cop_layerattribcreate (source_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::layerattribdelete" has_rundata]
opaque cop_layerattribdelete (source_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::layerproperties<0>" has_rundata]
opaque cop_layerproperties0 (source_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::layerproperties<1>" has_rundata]
opaque cop_layerproperties1 (source_0 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB
@[apex_node "cop::layertogeo<0>" has_rundata]
opaque cop_layertogeo0 (layer_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : Geometry
@[apex_node "cop::layertogeo<1>" has_rundata]
opaque cop_layertogeo1 (layer_0 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : Geometry
@[apex_node "cop::layertopoints" has_rundata]
opaque cop_layertopoints (layer_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : Geometry
@[apex_node "cop::matchcamera" has_rundata]
opaque cop_matchcamera (source_0 : ImageLayer) (camera_ref_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::matchudim" has_rundata]
opaque cop_matchudim (source_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::median" has_rundata]
opaque cop_median (source_0 : ImageLayer) (mask_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::mono<0>" has_rundata]
opaque cop_mono0 (source_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::mono<1>" has_rundata]
opaque cop_mono1 (source_0 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB
@[apex_node "cop::monotorgb<0>" has_rundata]
opaque cop_monotorgb0 (source_0 : ImageLayer) (ramp_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::monotorgb<1>" has_rundata]
opaque cop_monotorgb1 (source_0 : NanoVDB) (ramp_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB
@[apex_node "cop::premult" has_rundata]
opaque cop_premult (source_0 : ImageLayer) (mask_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::projectonlayer" has_rundata]
opaque cop_projectonlayer (target_0 : ImageLayer) (source_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::ramp" has_rundata]
opaque cop_ramp (size_ref_0 : ImageLayer) (pos_1 : ImageLayer) (ramp_2 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::rasterizelayer" has_rundata]
opaque cop_rasterizelayer (camera_ref_0 : ImageLayer) (source_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer×ImageLayer
@[apex_node "cop::remap<0>" has_rundata]
opaque cop_remap0 (source_0 : ImageLayer) (ramp_1 : ImageLayer) (mask_2 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::remap<1>" has_rundata]
opaque cop_remap1 (source_0 : NanoVDB) (ramp_1 : ImageLayer) (mask_2 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB
@[apex_node "cop::resample" has_rundata]
opaque cop_resample (source_0 : ImageLayer) (size_ref_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::rgbatorgb" has_rundata]
opaque cop_rgbatorgb (rgba_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer×ImageLayer
@[apex_node "cop::rgbtorgba" has_rundata]
opaque cop_rgbtorgba (rgb_0 : ImageLayer) (alpha_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::solvepoissonmultigrid" has_rundata]
opaque cop_solvepoissonmultigrid (rhs_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::sopimport" has_rundata]
opaque cop_sopimport (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : Geometry
@[apex_node "cop::stash<0>" has_rundata]
opaque cop_stash0 (source_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::stash<1>" has_rundata]
opaque cop_stash1 (source_0 : Geometry) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : Geometry
@[apex_node "cop::stash<2>" has_rundata]
opaque cop_stash2 (source_0 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB
@[apex_node "cop::streakblur" has_rundata]
opaque cop_streakblur (source_0 : ImageLayer) (direction_1 : ImageLayer) (scalelength_2 : ImageLayer) (mask_3 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::twowayswitchifwired<0>" has_rundata]
opaque cop_twowayswitchifwired0 (test_0 : ImageLayer) (first_1 : ImageLayer) (second_2 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::twowayswitchifwired<1>" has_rundata]
opaque cop_twowayswitchifwired1 (test_0 : ImageLayer) (first_1 : Geometry) (second_2 : Geometry) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : Geometry
@[apex_node "cop::twowayswitchifwired<2>" has_rundata]
opaque cop_twowayswitchifwired2 (test_0 : Geometry) (first_1 : ImageLayer) (second_2 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::twowayswitchifwired<3>" has_rundata]
opaque cop_twowayswitchifwired3 (test_0 : Geometry) (first_1 : Geometry) (second_2 : Geometry) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : Geometry
@[apex_node "cop::twowayswitchifwired<4>" has_rundata]
opaque cop_twowayswitchifwired4 (test_0 : NanoVDB) (first_1 : NanoVDB) (second_2 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB
@[apex_node "cop::twowayswitchifwired<5>" has_rundata]
opaque cop_twowayswitchifwired5 (test_0 : NanoVDB) (first_1 : Geometry) (second_2 : Geometry) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : Geometry
@[apex_node "cop::twowayswitchifwired<6>" has_rundata]
opaque cop_twowayswitchifwired6 (test_0 : NanoVDB) (first_1 : ImageLayer) (second_2 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::twowayswitchifwired<7>" has_rundata]
opaque cop_twowayswitchifwired7 (test_0 : ImageLayer) (first_1 : NanoVDB) (second_2 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB
@[apex_node "cop::twowayswitchifwired<8>" has_rundata]
opaque cop_twowayswitchifwired8 (test_0 : Geometry) (first_1 : NanoVDB) (second_2 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB
@[apex_node "cop::vdbleafpoints" has_rundata]
opaque cop_vdbleafpoints (vdb_0 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : Geometry
@[apex_node "cop::vdbreshape" has_rundata]
opaque cop_vdbreshape (source_0 : NanoVDB) (ref_1 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB
@[apex_node "cop::vectorxform2d" has_rundata]
opaque cop_vectorxform2d (source_0 : ImageLayer) (mask_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::vectorxform<0>" has_rundata]
opaque cop_vectorxform0 (source_0 : ImageLayer) (mask_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::vectorxform<1>" has_rundata]
opaque cop_vectorxform1 (source_0 : NanoVDB) (mask_1 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB
@[apex_node "cop::xform2d" has_rundata]
opaque cop_xform2d (source_0 : ImageLayer) (mask_1 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::xform<0>" has_rundata]
opaque cop_xform0 (source_0 : ImageLayer) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : ImageLayer
@[apex_node "cop::xform<1>" has_rundata]
opaque cop_xform1 (source_0 : NanoVDB) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : NanoVDB
@[apex_node "cop::xform<2>" has_rundata]
opaque cop_xform2 (source_0 : Geometry) (signature : StringArray) (cwd_node : String) (requests : IntArray) (parms : Dict) (context_data : VerbContext) : Geometry
@[apex_node "dict::Build" has_rundata]
