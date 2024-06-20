#![cfg(feature = "openxr")]
use crate::{
    TestApi,
    parsing_test,
};

#[derive(Debug, Clone, Copy)]
pub struct OpenXR;

impl TestApi for OpenXR {
    const MAIN_URL: Option<&'static str> = None;
    const ALLOW_WARNINGS: bool = true;
    const USE_VKXML: bool = false;
    fn download_url(major: u32, minor: u32, patch: u32, _url_suffix: &str) -> String {
        format!(
            "https://github.com/KhronosGroup/OpenXR-SDK-Source/raw/release-{}.{}.{}/specification/registry/xr.xml",
            major, minor, patch
        )
    }
}

macro_rules! test_xr_versions {
    ($($test_name:ident($major:expr, $minor:expr, $patch:expr)),+ $(,)?) => {
        $(
            #[test]
            fn $test_name() {
                parsing_test::<OpenXR>($major, $minor, $patch, "");
            }
        )+
    };
}

test_xr_versions! {
    xr_test_v1_0_33(1, 0, 33),
    xr_test_v1_0_34(1, 0, 34),
}
