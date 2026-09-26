import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import tomllib
import unittest


RUST = Path(__file__).resolve().parents[2]
HEAVY_CORE = {
    "aes-gcm", "chacha20poly1305", "argon2", "zip", "ammonia",
    "pulldown-cmark", "image", "mp4", "rust_h264", "rxing",
}
PLATFORM = {"arboard", "rfd", "directories", "pollster", "threadpool"}


def dependencies(crate, *features):
    output = subprocess.run(
        [
            os.environ.get("CARGO", "cargo"), "tree", "--offline", "--locked",
            "--manifest-path", str(RUST / crate / "Cargo.toml"),
            "--target", "x86_64-unknown-linux-gnu",
            "--edges", "normal,build", "--prefix", "none", "--format", "{p}",
            *features,
        ],
        check=True, capture_output=True, text=True, timeout=120,
    ).stdout
    return {line.split()[0] for line in output.splitlines() if line.strip()}


class DependencyFeatures(unittest.TestCase):
    def test_features_compile_without_defaults(self):
        for crate in ["functora-core", "functora-egui"]:
            manifest = RUST / crate / "Cargo.toml"
            features = tomllib.loads(manifest.read_text(encoding="utf-8"))["features"]
            for feature in [None, *(name for name in features if name != "default")]:
                with self.subTest(crate=crate, feature=feature):
                    result = subprocess.run(
                        [
                            os.environ.get("CARGO", "cargo"), "check", "--offline",
                            "--locked", "--all-targets", "--no-default-features",
                            "--manifest-path", str(manifest),
                            *(["--features", feature] if feature else []),
                        ],
                        capture_output=True, text=True, timeout=300, check=False,
                    )
                    self.assertEqual(result.returncode, 0, result.stderr)

    def test_core_features_can_be_enabled_by_other_dependents(self):
        with tempfile.TemporaryDirectory() as directory:
            project = Path(directory)
            (project / "src").mkdir()
            (project / "src/lib.rs").write_text(
                "pub use functora_egui::camera::start_camera;\n",
                encoding="ascii",
            )
            (project / "Cargo.toml").write_text(
                '[package]\nname = "feature-unification-test"\n'
                'version = "0.0.0"\nedition = "2024"\n[dependencies]\n'
                'functora-egui = { path = '
                + json.dumps(str(RUST / "functora-egui"))
                + ', default-features = false, features = ["platform"] }\n'
                + 'functora-core = { path = '
                + json.dumps(str(RUST / "functora-core")) + ' }\n',
                encoding="ascii",
            )
            shutil.copyfile(RUST / "functora-egui/Cargo.lock", project / "Cargo.lock")
            result = subprocess.run(
                [
                    os.environ.get("CARGO", "cargo"), "check", "--offline", "--lib",
                    "--manifest-path", str(project / "Cargo.toml"),
                    "--target-dir", os.environ.get(
                        "CARGO_TARGET_DIR", str(RUST / "functora-egui/target"),
                    ),
                ],
                capture_output=True, text=True, timeout=300, check=False,
            )
            self.assertEqual(result.returncode, 0, result.stderr)

    def test_core_minimal(self):
        self.assertFalse(HEAVY_CORE & dependencies("functora-core", "--no-default-features"))

    def test_widgets_minimal(self):
        self.assertFalse(
            (HEAVY_CORE | PLATFORM)
            & dependencies("functora-egui", "--no-default-features")
        )

    def test_core_defaults_preserve_capabilities(self):
        self.assertTrue(HEAVY_CORE <= dependencies("functora-core"))

    def test_widgets_defaults_preserve_capabilities(self):
        self.assertTrue((HEAVY_CORE | PLATFORM) <= dependencies("functora-egui"))

    def test_core_capabilities_are_independent(self):
        capabilities = {
            "crypto": {"aes-gcm", "chacha20poly1305", "argon2"},
            "zip": {"zip"},
            "package": {"aes-gcm", "chacha20poly1305", "argon2", "zip"},
            "markdown": {"ammonia", "pulldown-cmark"},
            "thumbnail": {"image", "mp4", "rust_h264"},
            "qr": {"rxing"},
        }
        for feature, expected in capabilities.items():
            with self.subTest(feature=feature):
                self.assertEqual(
                    HEAVY_CORE & dependencies(
                        "functora-core", "--no-default-features", "--features", feature,
                    ),
                    expected,
                )

    def test_widget_capabilities_are_independent(self):
        capabilities = {
            "crypto": {"aes-gcm", "chacha20poly1305", "argon2"},
            "zip": {"zip"},
            "package": {"aes-gcm", "chacha20poly1305", "argon2", "zip"},
            "html-markdown": {"ammonia", "pulldown-cmark"},
            "thumbnail": {"image", "mp4", "rust_h264"},
            "qr": {"rxing"},
            "platform": set(),
            "clipboard": {"arboard", "image", "pollster", "threadpool"},
            "files": {"rfd", "pollster"},
            "runtime": {"pollster", "threadpool"},
            "storage": {"directories"},
        }
        for feature, expected in capabilities.items():
            with self.subTest(feature=feature):
                self.assertEqual(
                    (HEAVY_CORE | PLATFORM) & dependencies(
                        "functora-egui", "--no-default-features", "--features", feature,
                    ),
                    expected,
                )


if __name__ == "__main__":
    unittest.main()
