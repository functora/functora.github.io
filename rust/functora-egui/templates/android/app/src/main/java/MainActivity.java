package {{ package }};

import android.app.Activity;
import android.content.pm.PackageManager;
import android.graphics.ImageFormat;
import android.graphics.SurfaceTexture;
import android.hardware.Camera;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.WindowManager;

import androidx.core.view.WindowCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.core.view.WindowInsetsControllerCompat;

import com.google.androidgamesdk.GameActivity;

public class MainActivity extends GameActivity {

    static {
        System.loadLibrary("{{ lib_name }}");
    }

    // ------------------------------------------------------------------
    // Camera support (polled from Rust over JNI on this Activity instance,
    // whose class is always resolvable regardless of thread class loaders).
    // ------------------------------------------------------------------

    private static final String TAG_CAMERA = "FunctoraCamera";
    private static final String CAMERA_PERMISSION = "android.permission.CAMERA";
    private static final int CAMERA_REQUEST_CODE = 4242;

    private Camera camera;
    private SurfaceTexture cameraTexture;
    private byte[] latestFrame;
    private int frameWidth;
    private int frameHeight;
    private boolean cameraPermissionRequested;

    public synchronized int cameraPermissionState() {
        return checkSelfPermission(CAMERA_PERMISSION)
                        == PackageManager.PERMISSION_GRANTED
                ? 1
                : 0;
    }

    public synchronized void cameraRequestPermission() {
        if (!cameraPermissionRequested) {
            cameraPermissionRequested = true;
            requestPermissions(new String[] {CAMERA_PERMISSION}, CAMERA_REQUEST_CODE);
        }
    }

    /** 1 = running, 0 = waiting for permission, -1 = failed. */
    public synchronized int cameraStart(int maxDim) {
        if (camera != null) {
            return 1;
        }
        if (cameraPermissionState() != 1) {
            cameraRequestPermission();
            return 0;
        }
        try {
            Camera opened = Camera.open();
            Camera.Parameters params = opened.getParameters();
            Camera.Size size = pickPreviewSize(params, maxDim);
            params.setPreviewSize(size.width, size.height);
            params.setPreviewFormat(ImageFormat.NV21);
            if (params.getSupportedFocusModes()
                    .contains(Camera.Parameters.FOCUS_MODE_CONTINUOUS_VIDEO)) {
                params.setFocusMode(Camera.Parameters.FOCUS_MODE_CONTINUOUS_VIDEO);
            }
            opened.setParameters(params);
            SurfaceTexture surfTex = new SurfaceTexture(0);
            surfTex.setDefaultBufferSize(size.width, size.height);
            opened.setPreviewTexture(surfTex);
            opened.startPreview();
            camera = opened;
            cameraTexture = surfTex;
            frameWidth = size.width;
            frameHeight = size.height;
            latestFrame = null;
            armOneShot();
            return 1;
        } catch (Throwable t) {
            Log.w(TAG_CAMERA, "start failed", t);
            cameraStopInternal();
            return -1;
        }
    }

    /** Latest NV21 frame copy, or null when none captured yet. */
    public synchronized byte[] cameraPollFrame() {
        return latestFrame == null ? null : latestFrame.clone();
    }

    /** Packed frame dimensions: high 32 bits = width, low 32 bits = height. */
    public synchronized long cameraSizeCode() {
        return ((long) frameWidth << 32) | (frameHeight & 0xFFFFFFFFL);
    }

    public synchronized void cameraStop() {
        cameraStopInternal();
    }

    private void cameraStopInternal() {
        if (camera != null) {
            try {
                camera.setOneShotPreviewCallback(null);
            } catch (Throwable ignored) {
                // camera already in an error state
            }
            try {
                camera.stopPreview();
            } catch (Throwable ignored) {
                // preview not running
            }
            camera.release();
            camera = null;
        }
        if (cameraTexture != null) {
            cameraTexture.release();
            cameraTexture = null;
        }
        latestFrame = null;
    }

    private void armOneShot() {
        if (camera == null) {
            return;
        }
        camera.setOneShotPreviewCallback(
                new Camera.PreviewCallback() {
                    @Override
                    public void onPreviewFrame(byte[] data, Camera cam) {
                        latestFrame = data.clone();
                        synchronized (MainActivity.this) {
                            if (camera != null) {
                                armOneShot();
                            }
                        }
                    }
                });
    }

    private static Camera.Size pickPreviewSize(Camera.Parameters params, int maxDim) {
        Camera.Size best = null;
        for (Camera.Size size : params.getSupportedPreviewSizes()) {
            boolean fits = Math.max(size.width, size.height) <= maxDim;
            boolean better =
                    best == null
                            || fits && Math.max(best.width, best.height) > maxDim
                            || fits == (Math.max(best.width, best.height) <= maxDim)
                                    && size.width * (long) size.height
                                            > best.width * (long) best.height;
            if (better) {
                best = size;
            }
        }
        return best != null ? best : params.getSupportedPreviewSizes().get(0);
    }

    private void hideSystemUI() {
        getWindow().getAttributes().layoutInDisplayCutoutMode =
                WindowManager.LayoutParams.LAYOUT_IN_DISPLAY_CUTOUT_MODE_ALWAYS;
        View decorView = getWindow().getDecorView();
        WindowInsetsControllerCompat controller =
                new WindowInsetsControllerCompat(getWindow(), decorView);
        controller.hide(WindowInsetsCompat.Type.systemBars());
        controller.hide(WindowInsetsCompat.Type.displayCutout());
        controller.setSystemBarsBehavior(
                WindowInsetsControllerCompat.BEHAVIOR_SHOW_TRANSIENT_BARS_BY_SWIPE);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        WindowCompat.setDecorFitsSystemWindows(getWindow(), false);
        hideSystemUI();
        super.onCreate(savedInstanceState);
    }

    @Override
    protected void onResume() {
        super.onResume();
        hideSystemUI();
    }
}
