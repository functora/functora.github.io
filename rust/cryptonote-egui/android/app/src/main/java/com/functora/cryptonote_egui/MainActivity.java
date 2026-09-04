package com.functora.cryptonote_egui;

import android.app.Activity;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.graphics.ImageFormat;
import android.graphics.SurfaceTexture;
import android.hardware.Camera;
import android.net.Uri;
import android.os.Bundle;
import android.provider.OpenableColumns;
import android.util.Log;
import android.view.View;
import android.view.WindowManager;

import androidx.activity.OnBackPressedCallback;
import androidx.core.view.WindowCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.core.view.WindowInsetsControllerCompat;

import com.functora.Waker;
import com.google.androidgamesdk.GameActivity;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.ArrayList;

public class MainActivity extends GameActivity {

    static {
        System.loadLibrary("cryptonote_egui");
    }

    // ------------------------------------------------------------------
    // System back handling (predictive back / gesture + 3-button)
    // Polled from Rust to drive NavHistory navigation without exiting.
    // ------------------------------------------------------------------

    private boolean backPressed = false;
    private OnBackPressedCallback backCallback;

    public synchronized boolean peekBackPressed() {
        return backPressed;
    }

    public synchronized boolean pollBackPressed() {
        boolean v = backPressed;
        backPressed = false;
        return v;
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

    // ------------------------------------------------------------------
    // File picker support (Storage Access Framework, polled from Rust)
    // ------------------------------------------------------------------

    private static final String TAG_FILE_PICKER = "FunctoraFilePicker";
    private static final int FILE_PICKER_REQUEST_CODE = 4243;

    private ArrayList<Uri> filePickerUris = null;
    private boolean filePickerPending = false;
    private boolean filePickerCancelled = false;

    public synchronized void filePickerStart(boolean multiple) {
        filePickerUris = null;
        filePickerCancelled = false;
        filePickerPending = true;
        runOnUiThread(
                new Runnable() {
                    @Override
                    public void run() {
                        try {
                            Intent intent = new Intent(Intent.ACTION_OPEN_DOCUMENT);
                            intent.addCategory(Intent.CATEGORY_OPENABLE);
                            intent.setType("*/*");
                            intent.putExtra(Intent.EXTRA_ALLOW_MULTIPLE, multiple);
                            intent.addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION);
                            intent.addFlags(Intent.FLAG_GRANT_PERSISTABLE_URI_PERMISSION);
                            startActivityForResult(
                                    Intent.createChooser(intent, "Pick file"),
                                    FILE_PICKER_REQUEST_CODE);
                        } catch (Throwable t) {
                            Log.w(TAG_FILE_PICKER, "start failed", t);
                            synchronized (MainActivity.this) {
                                filePickerPending = false;
                                filePickerCancelled = true;
                            }
                        }
                    }
                });
    }

    /** 0 = waiting, 1 = done, -1 = cancelled, -2 = idle. */
    public synchronized int filePickerState() {
        if (filePickerPending) {
            return 0;
        }
        if (filePickerCancelled) {
            return -1;
        }
        if (filePickerUris != null) {
            return 1;
        }
        return -2;
    }

    public synchronized String[] filePickerNames() {
        if (filePickerUris == null) {
            return new String[0];
        }
        String[] out = new String[filePickerUris.size()];
        for (int i = 0; i < filePickerUris.size(); i++) {
            out[i] = getDisplayName(filePickerUris.get(i));
        }
        return out;
    }

    public synchronized byte[][] filePickerBytes() {
        if (filePickerUris == null) {
            return new byte[0][];
        }
        byte[][] out = new byte[filePickerUris.size()][];
        for (int i = 0; i < filePickerUris.size(); i++) {
            out[i] = readUriBytes(filePickerUris.get(i));
            if (out[i] == null) {
                out[i] = new byte[0];
            }
        }
        return out;
    }

    public synchronized void filePickerClear() {
        filePickerUris = null;
        filePickerPending = false;
        filePickerCancelled = false;
    }

    private String getDisplayName(Uri uri) {
        String name = null;
        Cursor cursor = null;
        try {
            cursor = getContentResolver().query(uri, null, null, null, null);
            if (cursor != null && cursor.moveToFirst()) {
                int idx = cursor.getColumnIndex(OpenableColumns.DISPLAY_NAME);
                if (idx >= 0) {
                    name = cursor.getString(idx);
                }
            }
        } catch (Throwable t) {
            Log.w(TAG_FILE_PICKER, "getDisplayName query failed", t);
        } finally {
            if (cursor != null) {
                try {
                    cursor.close();
                } catch (Throwable ignored) {
                }
            }
        }
        if (name == null || name.isEmpty()) {
            String path = uri.getLastPathSegment();
            if (path != null && !path.isEmpty()) {
                int slash = path.lastIndexOf('/');
                name = slash >= 0 ? path.substring(slash + 1) : path;
            }
        }
        if (name == null || name.isEmpty()) {
            name = "file";
        }
        return name;
    }

    private byte[] readUriBytes(Uri uri) {
        try {
            try {
                getContentResolver().takePersistableUriPermission(uri, Intent.FLAG_GRANT_READ_URI_PERMISSION);
            } catch (Throwable ignored) {
            }
            InputStream is = getContentResolver().openInputStream(uri);
            if (is == null) {
                return new byte[0];
            }
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            byte[] buf = new byte[8192];
            int n;
            while ((n = is.read(buf)) != -1) {
                baos.write(buf, 0, n);
            }
            try {
                is.close();
            } catch (Throwable ignored) {
            }
            return baos.toByteArray();
        } catch (Throwable t) {
            Log.w(TAG_FILE_PICKER, "readUriBytes failed", t);
            return new byte[0];
        }
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        if (requestCode == FILE_PICKER_REQUEST_CODE) {
            synchronized (this) {
                if (resultCode == Activity.RESULT_OK && data != null) {
                    ArrayList<Uri> uris = new ArrayList<>();
                    if (data.getClipData() != null) {
                        int count = data.getClipData().getItemCount();
                        for (int i = 0; i < count; i++) {
                            Uri u = data.getClipData().getItemAt(i).getUri();
                            if (u != null) {
                                uris.add(u);
                            }
                        }
                    } else if (data.getData() != null) {
                        uris.add(data.getData());
                    }
                    filePickerUris = uris;
                    filePickerCancelled = false;
                } else {
                    filePickerUris = null;
                    filePickerCancelled = true;
                }
                filePickerPending = false;
            }
        }
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
        getOnBackPressedDispatcher()
                .addCallback(
                        this,
                        backCallback =
                                new OnBackPressedCallback(true) {
                                    @Override
                                    public void handleOnBackPressed() {
                                        synchronized (MainActivity.this) {
                                            backPressed = true;
                                        }
                                        try {
                                            Waker.wake();
                                        } catch (Throwable ignored) {
                                        }
                                    }
                                });
    }

    @Override
    protected void onResume() {
        super.onResume();
        hideSystemUI();
    }
}