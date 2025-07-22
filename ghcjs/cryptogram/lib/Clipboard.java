package com.capacitorjs.plugins.clipboard;

import android.content.ClipData;
import android.content.ClipDescription;
import android.content.ClipboardManager;
import android.content.Context;
import android.graphics.Bitmap;
import android.net.Uri;
import android.provider.MediaStore;
import android.util.Base64;
import com.getcapacitor.Logger;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;

public class Clipboard {

  private static final String TAG = "Clipboard";

  private Context context;
  private ClipboardManager clipboard;

  public Clipboard(Context context) {
    this.context = context;
    this.clipboard =
        (ClipboardManager)context.getSystemService(Context.CLIPBOARD_SERVICE);
  }

  /**
   * Writes provided content to the clipboard.
   *
   * @param label User-visible label for the clip data.
   * @param content The content to be written to the clipboard.
   * @return A response indicating the success status of the write request.
   */
  public ClipboardWriteResponse write(String label, String content) {
    ClipData data = ClipData.newPlainText(label, content);

    if (data != null && clipboard != null) {
      try {
        clipboard.setPrimaryClip(data);
      } catch (Exception e) {
        Logger.error(TAG, e);
        return new ClipboardWriteResponse(false,
                                          "Writing to the clipboard failed");
      }

      return new ClipboardWriteResponse(true);
    } else if (clipboard == null) {
      return new ClipboardWriteResponse(
          false, "Problem getting a reference to the system clipboard");
    } else {
      return new ClipboardWriteResponse(false, "Problem formatting data");
    }
  }

  /**
   * Reads data from the clipboard.
   * @return Data from the clipboard or null if no reference to the system
   *     clipboard.
   */
  public ClipboardData read() {
    if (clipboard != null) {
      CharSequence value = null;
      String type = "text/plain";

      if (clipboard.hasPrimaryClip()) {
        ClipData.Item item = clipboard.getPrimaryClip().getItemAt(0);

        if (clipboard.getPrimaryClipDescription().hasMimeType(
                ClipDescription.MIMETYPE_TEXT_PLAIN)) {
          Logger.debug(TAG, "Got plain text");
          value = item.getText();
        } else if (clipboard.getPrimaryClipDescription().hasMimeType(
                       ClipDescription.MIMETYPE_TEXT_HTML)) {
          Logger.debug(TAG, "Got HTML text");
          value = item.coerceToText(context).toString();
          type = "text/html";
        } else if (clipboard.getPrimaryClipDescription().hasMimeType(
                       "image/*")) {
          Logger.debug(TAG, "Got image");
          Uri imageUri = item.getUri();

          try {
            Bitmap bitmap = MediaStore.Images.Media.getBitmap(
                context.getContentResolver(), imageUri);
            if (bitmap != null) {
              ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
              bitmap.compress(Bitmap.CompressFormat.PNG, 100, outputStream);
              byte[] imageBytes = outputStream.toByteArray();
              String base64Image =
                  Base64.encodeToString(imageBytes, Base64.NO_WRAP);
              value = "data:image/png;base64," + base64Image;
              type = "image/png";
            }
          } catch (Exception e) {
            Logger.error(TAG, "Failed to read image from clipboard", e);
          }
        } else {
          Logger.debug(TAG, "Unknown MIME type, trying to coerce to text");
          value = item.coerceToText(context).toString();
        }
      }

      ClipboardData clipboardData = new ClipboardData();
      if (value != null) {
        clipboardData.setValue(value.toString());
      }
      clipboardData.setType(type);
      return clipboardData;
    }

    return null;
  }
}
