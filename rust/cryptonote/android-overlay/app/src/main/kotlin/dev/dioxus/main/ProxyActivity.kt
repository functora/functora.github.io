package dev.dioxus.main

import android.app.Activity
import android.content.Intent
import android.net.Uri
import android.os.Bundle
import android.util.Log
import java.io.File
import java.io.IOException
import kotlin.concurrent.thread

class ProxyActivity : Activity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        val uri = when (intent.action) {
            Intent.ACTION_VIEW -> if (intent.data?.scheme != "https") intent.data else null
            Intent.ACTION_SEND -> {
                @Suppress("DEPRECATION")
                intent.getParcelableExtra<Uri>(Intent.EXTRA_STREAM)
            }
            else -> null
        }
        if (uri == null) {
            Log.w(TAG, "No URI to relay")
            finish()
            return
        }
        thread(name = "cryptonote-copy") {
            val path = copyToCache(uri)
            runOnUiThread {
                relay(path)
                finish()
            }
        }
    }

    private fun relay(path: String?) {
        if (path == null) {
            Log.e(TAG, "Archive copy failed, not relaying")
            return
        }
        Log.i(TAG, "Relaying archive to MainActivity: $path")
        Intent(this, MainActivity::class.java)
            .putExtra(MainActivity.FILE_PATH, path)
            .addFlags(Intent.FLAG_ACTIVITY_NEW_TASK or Intent.FLAG_ACTIVITY_SINGLE_TOP)
            .let(::startActivity)
    }

    private fun copyToCache(uri: Uri): String? = runCatching {
        Log.i(TAG, "Copying $uri")
        val file = File(cacheDir, sanitize(uri.lastPathSegment ?: DEFAULT_NAME))
        val input = contentResolver.openInputStream(uri)
            ?: throw IOException("Cannot open $uri")
        input.use { input ->
            file.outputStream().use { output -> input.copyTo(output, BUFFER) }
        }
        Log.i(TAG, "Copied ${file.length()} bytes to ${file.absolutePath}")
        file.absolutePath
    }.onFailure { Log.e(TAG, "Copy failed for $uri", it) }.getOrNull()

    private fun sanitize(name: String): String =
        name.replace(INVALID_CHARS, "_").ifEmpty { DEFAULT_NAME }

    companion object {
        private const val TAG = "CryptonoteProxy"
        private const val BUFFER = 1 shl 16
        private const val DEFAULT_NAME = "archive.cryptonote"
        private val INVALID_CHARS = Regex("[^A-Za-z0-9._-]")
    }
}
