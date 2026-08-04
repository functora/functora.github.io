package dev.dioxus.main

import android.app.Activity
import android.content.Intent
import android.net.Uri
import android.os.Bundle

class ProxyActivity : Activity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        when (intent.action) {
            Intent.ACTION_VIEW -> intent.data?.let { data ->
                if (data.scheme != "https") {
                    relay(readUriBytes(data))
                }
            }
            Intent.ACTION_SEND -> {
                @Suppress("DEPRECATION")
                val uri = intent.getParcelableExtra<Uri>(Intent.EXTRA_STREAM)
                relay(readUriBytes(uri))
            }
        }
        finish()
    }

    private fun relay(bytes: ByteArray?) {
        if (bytes == null) return
        Intent(this, MainActivity::class.java)
            .putExtra(MainActivity.BYTES, bytes)
            .addFlags(Intent.FLAG_ACTIVITY_NEW_TASK or Intent.FLAG_ACTIVITY_SINGLE_TOP)
            .let(::startActivity)
    }

    private fun readUriBytes(uri: Uri?): ByteArray? = runCatching {
        uri?.let { contentResolver.openInputStream(it)?.use { stream -> stream.readBytes() } }
    }.getOrNull()
}