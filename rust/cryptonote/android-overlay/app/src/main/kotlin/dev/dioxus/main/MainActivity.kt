package dev.dioxus.main

import android.content.Intent
import android.net.Uri
import android.os.Bundle

typealias BuildConfig = com.functora.cryptonote.BuildConfig

class MainActivity : WryActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        if (webViewInitialized) {
            handleDeepLinkIntent(intent)
            finish()
            return
        }
        webViewInitialized = true
        super.onCreate(savedInstanceState)
        handleDeepLinkIntent(intent)
    }

    override fun onNewIntent(intent: Intent) {
        handleDeepLinkIntent(intent)
        super.onNewIntent(intent)
    }

    private fun handleDeepLinkIntent(intent: Intent) {
        intent.getByteArrayExtra(BYTES)?.let(::handleDeepLinkFile)
        when (intent.action) {
            Intent.ACTION_VIEW -> intent.data?.let { data ->
                if (data.scheme == "https") {
                    handleDeepLink(data.toString())
                } else {
                    readUriBytes(data)?.let { handleDeepLinkFile(it) }
                }
            }
            Intent.ACTION_SEND -> {
                @Suppress("DEPRECATION")
                val uri = intent.getParcelableExtra<Uri>(Intent.EXTRA_STREAM)
                readUriBytes(uri)?.let { handleDeepLinkFile(it) }
            }
        }
    }

    private fun readUriBytes(uri: Uri?): ByteArray? = runCatching {
        uri?.let { contentResolver.openInputStream(it)?.use { stream -> stream.readBytes() } }
    }.getOrNull()

    companion object {
        const val BYTES = "bytes"

        @Volatile
        private var webViewInitialized = false

        @JvmStatic
        private external fun handleDeepLink(url: String)

        @JvmStatic
        private external fun handleDeepLinkFile(bytes: ByteArray)
    }
}
