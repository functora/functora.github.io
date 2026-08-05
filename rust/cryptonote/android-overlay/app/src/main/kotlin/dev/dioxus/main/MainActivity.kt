package dev.dioxus.main

import android.content.Intent
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
        intent.getStringExtra(FILE_PATH)?.let(::handleDeepLinkFile)
        if (intent.action == Intent.ACTION_VIEW) {
            intent.data?.let { data ->
                if (data.scheme == "https") {
                    handleDeepLink(data.toString())
                }
            }
        }
    }

    companion object {
        const val FILE_PATH = "file_path"

        @Volatile
        private var webViewInitialized = false

        @JvmStatic
        private external fun handleDeepLink(url: String)

        @JvmStatic
        private external fun handleDeepLinkFile(path: String)
    }
}
