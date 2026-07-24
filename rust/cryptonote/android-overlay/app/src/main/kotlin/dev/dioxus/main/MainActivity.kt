package dev.dioxus.main

import android.content.Intent
import android.os.Bundle

typealias BuildConfig = com.functora.cryptonote.BuildConfig

class MainActivity : WryActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        handleDeepLinkIntent(intent)
    }

    override fun onNewIntent(intent: Intent) {
        super.onNewIntent(intent)
        handleDeepLinkIntent(intent)
    }

    private fun handleDeepLinkIntent(intent: Intent) {
        if (intent.action == Intent.ACTION_VIEW) {
            intent.data?.toString()?.let { handleDeepLink(it) }
        }
    }

    companion object {
        @JvmStatic
        private external fun handleDeepLink(url: String)
    }
}