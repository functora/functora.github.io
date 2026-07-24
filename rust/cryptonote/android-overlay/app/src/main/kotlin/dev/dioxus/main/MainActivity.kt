package dev.dioxus.main

import android.content.Intent
import android.net.Uri
import android.webkit.WebView

typealias BuildConfig = com.functora.cryptonote.BuildConfig

class MainActivity : WryActivity() {
    private var deepLinkWebView: WebView? = null

    override fun onWebViewCreate(webView: WebView) {
        deepLinkWebView = webView
    }

    override fun onNewIntent(intent: Intent) {
        super.onNewIntent(intent)
        if (intent.action == Intent.ACTION_VIEW) {
            val url = intent.data?.toString() ?: return
            val query = Uri.parse(url).encodedQuery ?: return
            deepLinkWebView?.loadUrl("file:///android_asset/index.html?$query")
        }
    }
}