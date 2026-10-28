import { execFile } from "node:child_process"
import { Plugin } from "@opencode/plugin"

export default Plugin.define({
  id: "notify",
  setup(ctx) {
    const notify = () =>
      execFile("notify-user", (err) => {
        if (err) console.error("notify-user failed:", err.message)
      })

    const isMain = async (sessionID: string) => {
      try {
        return !(await ctx.session.get({ sessionID })).parentID
      } catch (err) {
        console.error("notify: could not resolve session:", err)
        return false
      }
    }

    const controller = new AbortController()

    void (async () => {
      for await (const event of ctx.event.subscribe({
        signal: controller.signal,
        types: ["session.execution.succeeded", "session.execution.failed", "permission.asked", "form.created"],
      })) {
        switch (event.type) {
          case "session.execution.succeeded":
          case "session.execution.failed":
            if (await isMain(event.data.sessionID)) notify()
            break
          case "permission.asked":
          case "form.created":
            notify()
            break
        }
      }
    })().catch((err) => console.error("notify: event stream failed:", err))

    return () => controller.abort()
  },
})
