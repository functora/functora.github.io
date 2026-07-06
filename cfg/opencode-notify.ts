import { exec } from "node:child_process"

export default async () => {
  const notify = () =>
    exec("notify-user", (err) => {
      if (err) console.error("notify-user failed:", err.message)
    })

  return {
    event: async ({ event }: { event: { type: string } }) => {
      if (event.type === "session.idle" || event.type === "permission.asked" || event.type === "session.error") {
        notify()
      }
    },
  }
}
