import { exec } from "node:child_process"

export default async () => {
  const notify = () =>
    exec("notify-user", (err) => {
      if (err) console.error("notify-user failed:", err.message)
    })

  const mainSessions = new Set<string>()

  const getSessionID = (event: any): string | null => {
    if (!event) return null
    if (event.properties?.sessionID) return event.properties.sessionID
    if (event.properties?.info?.id) return event.properties.info.id
    return null
  }

  const isIdleEvent = (event: any): boolean =>
    event.type === "session.idle" ||
    (event.type === "session.status" && event.properties?.status?.type === "idle")

  return {
    event: async ({ event }: { event: any }) => {
      const sessionID = getSessionID(event)

      if (event.type === "session.created" && sessionID) {
        const parent = event?.properties?.info?.parentID
        if (!parent) mainSessions.add(sessionID)
      }

      if (event.type === "session.updated" && sessionID) {
        const parent = event?.properties?.info?.parentID
        if (parent) mainSessions.delete(sessionID)
      }

      if (isIdleEvent(event) && sessionID && mainSessions.has(sessionID)) {
        notify()
      }

      if (event.type === "permission.asked" || event.type === "session.error") {
        notify()
      }
    },
  }
}
