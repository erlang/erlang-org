export default async (request, context) => {
  const url = new URL(request.url)
  const path = url.pathname

  // If the path already has a trailing slash or a file extension, let Netlify serve it.
  if (path.endsWith('/') || path.split('/').pop().includes('.')) {
    return context.next()
  }

  // Prefer `path.html` if it exists (e.g. /doc/apps/erts.html for the app overview).
  const htmlTarget = new URL(path + '.html', url)
  const htmlResp = await fetch(htmlTarget, { method: 'HEAD' })
  if (htmlResp.ok) {
    return Response.redirect(htmlTarget, 301)
  }

  // Otherwise, if the path is a directory (has an index), redirect with a trailing slash
  // so relative links inside the index page resolve correctly.
  const dirTarget = new URL(path + '/', url)
  const dirResp = await fetch(dirTarget, { method: 'HEAD' })
  if (dirResp.ok) {
    return Response.redirect(dirTarget, 301)
  }

  return context.next()
}
