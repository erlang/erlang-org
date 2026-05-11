export default async (request, context) => {
  const url = new URL(request.url)
  const path = url.pathname

  // If the path already has a trailing slash or a file extension, let Netlify serve it.
  if (path.endsWith('/') || path.split('/').pop().includes('.')) {
    return context.next()
  }

  // Probe `path/index.html` explicitly. Netlify will silently serve a directory's
  // index.html when `path.html` is requested, so a `path.html` probe is useless
  // for telling files and directories apart. `path/index.html` only returns 200
  // when there's a real index file behind it.
  const indexProbe = new URL(path + '/index.html', url)
  const indexResp = await fetch(indexProbe, { method: 'HEAD' })
  if (indexResp.ok) {
    // Directory page: redirect to the trailing-slash URL so relative links in
    // the served index.html resolve against the right base.
    return Response.redirect(new URL(path + '/', url), 301)
  }

  // Otherwise, try the `.html` sibling.
  const htmlProbe = new URL(path + '.html', url)
  const htmlResp = await fetch(htmlProbe, { method: 'HEAD' })
  if (htmlResp.ok) {
    return Response.redirect(htmlProbe, 301)
  }

  return context.next()
}
