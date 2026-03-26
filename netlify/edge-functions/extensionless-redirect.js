export default async (request, context) => {
  const path = new URL(request.url).pathname

  // Only redirect if the last path segment has no file extension
  if (path.endsWith('/') || path.split('/').pop().includes('.')) {
    return context.next()
  }

  return Response.redirect(new URL(path + '.html', request.url), 301)
}
