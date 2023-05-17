# Blogs posts

Blog posts are displayed under `/blog` and `/blog/{{title}}/`.

All images are located in [https://www.erlang.org/blog/images](/blog/images). If your post has many images,
place them in a folder dedicated to that blog post.

Use the code below as a template:

```markdown
---
layout: post
title: "My new blog item"
author: "John Doe"
---
The first paragraph will be shown on the index page. So make sure
that it makes sense in its own and is not too long.

## Sub title

Write any internal links (i.e. links to erlang.org) using the {% link blog/images/config.png %}
syntax so that jekyll will do a check that it links to something that exists.

Also remember that if markdown does not support doing something, <sup>you can always use html</sup>.
```

The markdown dialect used is [github flavored markdown](https://github.github.com/gfm/).