# videofilter

Pandoc filter that allows you to embed YouTube or Vimeo videos into HTML using regular image embedding syntax.

Pass the path of the filter's binary to Pandoc via `--filter` command line option to enable it.

I've only tested it with Markdown, but it should also work with other input formats that support embedded images. The only supported output format is HTML.

## Markdown examples

### Embed Vimeo video

The number is the Vimeo's video id that you can find in the URL https://vimeo.com/95369722.

```
![](vimeo:95369722)
```

### Embed YouTube video

"FEFETKhhq8w" is the YouTube video id in the URL https://www.youtube.com/watch?v=FEFETKhhq8w

```
![](youtube:FEFETKhhq8w)
```

### Specify the video size

```
![400x300](youtube:FEFETKhhq8w)
```

### Set video size with an environment variable

Set `VIDEO_DIMENSIONS` environment variable to the desired video size in the same format `123x123` and it would be used by default for all videos without a specified size.
