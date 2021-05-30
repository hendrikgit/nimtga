import colors, math, os, sequtils, streams

type
  Tga* = object
    width*, height*, bpp*: int
    pixels*: seq[Pixel]

  Header = object
    idLength, colorMapType, imageType: uint8
    colorMapOrigin, colorMapLength: uint16
    colorMapEntrySize: uint8
    xOrigin, yOrigin, width, height: uint16
    bits, imageDescriptor: uint8
  
  Pixel* = object
    red*, green*, blue*, alpha*: uint8

func initTga*(width = 1, height = 1, color = colWhite, bpp = 24): Tga =
  if width < 1 or height < 1:
    raise newException(ValueError, "width or height < 1 are not supported. width: " & $width & ", height: " & $height)
  result.width = width
  result.height = height
  result.bpp = bpp
  let rgb = color.extractRGB
  let pixel = Pixel(red: rgb.r.uint8, green: rgb.g.uint8, blue: rgb.b.uint8, alpha: 255)
  result.pixels = newSeqWith(width * height, pixel)

func `$`*(tga: Tga): string =
  "(width: " & $tga.width & ", height: " & $tga.height & ", bpp: " & $tga.bpp & ", pixels: " & $tga.pixels.len & ")"

func fieldsSize(obj: object): int =
  for _, v in obj.fieldPairs:
    result += v.sizeof

func toBytes(header: Header): seq[uint8] =
  for _, v in header.fieldPairs:
    when v is uint8:
      result &= v
    when v is uint16:
      result &= v.uint8
      result &= (v shr 8).uint8

func idx*(tga: Tga, x, y: int): int =
  if x < 0 or x >= tga.width or y < 0 or y >= tga.height:
    raise newException(IndexDefect, "x or y out of bounds")
  (tga.height - 1 - y) * tga.width + x

proc setPixelAt*(tga: var Tga, pixel: Pixel, idx: int) =
  tga.pixels[idx] = pixel

proc setPixelAt*(tga: var Tga, pixel: Pixel, x, y: int) =
  tga.setPixelAt(pixel, tga.idx(x, y))

proc readTga*(data: Stream, size: int): Tga =
  data.setPosition 0
  let
    header = Header(
      idLength: data.readUint8,
      colorMapType: data.readUint8,
      imageType: data.readUint8,
      colorMapOrigin: data.readUint16,
      colorMapLength: data.readUint16,
      colorMapEntrySize: data.readUint8,
      xOrigin: data.readUint16,
      yOrigin: data.readUint16,
      width: data.readUint16,
      height: data.readUint16,
      bits: data.readUint8,
      imageDescriptor: data.readUint8
    )
    colorMapElementSize = header.colorMapEntrySize.int div 8
    colorMapSize = header.colorMapLength.int * colorMapElementSize
    pixelSize = if header.colorMapLength == 0: header.bits.int div 8 else: colorMapElementSize # bytes per pixel
    dataSize = size - header.fieldsSize - (if header.colorMapType == 1: colorMapSize else: 0)
    imageDataStart = header.fieldsSize + header.idLength.int + colorMapSize

  if header.colorMapType != 0:
    raise newException(ValueError, "Color map type not supported: " & $header.colorMapType)

  if header.xOrigin != 0 or header.yOrigin != 0:
    raise newException(ValueError, "Values other than 0 for x,y origin are not supported")

  if header.bits != 24 and header.bits != 32:
    raise newException(ValueError, "Unsupported pixel depth: " & $header.bits)

  result.width = header.width.int
  result.height = header.height.int
  result.bpp = header.bits.int

  data.setPosition imageDataStart

  case header.imageType
  of 2: # uncompressed rgb/truecolor image
    let imageData = cast[seq[uint8]](data.readStr(dataSize))
    for idx in countup(0, dataSize - pixelSize, step = pixelSize):
      # pixel color bytes are in the order of bgr(a)
      result.pixels &= Pixel(
        red: imageData[idx + 2],
        green: imageData[idx + 1],
        blue: imageData[idx],
        alpha: if pixelSize == 4: imageData[idx + 3] else: 255
      )
  of 10: # run-length encoded rgb
    # data contains header bytes with a count of how many pixels follow
    # it starts with a header byte, following header positions depend on pixelCount and type of repetition
    var idx = 0
    while idx < header.width.int * header.height.int:
      let header = data.readUint8
      let pixelCount = (header and 0x7f).int + 1
      idx += pixelCount
      # pixel color bytes are in the order of bgr(a)
      if (header and 0x80) != 0: # the same pixel is repeated pixelCount times
        let pixel = Pixel(blue: data.readUint8, green: data.readUint8, red: data.readUint8, alpha: if pixelSize == 4: data.readUint8 else: 255)
        for _ in 0 ..< pixelCount:
          result.pixels &= pixel
      else: # different pixels (but without header, pixelSize bytes long) follow pixelCount times
        for idxPixel in 0 ..< pixelCount:
          result.pixels &= Pixel(blue: data.readUint8, green: data.readUint8, red: data.readUint8, alpha: if pixelSize == 4: data.readUint8 else: 255)
  else:
    raise newException(ValueError, "Unsupported image type: " & $header.imageType)

proc readTga*(data: string): Tga =
  let stream = data.newStringStream
  result = readTga(stream, data.len)
  stream.close

proc readTgaFile*(filename: string): Tga =
  let stream = filename.openFileStream
  result = readTga(stream, filename.getFileSize.int)
  stream.close

proc write*(tga: Tga, filename: string) =
  # only writes type 2 images with 24 or 32 bpp
  if tga.bpp != 24 and tga.bpp != 32:
    raise newException(ValueError, "Unsupported pixel depth: " & $tga.bpp)
  let
    pixelSize = tga.bpp div 8
    header = Header(
      imageType: 2,
      width: tga.width.uint16,
      height: tga.height.uint16,
      bits: tga.bpp.uint8
    )
  var data = newSeq[uint8]()
  for b in header.toBytes:
    data &= b
  for idx, pixel in tga.pixels:
    # put bytes in order bgr(a)
    data &= pixel.blue
    data &= pixel.green
    data &= pixel.red
    if pixelSize == 4:
      data &= pixel.alpha
  filename.writeFile data

func rotate90cw*(tga: Tga): Tga =
  result.width = tga.height
  result.height = tga.width
  result.bpp = tga.bpp
  for x in countdown(tga.width - 1, 0):
    for y in 0 ..< tga.height:
      result.pixels &= tga.pixels[y * tga.width + x]

func concatR*(tga1: Tga, tgas: varargs[Tga]): Tga =
  result.width = tga1.width + tgas.mapIt(it.width).sum
  result.height = max(@[tga1.height] & tgas.mapIt(it.height))
  result.bpp = max(@[tga1.bpp] & tgas.mapIt(it.bpp))
  let whitePixel = Pixel(red: 255, green: 255, blue: 255, alpha: 255)
  for y in 0 ..< result.height:
    for tga in @[tga1] & @tgas:
      if y < tga.height:
        for p in tga.pixels[y * tga.width ..< (y + 1) * tga.width]:
          result.pixels &= p
      else:
        for x in 0 ..< tga.width:
          result.pixels &= whitePixel

func concatB*(tga1: Tga, tgas: varargs[Tga]): Tga =
  result.width = max(@[tga1.width] & tgas.mapIt(it.width))
  result.height = tga1.height + tgas.mapIt(it.height).sum
  result.bpp = max(@[tga1.bpp] & tgas.mapIt(it.bpp))
  let whitePixel = Pixel(red: 255, green: 255, blue: 255, alpha: 255)
  for tga in @[tga1] & @tgas:
    for y in 0 ..< tga.height:
      for p in tga.pixels[y * tga.width ..< (y + 1) * tga.width]:
        result.pixels &= p
      for x in tga.width ..< result.width:
        result.pixels &= whitePixel

func scale(tga: Tga, ratioX, ratioY: float): Tga =
  result.width = (tga.width.toFloat * ratioX).toInt
  result.height = (tga.height.toFloat * ratioY).toInt
  result.bpp = tga.bpp
  result.pixels.setLen(result.width * result.height)
  let scaleRatioX = 1 / ratioX
  let scaleRatioY = 1 / ratioY
  for y in 0 ..< result.height:
    for x in 0 ..< result.width:
      let px = (x.toFloat * scaleRatioX).floor
      let py = (y.toFloat * scaleRatioY).floor
      result.pixels[y * result.width + x] = tga.pixels[(py * tga.width.toFloat + px).toInt]

func scale*(tga: Tga, ratio: float): Tga =
  tga.scale(ratio, ratio)

proc scale*(tga: Tga, width, height: int): Tga =
  tga.scale(width / tga.width, height / tga.height)
