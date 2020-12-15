import os, sequtils, streams

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

proc read*(data: Stream, size: int): Tga =
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

  if header.xOrigin != 0 or header.yOrigin != 0:
    raise newException(ValueError, "Values other than 0 for x,y origin are not supported")

  result.width = header.width.int
  result.height = header.height.int
  result.bpp = header.bits.int

  data.setPosition imageDataStart

  case header.imageType
  of 2: # uncompressed rgb/truecolor image
    if header.bits == 24 or header.bits == 32:
      let imageData = cast[seq[uint8]](data.readStr(dataSize))
      for idx in countup(0, dataSize - pixelSize, step = pixelSize):
        # pixel color bytes are in the order of bgr(a)
        result.pixels &= Pixel(
          red: imageData[idx + 2],
          green: imageData[idx + 1],
          blue: imageData[idx],
          alpha: if pixelSize == 4: imageData[idx + 3] else: 255
        )
    else:
      raise newException(ValueError, "Unsupported pixel depth")
  else:
    raise newException(ValueError, "Unsupported image type")

proc read*(filename: string): Tga =
  let stream = filename.openFileStream
  result = read(stream, filename.getFileSize.int)
  stream.close

proc write*(tga: Tga, filename: string) =
  # only writes type 2 images with 24 or 32 bpp
  if tga.bpp != 24 and tga.bpp != 32:
    raise newException(ValueError, "Unsupported pixel depth")
  let
    pixelSize = tga.bpp div 8
    header = Header(
      imageType: 2,
      width: tga.width.uint16,
      height: tga.height.uint16,
      bits: tga.bpp.uint8
    )
  var data = newSeq[uint8]()
  data.insert header.toBytes
  for idx, pixel in tga.pixels:
    # put bytes in order bgr(a)
    data &= pixel.blue
    data &= pixel.green
    data &= pixel.red
    if pixelSize == 4:
      data &= pixel.alpha
  filename.writeFile data
