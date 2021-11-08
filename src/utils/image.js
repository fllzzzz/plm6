export function imgLoaded(url) {
  if (!url) {
    return undefined
  }
  return new Promise(function (resolve, reject) {
    if (url.slice(0, 4) === 'data') { // base64
      const img = new Image()
      img.onload = function () {
        resolve(img)
      }
      img.onerror = function () {
        reject('Image load error')
      }
      img.src = url
      return
    }
    const img = new Image()
    img.setAttribute('crossOrigin', 'Anonymous')
    img.onload = function () {
      resolve(img)
    }
    img.onerror = function () {
      resolve()
    }
    img.src = url
  })
}

const BASE64_MARKER = ';base64,'

// base64 to binary(Uint8Array)
export function base64ToUint8Array(dataURI) {
  var base64Index = dataURI.indexOf(BASE64_MARKER) + BASE64_MARKER.length
  var base64 = dataURI.substring(base64Index)
  var raw = window.atob(base64)
  var rawLength = raw.length
  var array = new Uint8Array(new ArrayBuffer(rawLength))

  for (let i = 0; i < rawLength; i++) {
    array[i] = raw.charCodeAt(i)
  }
  return array
}

// image to binary(Uint8Array)
export function img2Uint8Array(img) {
  const base64 = img2base64(img)
  return base64ToUint8Array(base64)
}

// get base64 format of the image
export function img2base64(img) {
  var canvas = document.createElement('canvas')
  canvas.width = img.width
  canvas.height = img.height
  var ctx = canvas.getContext('2d')
  ctx.drawImage(img, 0, 0, img.width, img.height)
  var ext = img.src.substring(img.src.lastIndexOf('.') + 1).toLowerCase()
  var dataURL = canvas.toDataURL('image/' + ext)
  // return dataURL.replace(/^data:image\/(png|jpg);base64,/, '')
  return dataURL
}
