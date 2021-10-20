/**
 * 处理文件工具类
 */
import { parseTime } from '@/utils/date'

// 获取文件后缀名
export function getFileSuffix(fileName) {
  var first = fileName.lastIndexOf('.') // 取到文件名开始到最后一个点的长度
  var nameLength = fileName.length // 取到文件名长度
  var fileSuffix = fileName.substring(first + 1, nameLength) // 截取获得后缀名
  return fileSuffix
}

/**
 * 文件下载
 * @param {function} fn 文件下载接口
 */
export async function fileDownload(fn) {
  const context = this
  const args = Array.prototype.slice.call(arguments)
  args.shift()
  try {
    const response = await fn.apply(context, args)
    const result = downloadFileByResponse(response)
    if (result) {
      Notification({ title: '导出成功', type: 'success', duration: 2 * 1000 })
    } else {
      Notification({ title: '导出失败', type: 'error', duration: 2 * 1000 })
      throw Error('导出失败')
    }
  } catch (error) {
    throw Error(error)
  }
}

/**
 * 下载
 * @export
 * @param {*} response
 * @returns
 */
export function downloadFileByResponse(response) {
  const data = response.data
  const headers = response.headers
  if (!data || !headers) {
    return false
  }
  const type = headers['content-type']
  // application/json;charset=UTF-8：就是指“无类型”，一般的字节流用于数据传输，非文件下载
  if (type === 'application/json;charset=UTF-8') {
    // this.response为arraybuffer对象，转为uint8数组
    // const uint8 = new Uint8Array(res)
    // console.log(uint8)
    // // 解决使用fromCharCode后中文乱码的问题
    // const resToString = decodeURIComponent(escape((String.fromCharCode(...uint8))))
    // console.log(resToString)
    // const message = JSON.parse(resToString).message
    // console.log(message)
    return false
  }
  const blob = new Blob([data], { type: type }) // application/vnd.openxmlformats-officedocument.spreadsheetml.sheet这里表示xlsx类型
  const downloadElement = document.createElement('a')
  const href = window.URL.createObjectURL(blob) // 创建下载的链接
  downloadElement.href = href
  const _fullNameArr = headers && headers['content-disposition'] ? headers['content-disposition'].split('=')[1].split('.') : []
  if (!_fullNameArr || _fullNameArr.length === 0) {
    return false
  }
  const _suffix = `${decodeURI(_fullNameArr.pop())}` // 处理文件名乱码问题,后缀名
  if (!_suffix) {
    return false
  }
  // 获取文件名
  let _name = `${decodeURI(_fullNameArr.join('.'))}` // 处理文件名乱码问题

  _name = `${_name}_`
  const fileName = `${_name}${parseTime(new Date(), '{y}{m}{d}{h}{i}{s}')}.${_suffix}` // 处理文件名乱码问题
  downloadElement.download = fileName // 下载后文件名
  document.body.appendChild(downloadElement)
  downloadElement.click() // 点击下载
  document.body.removeChild(downloadElement) // 下载完成移除元素
  window.URL.revokeObjectURL(href) // 释放掉blob对象
  return true
}

// 下载文件
export function downloadFile(obj, name, suffix) {
  const url = window.URL.createObjectURL(new Blob([obj]))
  const link = document.createElement('a')
  link.style.display = 'none'
  link.href = url
  const fileName = parseTime(new Date()) + '-' + name + '.' + suffix
  link.setAttribute('download', fileName)
  document.body.appendChild(link)
  link.click()
  document.body.removeChild(link)
}
