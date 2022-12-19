/**
 * 处理文件工具类
 */
import { parseTime } from '@/utils/date'
import XLSX from 'xlsx-styleable'
import { ElMessage, ElNotification } from 'element-plus'
import { isNotBlank, toPrecision } from './data-type'
import { trimStr } from './data-type/string'
import { patternDP } from './validate/pattern'

// 获取文件后缀名
export function getFileSuffix(fileName) {
  if (!isNotBlank(fileName)) return ''
  var first = fileName.lastIndexOf('.') // 取到文件名开始到最后一个点的长度
  var nameLength = fileName.length // 取到文件名长度
  if (first !== -1) {
    var fileSuffix = fileName.substring(first + 1, nameLength) // 截取获得后缀名
    return fileSuffix
  } else {
    return ''
  }
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
      ElNotification({ title: '导出成功', type: 'success', duration: 2 * 1000 })
    } else {
      ElNotification({ title: '导出失败', type: 'error', duration: 2 * 1000 })
      throw Error('导出失败')
    }
  } catch (error) {
    throw Error(error)
  }
}

/**
 * 文件下载2
 * @param {function} fn 文件下载接口,根据状态
 */
export async function fileDownloadByReturnStatus(fn) {
  const context = this
  const args = Array.prototype.slice.call(arguments)
  args.shift()
  try {
    const response = await fn.apply(context, args)
    resolveDownload(response)
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
export function downloadFileByResponse(response, fileName) {
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

  if (!fileName) {
    fileName = getFileName(response)
  }
  const _nameArr = fileName.split('.')
  fileName = `${_nameArr[_nameArr.length - 2]}_${parseTime(new Date(), '{y}{m}{d}{h}{i}{s}')}.${_nameArr[_nameArr.length - 1]}` // 处理文件名乱码问题
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

/**
 * 文件校验
 * @param {*} file 文件
 * @param {*} param 校验参数
 * @returns
 */
export function fileVerification(file, { accept, sizeLimit }) {
  if (accept) {
    const typeFlag = accept.split(',').indexOf(`.${getFileSuffix(file.name).toLowerCase()}`) > -1
    if (!typeFlag) {
      ElMessage.error(`上传文件后缀需为${accept}格式`, 2000)
      return false
    }
  }
  const sizeM = file.size / 1024 / 1024
  const isLimit = !sizeLimit || (sizeLimit && sizeM < sizeLimit)
  if (!isLimit) {
    ElMessage.error(`上传文件大小大小不能超过 ${sizeLimit}MB!`, 2000)
    return false
  }
  return true
}

/**
 * 解析excel
 * @param {*} file 文件
 * @returns
 */
export function resolveExcel(file) {
  return new Promise((resolve, reject) => {
    var rABS = false // 是否将文件读取为二进制字符串
    var f = file
    var reader = new FileReader()
    // if (!FileReader.prototype.readAsBinaryString) {
    FileReader.prototype.readAsBinaryString = function (f) {
      var binary = ''
      var rABS = false // 是否将文件读取为二进制字符串
      // eslint-disable-next-line no-unused-vars
      // var pt = this
      var wb // 读取完成的数据
      var outData
      var reader = new FileReader()
      reader.onload = (e) => {
        var bytes = new Uint8Array(reader.result)
        var length = bytes.byteLength
        for (var i = 0; i < length; i++) {
          binary += String.fromCharCode(bytes[i])
        }
        if (rABS) {
          // eslint-disable-next-line no-undef
          wb = XLSX.read(btoa(fixdata(binary)), {
            // 手动转化
            type: 'base64'
          })
        } else {
          wb = XLSX.read(binary, {
            type: 'binary'
          })
        }
        outData = XLSX.utils.sheet_to_json(wb.Sheets[wb.SheetNames[0]]) // outData就是结果
        resolve(outData)
        // return arr
      }
      reader.readAsArrayBuffer(f)
    }

    if (rABS) {
      reader.readAsArrayBuffer(f)
    } else {
      reader.readAsBinaryString(f)
    }
  })
}

// excel数据格式转换
export async function formatExcelData(data, template = {}) {
  const res = []
  const startRow = template.startRow || 0
  const endIgnoreRow = template.endIgnoreRow || 0
  const fields = template.fields
  if (data.length >= startRow && fields) {
    const _data = data.slice(startRow - 1, data.length - endIgnoreRow)
    // 遍历数据
    _data.forEach((item) => {
      const obj = {}
      // 遍历表格字段
      fields.forEach((f) => {
        obj[f.field] = trimStr(item[f.excelField])
        // 是否为数值类型
        if (f.type === 'number') {
          obj[f.field] = Number(obj[f.field])
          // 设置了小数精度
          if (patternDP.test(f.precision)) {
            obj[f.field] = toPrecision(obj[f.field], f.precision)
          }
        } else if (typeof obj[f.field] === 'number') {
          // 非数值类型的 表格解析为数值的转为字符串
          obj[f.field] = String(obj[f.field])
        }
      })
      res.push(obj)
    })
  }
  if (typeof template.format === 'function') {
    return await template.format(res)
  }
  return res
}

/**
 * 获取文件名
 * @param {*} response
 * @returns
 */
export function getFileName(response) {
  const headers = response.headers
  const _fullPathName = headers && headers['content-disposition'] ? headers['content-disposition'].split('=')[1].split('.') : []
  if (!_fullPathName || _fullPathName.length === 0) {
    return false
  }
  const _suffix = `${decodeURIComponent(_fullPathName.pop())}` // 处理文件名乱码问题,后缀名
  if (!_suffix) {
    return false
  }
  // 获取文件名
  const _name = `${decodeURIComponent(_fullPathName.join('.'))}` // 处理文件名乱码问题
  const fileName = `${_name}.${_suffix}` // 处理文件名乱码问题
  return fileName
}

/**
 * 下载解析 code=20000;message=xxxxx
 * @param {*} res
 */
export function resolveDownload(res, { fileName, successMsg, errorMsg } = {}) {
  const result = res.headers.result && decodeURIComponent(res.headers.result).split(';')
  console.log('res', res)
  if (result) {
    // code 20000上传成功，50000上传失败（返回Excel模板或错误提示）
    const code = result[0].split('=')[1]
    const message = result[1].split('=')[1]
    const currentFileName = getFileName(res) || ''
    if (code === '20000') {
      ElNotification.success({ title: `${fileName || currentFileName}下载请求成功, 正在下载`, message: successMsg })
      downloadFileByResponse(res, currentFileName)
    } else {
      ElNotification.error({ title: `${fileName || currentFileName}下载请求失败`, message: errorMsg || `${message}` })
    }
  } else {
    ElNotification.error(`下载请求失败`)
  }
}
