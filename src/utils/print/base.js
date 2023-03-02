import { getLodop } from './lodop-funcs'
import { ElMessageBox } from 'element-plus'
import { printModeEnum as PrintMode } from './enum'
import { codeWait } from '@/utils'

let LODOP
let printEnd = false
let currentPollingNum = 0
let taskOKArr = []
let taskExistArr = []
let lastTaskExistArr = taskExistArr
let lastTaskOKArr = taskOKArr
let hasCleanTask = false
let hasOKReturn = true
let hasExistReturn = true

let intPos = '' // 打印机序号
// const pollingTime = 500 // 轮序时间 / 暂时废除
const whileMaxFrequency = 500 // while 次数上限
const maxPollingNum = 30 // 最大轮询次数
const taskTimeoutTime = 10000 // timeout 时间

let getTaskTimeout = null

export function getLODOP() {
  return new Promise((resolve, reject) => {
    try {
      LODOP = getLodop(document.getElementById('LODOP_OB'), document.getElementById('LODOP_EM'))
      LODOP.PRINT_INIT('') /* 初始化*/
      LODOP.SET_PRINT_MODE('RESELECT_COPIES', true)
      parmaInit()
      resolve(LODOP)
    } catch (e) {
      download()
      reject(e)
    }
  })
}

// 参数初始化
function parmaInit() {
  printEnd = false
  currentPollingNum = 0
  lastTaskOKArr = taskOKArr
  lastTaskExistArr = taskExistArr
  taskOKArr = []
  taskExistArr = []
  hasCleanTask = false
  hasOKReturn = true
  hasExistReturn = true
}

// 打印模式
// TODO: 预览模式需要优化，有多个文件时，应关闭预览窗口时才结束
export async function printByMode(mode) {
  let _result = false
  switch (mode) {
    case PrintMode.NORMAL.V:
      _result = print()
      break
    case PrintMode.QUEUE.V:
      _result = await printByInQueue()
      break
    case PrintMode.RESULT.V:
      _result = await printByHasResult()
      break
    case PrintMode.PREVIEW.V:
      _result = preview()
      break
    case PrintMode.PRINT_DESIGN.V:
      _result = printDesign()
      break
    default:
      _result = print()
      break
  }
  return _result
}

// 直接打印
function print() {
  LODOP.PRINT()
  return true
}

// 直接预览
function preview() {
  // 允许重新选择纸张方向
  LODOP.SET_PRINT_MODE('RESELECT_ORIENT', true)
  LODOP.PREVIEW()
  return true
}

function printDesign() {
  LODOP.PRINT_DESIGN()
  return true
}

// 打印并判断是否进入打印队列
async function printByInQueue() {
  return new Promise((resolve, reject) => {
    let _returnValue
    if (LODOP.CVERSION) {
      LODOP.On_Return = async (TaskID, Value) => {
        console.log('TaskID', TaskID, Value)
        _returnValue = Value
        resolve(_returnValue)
      }
      LODOP.PRINT()
    } else {
      _returnValue = LODOP.PRINT()
      resolve(_returnValue)
    }
  })
}

// 打印并判断是否打印成功（暂时用 是否成功 || 是否出打印队列 来判断）
async function printByHasResult() {
  const _returnValue = { value: '' }
  LODOP.SET_PRINT_MODE('CATCH_PRINT_STATUS', true)
  if (LODOP.CVERSION) {
    let loopNum = 0
    LODOP.On_Return = async (TaskID, Value) => {
      loopNum++
      _returnValue.value = Value
      console.log('第一次ON_RETURN C-LODOP: 获取JOB代码', TaskID, Value)
      if (Value.toString().indexOf('该打印机在“云服务器工作模式”下被禁用') > -1) {
        setTimeout(() => {
          alert('请启动LODOP或C-LODOP')
        }, 500)
        return false
      }
      // 某种出错的情况下停止打印
      if (loopNum > 0) {
        console.log('getTaskTimeout-------------', getTaskTimeout)
        if (getTaskTimeout) clearTimeout(getTaskTimeout)
        getTaskTimeout = setTimeout(async () => {
          clearTimeout(getTaskTimeout)
          console.log('无法获取到正确的JOB代码,并清空队列', intPos)
          await controlPrinterPURGE(intPos)
          return false
        }, taskTimeoutTime)
      }
      if (lastTaskOKArr.indexOf(TaskID) === -1 && lastTaskExistArr.indexOf(TaskID) === -1) {
        // 清除task-timeout
        if (getTaskTimeout) clearTimeout(getTaskTimeout)
        // 设置打印机序号
        intPos = GetPrinterIDfromJOBID(Value)
        const _result = await pollingQueryRes(_returnValue)
        return _result
      }
    }
    let hasExec = false
    console.log('-----------------PRINT------------------clodop')
    let whileFrequency = 0
    do {
      if (whileFrequency++ > whileMaxFrequency) {
        throw new Error('PRINT-while超时')
      }
      if (LODOP.blOneByone === false) {
        LODOP.PRINT()
        hasExec = true
      } else {
        console.log('窗体存在--------------------延迟500:PRINT', LODOP.blOneByone)
        await codeWait(500)
      }
    } while (LODOP.blOneByone === true && hasExec)
    console.log('-----------------PRINT—END------------------clodop')
  } else {
    _returnValue.value = LODOP.PRINT()
    console.log('第一次ON_RETURN LODOP: 获取JOB代码', _returnValue.value)
    const _result = await pollingQueryRes(_returnValue)
    return _result
  }
}

// 轮序打印结果
async function pollingQueryRes(returnValue) {
  console.log('轮序接收到的JOB代码', returnValue)
  for (let i = 0; i < maxPollingNum; i++) {
    console.log('轮询------------------------------------------', i, LODOP.blOneByone)
    // if(LODOP.blOneByone === false) {
    if (printEnd) {
      return true
    }
    // 是否打印成功
    await getStatusValue('PRINT_STATUS_OK', returnValue.value)
    let _whileFrequency = 0
    // eslint-disable-next-line no-unmodified-loop-condition
    while (!hasOKReturn) {
      if (_whileFrequency++ > whileMaxFrequency) {
        await controlPrinterPURGE(returnValue.value)
        throw new Error('等待回调-OK-while超时')
      }
      console.log(`------------------等待回调-OK------------------${hasOKReturn},${hasExistReturn}`)
      await codeWait(500)
    }
    // 是否在队列
    await getStatusValue('PRINT_STATUS_EXIST', returnValue.value)
    _whileFrequency = 0
    // eslint-disable-next-line no-unmodified-loop-condition
    while (!hasExistReturn) {
      if (_whileFrequency++ > whileMaxFrequency) {
        await controlPrinterPURGE(returnValue.value)
        throw new Error('等待回调-EXIST-while超时')
      }
      console.log(`------------------等待回调-EXIST------------------${hasOKReturn},${hasExistReturn}`)
      await codeWait(500)
    }
    // 打印结果状态码
    // getStatusValue('PRINT_STATUS_ID', returnValue.value)
    // await codeWait(pollingTime)
    // }
  }
  await controlPrinterPURGE(returnValue.value)
  return false
}

// 查询打印状态对应的值
async function getStatusValue(ValueType, ValueIndex) {
  let _strResult = null
  if (LODOP.CVERSION) {
    let hasExec = false
    LODOP.On_Return_Remain = true
    LODOP.On_Return = (TaskID, Value) => {
      // if(printEnd) return
      console.log(`任务ID：${TaskID}     任务完成ID：${taskOKArr}     任务队列ID：${taskExistArr}     结果：${Value}`)
      if (taskOKArr.indexOf(TaskID) > -1) {
        hasOKReturn = true
        // 为1时打印成功（当前值可能会不准确，所以下面增加是否在队列中的判断）
        if (+Value === 1) {
          console.log('------------------打印成功------------------')
          printEnd = true
        }
      } else if (taskExistArr.indexOf(TaskID) > -1) {
        hasExistReturn = true
        if (+Value === 0) {
          console.log('------------------已出队列------------------')
          printEnd = true
        }
      } else {
        hasOKReturn = true
        hasExistReturn = true
        console.log('-----------------不匹配情况-----------------', Value)
      }
    }
    console.log(`------------------GET_VALUE------------------${ValueType}`)
    let whileFrequency = 0
    do {
      if (whileFrequency++ > whileMaxFrequency) {
        await controlPrinterPURGE(ValueIndex)
        throw new Error('GET_VALUE-while超时')
      }
      if (LODOP.blOneByone === false) {
        _strResult = LODOP.GET_VALUE(ValueType, ValueIndex)
        hasExec = true
      } else {
        console.log('窗体存在--------------------延迟500:GET_VALUE', LODOP.blOneByone)
        await codeWait(500)
      }
    } while (LODOP.blOneByone === true && hasExec)
    console.log(`------------------GET_VALUE-END------------------`)
    if (ValueType === 'PRINT_STATUS_OK') {
      hasOKReturn = false
      taskOKArr.push(_strResult)
    }
    if (ValueType === 'PRINT_STATUS_EXIST') {
      hasExistReturn = false
      taskExistArr.push(_strResult)
    }
  }

  // LODOP 的情况
  if (!LODOP.CVERSION) {
    console.log('------------------GET_VALUE------------------LODOP')
    _strResult = LODOP.GET_VALUE(ValueType, ValueIndex)
    if (ValueType === 'PRINT_STATUS_OK') {
      hasOKReturn = false
      console.log('OK-------------------', _strResult)
      if (_strResult) {
        printEnd = true
        hasOKReturn = true
      }
    }
    if (ValueType === 'PRINT_STATUS_EXIST') {
      hasExistReturn = false
      console.log('EXIST-------------------', _strResult)
      if (!_strResult && currentPollingNum > 0) {
        printEnd = true
        hasExistReturn = true
      }
    }
  }
}

// 清理打印任务
async function controlPrinterPURGE(strJOBID) {
  if (hasCleanTask) {
    return
  }
  hasCleanTask = true
  console.log('清除任务')
  const strPrinterID = GetPrinterIDfromJOBID(strJOBID)
  if (LODOP.CVERSION) {
    LODOP.On_Return = (TaskID, Value) => {
      console.log('清理结果:' + Value)
      if (Value.toLowerCase() === 'ok') {
        return
      }
    }
    let _hasExec = false
    console.log('-----------------SET_PRINT_MODE------------------clodop')
    let _whileFrequency = 0
    do {
      if (_whileFrequency++ > whileMaxFrequency) {
        throw new Error('SET_PRINT_MODE-while超时')
      }
      if (LODOP.blOneByone === false) {
        LODOP.SET_PRINT_MODE('CONTROL_PRINTER:' + strPrinterID, 'PURGE')
        _hasExec = true
      } else {
        console.log('窗体存在--------------------延迟500:SET_PRINT_MODE-清除', LODOP.blOneByone)
        await codeWait(500)
      }
    } while (LODOP.blOneByone === true && _hasExec)
    return
  } else {
    var strResult = LODOP.SET_PRINT_MODE('CONTROL_PRINTER:' + strPrinterID, 'PURGE')
    console.log('清理结果:' + strResult)
  }
}

// 从JOB代码找出打印机序号：
function GetPrinterIDfromJOBID(strJOBID) {
  var intPos = strJOBID.indexOf('_')
  if (intPos < 0) {
    return strJOBID
  } else {
    return strJOBID.substr(0, intPos)
  }
}

// eslint-disable-next-line no-unused-vars
function getStatusMessage(statusID) {
  var messages = ''
  if (statusID & 1) messages += '已暂停 -'
  if (statusID & 2) messages += '错误 -'
  if (statusID & 4) messages += '正删除 -'
  if (statusID & 8) messages += '进入队列 -'
  if (statusID & 16) messages += '正在打印 -'
  if (statusID & 32) messages += '脱机 -'
  if (statusID & 64) messages += '缺纸 -'
  if (statusID & 128) messages += '打印结束 -'
  if (statusID & 256) messages += '已删除 -'
  if (statusID & 512) messages += '堵了 -'
  if (statusID & 1024) messages += '用户介入 -'
  if (statusID & 2048) messages += '正在重新启动 -'
  return messages
}

function download() {
  ElMessageBox.confirm('请下载Web打印控件\n下载后刷新页面重试\n地址：http://www.LODOP.net/index.html', `请下载打印控件`, {
    showCancelButton: false,
    confirmButtonText: '确认',
    type: 'warning'
  })
  // try {
  //   const elem = document.createElement('iframe')
  //   elem.src = '../../../../../assets/printtool.rar'
  //   elem.style.display = 'none'
  //   document.body.appendChild(elem)
  // } catch (e) {
  //   console.log('下载异常')
  // }
}

export function combineHtml(style, body) {
  let html = ''
  html = `<!DOCTYPE html>
          <html>
          <head>
            ${style}
          </head>
          <body>
            ${body}
          </body>
        </html>`
  return html
}

export function getPageSize(w, h) {
  let _pageSize = ''
  if (w === 210 && h === 297) _pageSize = 'A4'
  return _pageSize
}

export default {
  getLODOP,
  printByMode,
  combineHtml
}
