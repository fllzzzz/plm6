/* eslint-disable no-irregular-whitespace */
import { printModeEnum as PrintMode } from './enum'
import { getLODOP, printByMode } from './base'

let LODOP

/**
 * 1---纵(正)向打印，固定纸张；
 * 2---横向打印，固定纸张；
 * 3---纵(正)向打印，宽度固定，高度按打印内容的高度自适应；
 * 0(或其它)----打印方向由操作者自行选择或按打印机缺省设置；
 */
const intOrient = 2
const pageWidth = 2100
const pageHeight = 2970

/**
 * PDF打印：打印由pdfjs解析过的pdf【通过获取canvas转base64图片】的图片打印
 */
export async function printPDFJSCanvas({ canvasBase64, copies = 1, printMode = PrintMode.QUEUE.V } = {}) {
  let result = false
  try {
    LODOP = await getLODOP()
    // 设置纸张类型，打印风格等
    LODOP.SET_PRINT_PAGESIZE(intOrient, pageWidth, pageHeight, '1')
    LODOP.SET_PRINT_STYLE('Stretch', 2)
    // 设置边距，传入待打印图片的base64编码
    LODOP.ADD_PRINT_IMAGE('0mm', '0mm', 'RightMargin:0mm', 'BottomMargin:0mm', canvasBase64)
    // LODOP.PRINT() /* 打印设计*/
    // LODOP.PREVIEW() /* 打印预览*/
    result = await printByMode(printMode)
  } catch (error) {
    throw new Error(error)
  }
  return result
}

/**
 * PDF打印 【需要顶级注册 暂不考虑使用】
 * intOrient
 * 1---纵(正)向打印，固定纸张；
 * 2---横向打印，固定纸张；
 * 3---纵(正)向打印，宽度固定，高度按打印内容的高度自适应；
 * 0(或其它)----打印方向由操作者自行选择或按打印机缺省设置；
 */
export async function printPDF({ pdfData, copies = 1, printMode = PrintMode.QUEUE.V } = {}) {
  let result = false
  try {
    LODOP = await getLODOP()
    // 设置打印纸张大小
    // var strURL = ''
    LODOP.PRINT_INIT('测试PDF打印功能')
    LODOP.SET_PRINT_PAGESIZE(intOrient, pageWidth, pageHeight, '1')
    LODOP.ADD_PRINT_PDF(0, 0, '100%', '100%', pdfData)
    LODOP.PRINT() /* 打印设计*/
    // LODOP.PREVIEW() /* 打印预览*/
    result = await printByMode(printMode)
  } catch (error) {
    throw new Error(error)
  }
  return result
}
