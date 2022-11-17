/* eslint-disable no-irregular-whitespace */
import { printModeEnum as PrintMode } from './enum'
import { getLODOP, printByMode } from './base'

let LODOP
/**
 * PDF打印
 * intOrient
 * 1---纵(正)向打印，固定纸张；
 * 2---横向打印，固定纸张；
 * 3---纵(正)向打印，宽度固定，高度按打印内容的高度自适应；
 * 0(或其它)----打印方向由操作者自行选择或按打印机缺省设置；
 */
export async function printPDF({ pdfData, intOrient = 1, pageWidth = 2100, pageHeight = 2970, copies = 1, printMode = PrintMode.QUEUE.V } = {}) {
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
