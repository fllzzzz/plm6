import { ref } from 'vue'
import { ElNotification, ElLoading } from 'element-plus'

import { codeWait } from '@/utils'

// 标签打印
export default function useLabelPrint({ getPrintTotalNumber, getLabelInfo, getLoadingTextFunc, printLabelFunc, needAddPrintRecord = false, addPrintIdField = 'id', addPrintRecordReq, batchPrintSuccessHook, printFinallyHook }) {
  const printLoading = ref()
  async function batchPrint(rows) {
    try {
      await print(rows)
      if (typeof batchPrintHook === 'function') batchPrintSuccessHook()
    } catch (error) {
      console.log('批量打印错误', error)
    }
  }

  async function print(rows) {
    console.log(rows)
    printLoading.value = ElLoading.service({
      lock: true,
      text: '正在准备加入打印队列',
      spinner: 'el-icon-loading',
      fullscreen: true
    })
    try {
      for (const row of rows) {
        console.log(row)
        await printLabel(row)
      }
      printLoading.value.text = `已全部加入打印队列`
      await codeWait(500)
    } catch (error) {
      ElNotification({ title: '加入打印队列失败，请重试', type: 'error', duration: 2500 })
      throw new Error(error)
    } finally {
      printLoading.value.close()
      if (typeof printFinallyHook === 'function') printFinallyHook()
    }
  }

  async function printLabel(row) {
    console.log(getPrintTotalNumber(row), 'printTotalNumber')
    let pollingTimes = getPrintTotalNumber(row)// 打印总次数
    let printedTimes = 0 // 已打印次数
    const startTime = new Date().getTime()
    try {
      const labelInfo = await getLabelInfo(row)
      while (pollingTimes--) {
        printLoading.value.text = `正在加入打印队列：${getLoadingTextFunc(row)} 第${printedTimes + 1}张`
        await codeWait(500)
        await printLabelFunc(labelInfo)
        printedTimes++
      }
    } catch (error) {
      console.log('打印标签时发生错误', error)
      throw new Error(error)
    } finally {
      const endTime = new Date().getTime()
      if (needAddPrintRecord) {
        console.log(row, row[addPrintIdField], addPrintIdField)
        addPrintRecord(row, { id: row[addPrintIdField], quantity: printedTimes, startTime, endTime })
      }
    }
  }

  async function addPrintRecord(row, { id, quantity, startTime, endTime }) {
    if (!id || !quantity) return
    try {
      console.log('添加打印记录')
      await addPrintRecordReq({ id, quantity, startTime, endTime })
      row.printedQuantity += quantity
    } catch (error) {
      console.log('添加打印记录失败', error)
    }
  }

  return {
    batchPrint,
    print
  }
}
