import { ElLoading, ElNotification } from 'element-plus'
import { codeWait } from '@/utils'
import { printMaterialLabel } from '@/utils/print/wms-material-label'
import { addPrintRecord } from '@/api/wms/material-label-print/index'
import { spliceMaterialSpec, spliceSteelSize } from '@/utils/wms/spec-format'

export default function usePrint() {
  // 打印loading
  let printLoading

  async function print(list = [], copies) {
    let matList
    openLoading()
    if (typeof list === 'function') {
      matList = await list()
    } else {
      matList = list
    }
    try {
      for (const row of matList) {
        await printLabel(row, copies)
      }
      printLoading.setText(`已全部加入打印队列`)
      await codeWait(500)
    } catch (error) {
      ElNotification({ title: '加入打印队列失败，请重试', type: 'error', duration: 2500 })
      throw new Error(error)
    } finally {
      printLoading && printLoading.close()
    }
  }

  async function printLabel(material, copies) {
    const pollingTimes = material.printNumber * copies // 打印总次数
    // const printedTimes = 0 // 已打印次数
    const startTime = new Date().getTime()
    try {
      // eslint-disable-next-line no-irregular-whitespace
      printLoading.setText(`正在加入打印队列：${material.classifyFullName} ${spliceSteelSize(material)}　${spliceMaterialSpec(material)}`)
      await printMaterialLabel({ material: material, copies: pollingTimes })
      const endTime = new Date().getTime()
      // 添加打印记录
      await addRecord({
        material: material,
        number: material.printNumber,
        copies: copies,
        startTime,
        endTime
      })
      return true
    } catch (error) {
      console.log('打印标签时发生错误', error)
      throw new Error(error)
    }
  }

  // 添加打印记录
  async function addRecord({ material, number, copies, startTime, endTime }) {
    const receiptMaterialId = material.id
    if (!receiptMaterialId || !number) return
    try {
      await addPrintRecord({ receiptMaterialId, number, copies, startTime, endTime })
      material.printedNumber += number
    } catch (error) {
      console.log('添加打印记录失败', error)
    }
  }

  // 打开loading
  function openLoading() {
    printLoading = ElLoading.service({
      lock: true,
      text: '正在准备加入打印队列',
      fullscreen: true,
      background: 'rgba(255, 255, 255, 0.5)'
    })
  }

  return {
    print
  }
}
