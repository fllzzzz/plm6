import { isNotBlank } from '@data-type/index'
import { dateDifference } from '@/utils/date'

// 计算用时天数
function durationCalculation({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    // 计算天数
    row.allDays = ''
    row.alreadyDays = ''
    if (isNotBlank(row.startDate)) {
      // 工期
      if (isNotBlank(row.endDate)) {
        row.allDays = dateDifference(row.startDate, row.endDate)
      }
      // 用时天数（清单内所有任务全部入库，自动停止计时）
      const completeDate = row.completeDate || new Date().getTime()
      row.alreadyDays = dateDifference(row.startDate, completeDate)
    }
    return row
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}

// 处理合同台账收付款比例
function handleRate({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    row.collectionRate *= 100 || 0
    row.invoiceRate *= 100 || 0
    return row
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}

export default {
  handleRate,
  durationCalculation
}
