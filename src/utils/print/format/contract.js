import { isNotBlank } from '@data-type/index'
import { dateDifference } from '@/utils/date'
import { convertUnits } from '@/utils/convert/unit'
// import { supplierPayTypeEnum } from '@enum-ms/contract'

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

// 处理供应商付款税率
function handleSupplierPaymentRate({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    const amount = row.amount || row.freight || 0
    row.paymentRate = amount ? (row.paymentAmount || 0) / amount * 100 : 0
    row.invoiceRate = amount ? (row.invoiceAmount || 0) / amount * 100 : 0
    return row
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}

// 处理供应商付款订单名称
function handleSupplierPaymentOrder({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    if (row.serialNumber) {
      row.orderName = row.serialNumber
    } else {
      row.projectNameList = row.projectNameList || []
      row.serialNumberList = row.serialNumberList || []
      row.orderName = [...row.projectNameList, ...row.serialNumberList].join('、')
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

// 处理面积单位
function handleAreaUnit({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    row.totalArea = convertUnits(row.totalArea, 'mm2', 'm2')
    return row
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}

function collectionLedger({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    if (row.type === 2) {
      row.projectOrName = row.paymentUnit
      row.seller = row.collectionUnit
      row.businessType = 9
    } else {
      row.projectOrName = row.project.name
      row.seller = row.collectionUnit
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

export default {
  handleRate,
  handleAreaUnit,
  handleSupplierPaymentRate,
  durationCalculation,
  handleSupplierPaymentOrder,
  collectionLedger
}
