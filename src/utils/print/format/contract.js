import { toFixed } from '@/utils/data-type'
import { isNotBlank } from '@data-type/index'
import { dateDifference } from '@/utils/date'
import { convertUnits } from '@/utils/convert/unit'
import { expenseClassEnum } from '@enum-ms/contract'
import { DP } from '@/settings/config'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'

import { parseTime } from '@/utils/date'

import moment from 'moment'

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
    // 同台账：收付款比例
    row.collectionRate = toFixed((row.collectionRate || 0) * 100, 2)
    row.invoiceRate = toFixed((row.collectionRate || 0) * 100, 2)
    // 设备折旧：折旧率
    row.yearDepreciationRate = toFixed((row.yearDepreciationRate || 0) * 100, 2)
    row.monthDepreciationRate = toFixed((row.monthDepreciationRate || 0) * 100, 2)
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
    row.paymentRate = toFixed(amount ? (row.paymentAmount || 0) / amount * 100 : 0, 2)
    row.invoiceRate = toFixed(amount ? (row.invoiceAmount || 0) / amount * 100 : 0, 2)
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

// 处理时间范围
function handleTimeHorizon({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    let _startDate = moment(row.startDate).format('YYYY')
    let _endDate = moment(row.endDate).format('YYYY')
    if (_startDate !== header.year || _endDate !== header.year) {
      _startDate = moment(row.startDate).format('YYYY-MM-DD')
      _endDate = moment(row.endDate).format('YYYY-MM-DD')
    } else {
      _startDate = moment(row.startDate).format('MM-DD')
      _endDate = moment(row.endDate).format('MM-DD')
    }
    row.date = `${_startDate} ~ ${_endDate}`
    return row
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}

// 处理摊销记录
function handleAmortizationRecord({ header, table = [], footer, qrCode }) {
  header.name = expenseClassEnum.VL[header.expenseClassName]
  header.costRatio = toFixed(header.costAmount === 0 ? 0 : (header.sumAmount / header.costAmount) * 100, 2)
  const _table = table.map(row => {
    const _startDate = moment(row.startDate).format('YYYY-MM-DD')
    const _endDate = moment(row.endDate).format('YYYY-MM-DD')
    row.date = `${_startDate} ~ ${_endDate}`
    const _name = expenseClassEnum.VL[row.expenseClassEnum]
    if (_name !== row.name) {
      row.name = `${_name} > ${row.name}`
    }
    row.productMete = convertUnits(row.productMete, 'kg', 't', DP.CONTRACT_WT__T)
    row.expenseRatio = toFixed((row.amount / header.sumAmount) * 100, 2)
    row.costRatio = toFixed(header.costAmount === 0 ? 0 : (row.amount / header.costAmount) * 100, 2)
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
function handleExpenseRate({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    row.costAscriptionRate = toFixed((row.reimburseAmount / header.costAscriptionSumAmount) * 100, 2)
    row.expenseTypeRate = toFixed((row.reimburseAmount / header.sumAmount) * 100, 2)
    return row
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}

// 处理 费用录入/资产折旧 税率
function handleDepreciationRate({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    row.yearDepreciationRate = toFixed(row.yearDepreciationRate * 100, 2)
    row.monthDepreciationRate = toFixed(row.monthDepreciationRate * 100, 2)
    return row
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}

// 处理 业财报表/材料使用记录 不含税单价
async function handleUnitPrice({ header, table = [], footer, qrCode }) {
  await setSpecInfoToList(table)
  await numFmtByBasicClass(table)
  const _table = table.map(row => {
    if (row.amountExcludingVat) {
      row.unitPrice = toFixed(row.amountExcludingVat / row.mete, DP.YUAN)
    } else {
      row.unitPrice = undefined
      row.amountExcludingVat = undefined
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

// 处理 业财报表
async function handleFortuneReport({ header, table = [], footer, qrCode }) {
  await setSpecInfoToList(table)
  await numFmtByBasicClass(table)
  const _table = table.map(row => {
    if (row.grossProfitRate) {
      row.grossProfitRate = toFixed(row.grossProfitRate * 100, 2)
    }
    row.unpaidAmount = toFixed(row.contractAmount - row.collectionAmount, DP.YUAN)
    row.collectionRate = toFixed(row.collectionAmount / row.contractAmount * 100, 2)
    return row
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}

// 处理 重量
function handleLoadingWeight({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    row.loadingWeight = toFixed(row.loadingWeight / 1000, 2)
    return row
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}

// 处理 制成品重量
function handleActualWeight({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    row.actualWeight = toFixed(row.actualWeight / 1000, 2)
    return row
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}

// 商务录入处理退量
function handleContractAuxiliaryMaterial({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    if (row.boolReturn) {
      row.totalPrice = row.totalPrice * -1
      return row
    }
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}
// 处理税率为null
function invoiceRecord({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    row.taxRate = row.taxRate || 0
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

// 订单跟踪-入库统计-退量
function handleProjectWarehouseRecord({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    if (row.boolReturn) {
      row.totalPrice = row.totalPrice * -1
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

// 发运跟踪-退量
function handleContractAuxiliaryMaterialShipmentTracking({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    if (row.boolReturn) {
      row.totalPrice = row.totalPrice * -1
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

// 经典定制-原材料台账-加筛选时间

function handleSearchTime({ header, table = [], footer, qrCode }) {
  header.searchDate = header.startDate && header.endDate ? parseTime(header.startDate, '{y}-{m}-{d}') + '~' + parseTime(header.endDate, '{y}-{m}-{d}') : undefined
  return {
    header,
    table,
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
  handleTimeHorizon,
  handleAmortizationRecord,
  handleExpenseRate,
  handleDepreciationRate,
  handleUnitPrice,
  handleFortuneReport,
  handleLoadingWeight,
  handleActualWeight,
  handleContractAuxiliaryMaterial,
  handleProjectWarehouseRecord,
  handleContractAuxiliaryMaterialShipmentTracking,
  handleSearchTime,
  collectionLedger,
  invoiceRecord
}
