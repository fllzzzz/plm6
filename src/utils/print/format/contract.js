import { isNotBlank } from '@data-type/index'
import { dateDifference } from '@/utils/date'
import { convertUnits } from '@/utils/convert/unit'
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
    row.collectionRate *= 100 || 0
    row.invoiceRate *= 100 || 0
    // 设备折旧：折旧率
    row.yearDepreciationRate *= 100 || 0
    row.monthDepreciationRate *= 100 || 0
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

export default {
  handleRate,
  handleAreaUnit,
  handleSupplierPaymentRate,
  durationCalculation,
  handleSupplierPaymentOrder,
  handleTimeHorizon
}
