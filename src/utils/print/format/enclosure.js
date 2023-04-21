// 处理任务跟踪生产状态
function handleProductionStatus({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    row.booleanlag = row.booleanlag ? '正常' : '滞后'
    return row
  })
  return {
    header,
    table: _table,
    qrCode,
    footer
  }
}

// 处理生产总额
function handleProductionAmount({ header, table = [], footer, qrCode }) {
  const _table = table.map(row => {
    row.amount = (row.totalLength || row.reportTotalLength || 0) / 1000 * (row.price || 0)
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
  handleProductionStatus,
  handleProductionAmount
}
