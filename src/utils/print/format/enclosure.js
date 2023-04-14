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

export default {
  handleProductionStatus
}
