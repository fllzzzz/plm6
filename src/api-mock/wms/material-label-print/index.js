// 添加打印记录
const addPrintRecord = {
  url: '/api/wms/material/label-print/print-record',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 获取未打印的数量
const getNotPrintedMaterialNumber = {
  url: '/api/wms/material/label-print/not-printed-number',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'totalMaterial|10-20': 20,
        'inboundMaterial|1-10': 10,
        'outboundMaterial|1-3': 0,
        'transferMaterial|1-5': 5,
        'returnMaterial|1-2': 2
      }
    }
  }
}

export default [addPrintRecord, getNotPrintedMaterialNumber]
