// 调整记录
const supplementRecord = {
  url: '/api/wms/report/raw-materials/supplement/details',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'hasPreviousPage': false,
        'hasNextPage': false,
        'totalElements': 1,
        'content': [
          {
            'id': 1,
            'basicClass': 1,
            'classifyId': 1,
            'specification': 'Q355C',
            'length': 10000,
            'width': 100000,
            'thickness': 1000,
            'color': null,
            'accountingUnit': '千克',
            'mete': 5000,
            'amount': -7850000,
            'brand': '大品牌',
            'amountExcludingVAT': -7136363.64,
            'serialNumber': 'HC-220519-1',
            'receipt': {
              'id': 1,
              'serialNumber': 'TH-220519-1',
              'receiptType': null
            },
            'workshop': {
              'id': 7,
              'name': '杭州车间'
            },
            project: {
              'id|+1': 1,
              'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
              'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
              serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
            },
            'createTime': 1652952797421
          }
        ],
        'totalAmount': null
      }
    }
  }
}

// 获取调整单详情
const getDetails = {
  url: RegExp('/api/wms/report/raw-materials/supplement/' + '\\d'),
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'id': 1,
        'basicClass': 1,
        'classifyId': 1,
        'specification': 'Q355C',
        'length': 10000,
        'width': 100000,
        'thickness': 1000,
        'color': null,
        'accountingUnit': '千克',
        'mete': 5000,
        'amount': -7850000,
        'brand': '大品牌',
        'amountExcludingVAT': -7136363.64,
        'serialNumber': 'HC-220519-1',
        'receipt': {
          'id': 1,
          'serialNumber': 'TH-220519-1',
          'receiptType': null
        },
        'workshop': {
          'id': 7,
          'name': '杭州车间'
        },
        project: {
          'id|+1': 1,
          'name|+1': ['长安街666666号辅路', '你脸红个泡泡茶壶666号主路'],
          'shortName|+1': ['长安街', '你脸红个泡泡茶壶'],
          serialNumber: /([A-Z0-9]{2,3}\-){1,3}[A-Z0-9]{2,3}/
        },
        'createTime': 1652952797421
      }
    }
  }
}

export default [supplementRecord, getDetails]
