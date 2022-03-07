
const getMachinePart = {
  url: '/api/plan/machinePart/listByCondition',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'hasPreviousPage': false,
        'hasNextPage': false,
        'totalElements': 2,
        'content': [{
          'id': 4,
          'name': null,
          'serialNumber': 'A11',
          'specification': 'PL10*509',
          'length': 728.00000000,
          'material': 'Q355B',
          'quantity': 1,
          'netWeight': 28.93000000,
          'totalNetWeight': 28.93000000,
          'grossWeight': 28.93000000,
          'totalGrossWeight': 28.93000000,
          'area': null,
          'drawingNumber': null,
          'type': 1,
          'remark': null,
          'totalProductionQuantity': null,
          'usedQuantity': 0
        }, {
          'id': 3,
          'name': null,
          'serialNumber': 'A10',
          'specification': 'PL18*728',
          'length': 544.00000000,
          'material': 'Q355B',
          'quantity': 1,
          'netWeight': 48.11000000,
          'totalNetWeight': 48.11000000,
          'grossWeight': 48.11000000,
          'totalGrossWeight': 48.11000000,
          'area': null,
          'drawingNumber': null,
          'type': 1,
          'remark': null,
          'totalProductionQuantity': null,
          'usedQuantity': 0
        }]
      }
    }
  }
}

export default [
  getMachinePart
]
