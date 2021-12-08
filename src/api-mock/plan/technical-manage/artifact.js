
const getArtifact = {
  url: '/api/plan/artifact/listByCondition',
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
          'id': 8,
          'assembleSerialNumber': '',
          'name': '焊接H钢柱',
          'serialNumber': 'GZ1-4',
          'specification': 'BH600*600*14*28',
          'length': 9770.00000000,
          'material': 'Q355B',
          'quantity': 1,
          'netWeight': 3968.98000000,
          'totalNetWeight': 3968.98000000,
          'grossWeight': 3968.98000000,
          'totalGrossWeight': 3968.98000000,
          'area': 12.00000000,
          'drawingNumber': null,
          'productQuantity': null,
          'remark': null,
          'boolStatusEnum': true
        }, {
          'id': 7,
          'assembleSerialNumber': '',
          'name': '焊接H钢柱',
          'serialNumber': 'GZ1-3',
          'specification': 'BH600*600*14*28',
          'length': 10919.00000000,
          'material': 'Q355B',
          'quantity': 1,
          'netWeight': 4135.82000000,
          'totalNetWeight': 4135.82000000,
          'grossWeight': 4135.82000000,
          'totalGrossWeight': 4135.82000000,
          'area': 12.00000000,
          'drawingNumber': null,
          'productQuantity': null,
          'remark': null,
          'boolStatusEnum': true
        }]
      }
    }
  }
}

export default [
  getArtifact
]