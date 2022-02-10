const get = {
  url: '/api/mes/building/analysis/hysteresis',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'artifactAssembleList|1-10': [{
          'dateTime': '@datetime',
          'askCompleteTime': '@datetime',
          'taskQuantity|1-1000': 50,
          'completeQuantity|1-1000': 0,
          'taskNetWeight|1-1000.1-8': 206791.00000000,
          'taskGrossWeight|1-1000.1-8': 1000000.00000000,
          'completeNetWeight|1-1000.1-8': 0,
          'completeGrossWeight|1-1000.1-8': 0
        }],
        'enclosureList': [{
          'dateTime': '@datetime',
          'askCompleteTime': '@datetime',
          'taskQuantity|1-1000': 50,
          'completeQuantity|1-1000': 0,
          'taskArea|1-1000.1-8': 206791.00000000,
          'taskLength|1-1000.1-8': 1000000.00000000,
          'completeArea|1-1000.1-8': 0,
          'completeLength|1-1000.1-8': 0
        }]
      }
    }
  }
}

const getDetail = {
  url: '/api/mes/building/analysis/production_summary/details',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'artifactAssembleList': [{
          'askCompleteTime': 1638460800000,
          'taskQuantity': 50,
          'completeQuantity': 0,
          'taskNetWeight': 206791.00000000,
          'taskGrossWeight': 1000000.00000000,
          'completeNetWeight': 0E-8,
          'completeGrossWeight': 0E-8,
          'project': {
            'id': 38,
            'name': 'hgg',
            'contractNo': 'hggggggg',
            'shortName': 'hghghgh',
            'type': null
          },
          'monomer': {
            'id': 3,
            'name': '单体#1'
          },
          'areaDetail': {
            'id': 3,
            'name': '区域'
          },
          'factory': {
            'id': 1,
            'name': 'dzx一号工厂'
          },
          'workshop': {
            'id': 1,
            'name': 'dzx一号车间'
          },
          'productionLine': {
            'id': 1,
            'name': 'dzx一号生产线'
          },
          'name': '焊接H钢柱',
          'serialNumber': 'GZ1-3',
          'specification': 'BH600*600*14*28',
          'material': 'Q355B'
        }],
        'enclosureList': [{
          'areaDetail': {
            'id': 3,
            'name': '区域'
          },
          'askCompleteTime': '2021-12-08T00:00:00',
          'color': '',
          'completeArea': 0,
          'completeLength': 0,
          'completeQuantity': 0,
          'factory': {
            'id': 1,
            'name': 'dzx一号工厂'
          },
          'monomer': {
            'id': 3,
            'name': '单体#1'
          },
          'name': '桁架楼承板',
          'plate': 'TD-75-1',
          'productionLine': {
            'id': 1,
            'name': 'dzx一号生产线'
          },
          'project': {
            'contractNo': 'hggggggg',
            'id': 38,
            'name': 'hgg',
            'shortName': 'hghghgh',
            'type': null
          },
          'serialNumber': '',
          'taskArea': 34356000,
          'taskLength': 49080,
          'taskQuantity': 4,
          'workshop': {
            'id': 1,
            'name': 'dzx一号车间'
          }
        }]
      }
    }
  }
}

export default [
  get,
  getDetail
]
