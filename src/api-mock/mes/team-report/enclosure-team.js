
const getEnclosure = {
  url: '/api/mes/building/team_form/enclosure',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': [{
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
        'productType': 4,
        'taskQuantity': 34,
        'taskLength': 417180.00000000,
        'taskArea': 292026000.0000000000000000,
        'completeQuantity': 20,
        'completeLength': 245400.00000000,
        'completekArea': null
      }]
    }
  }
}

const getEnclosureDetail = {
  url: '/api/mes/building/team_form/enclosure/details',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': [{
        'project': {
          'id': null,
          'name': 'hgg',
          'contractNo': 'hggggggg',
          'shortName': 'hghghgh',
          'type': null
        },
        'monomer': {
          'id': null,
          'name': '单体#1'
        },
        'areaDetail': {
          'id': null,
          'name': '区域'
        },
        'name': '桁架楼承板',
        'serialNumber': 'B1',
        'plate': 'TD-75-1',
        'color': null,
        'material': null,
        'width': 700.00000000,
        'length': 12270.00000000,
        'category': 8,
        'thickness': null,
        'taskQuantity': 10,
        'taskLength': 122700.00000000,
        'taskArea': 85890000.0000000000000000,
        'completeQuantity': 4,
        'completeLength': 49080.00000000,
        'completeArea': 34356000.0000000000000000,
        'completeTime': 1639324800000
      }, {
        'project': {
          'id': null,
          'name': 'hgg',
          'contractNo': 'hggggggg',
          'shortName': 'hghghgh',
          'type': null
        },
        'monomer': {
          'id': null,
          'name': '单体#1'
        },
        'areaDetail': {
          'id': null,
          'name': '区域'
        },
        'name': '桁架楼承板',
        'serialNumber': 'B1',
        'plate': 'TD-75-1',
        'color': null,
        'material': null,
        'width': 700.00000000,
        'length': 12270.00000000,
        'category': 8,
        'thickness': null,
        'taskQuantity': 10,
        'taskLength': 122700.00000000,
        'taskArea': 85890000.0000000000000000,
        'completeQuantity': 5,
        'completeLength': 61350.00000000,
        'completeArea': 42945000.0000000000000000,
        'completeTime': 1639411200000
      }]
    }
  }
}

export default [
  getEnclosure,
  getEnclosureDetail
]
