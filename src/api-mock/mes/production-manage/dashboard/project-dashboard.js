const getSummaryList = {
  url: '/api/mes/building/kanban/form/formSummary',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data|1-10': [{
        'name': '劲性柱',
        'quantity': 2,
        'totalNetWeight': 226.2000,
        'totalLength': 3500.00,
        'completeQuantity': 2,
        'completeNetWeight': 226.2000,
        'completeLength': 3500.00,
        'cargoQuantity': 0,
        'cargoNetWeight': null,
        'cargoLength': 0.00
      }, {
        'name': '钢柱',
        'quantity': 4,
        'totalNetWeight': 1293.8400,
        'totalLength': 8128.00,
        'completeQuantity': 4,
        'completeNetWeight': 1293.8400,
        'completeLength': 8128.00,
        'cargoQuantity': 0,
        'cargoNetWeight': null,
        'cargoLength': 0.00
      }, {
        'name': '钢梁',
        'quantity': 3,
        'totalNetWeight': 230.3200,
        'totalLength': 3960.00,
        'completeQuantity': 3,
        'completeNetWeight': 230.3200,
        'completeLength': 3960.00,
        'cargoQuantity': 2,
        'cargoNetWeight': null,
        'cargoLength': 2810.00
      }]
    }
  }
}

const getShipList = {
  url: '/api/mes/building/cargo/report/ship',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': [{
        'date': '2021-12-31',
        'quantity': 1
      }]
    }
  }
}

const getQhseList = {
  url: '/api/mes/building/qhse/summary',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data|1-10': [{
        'quantity|1-10': 1,
        'qualityType': '@cword(2)',
        'qualityValue': '1'
      }]
    }
  }
}

export default [
  getSummaryList,
  getShipList,
  getQhseList
]
