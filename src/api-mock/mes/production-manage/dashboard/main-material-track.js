const getSummary = {
  url: '/api/mes/building/kanban/tracking/summary',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'plates': 0,
        'artifacts': 1750.3600
      }
    }
  }
}

const getCompare = {
  url: '/api/mes/building/kanban/tracking',
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
        'content': []
      }
    }
  }
}

const productionRecord = {
  url: '/api/mes/building/kanban/tracking/artifact/record',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'totalElements': 1,
        'content|1-12': [{
          'groupKey|+1': 1,
          'mete|1-1000.1-8': 1
        }]
      }
    }
  }
}

const productionDetail = {
  url: '/api/mes/building/kanban/tracking/artifact/record/detail',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'hasPreviousPage': false,
        'hasNextPage': false,
        'totalElements': 9,
        'content|1-20': [{
          'project': {
            'id': 1,
            'name': '浙江省国家大学科技园新建一期工程钢结构项目',
            'shortName': '大学科技园',
            'serialNumber': 'CMJK-21/12/31-1'
          },
          'monomer': {
            'id': 1,
            'name': '科研楼'
          },
          'name': '劲性柱',
          'serialNumber': 'GL-1',
          'specification': 'BH300*200*12*12',
          'length': 1750.00,
          'material': 'Q235B',
          'netWeight': 113.1000,
          'totalNetWeight': 113.1000,
          'grossWeight': 114.1200,
          'totalGrossWeight': 114.1200,
          'quantity': 1,
          'drawingNumber': null,
          'createTime': 1640932273000
        }]
      }
    }
  }
}

const outboundRecord = {
  url: '/api/mes/building/kanban/tracking/plate/record',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'totalElements': 1,
        'content|1-12': [{
          'groupKey|+1': 1,
          'mete|1-1000.1-8': 1
        }]
      }
    }
  }
}

export default [
  getSummary,
  getCompare,
  productionRecord,
  outboundRecord,
  productionDetail
]
