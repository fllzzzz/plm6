import {
  artifactListInfo
} from '../../common-mock-data/product-type-data'

const getList = {
  url: '/api/mes/building/kanban/assemble_matching/page',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'hasNextPage': false,
        'hasPreviousPage': false,
        'totalElements': 1,
        'content|1-100': [{
          ...artifactListInfo,
          'type|1': [1, 2, 4]
        }]

      }
    }
  }
}

const getDetail = {
  url: '/api/mes/building/kanban/assemble_matching/detail',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data|1-50': [{
        'id|+1': 1,
        'areaId|+1': 3,
        'grossWeight': 48.11000000,
        'length': 544.00000000,
        'material': 'Q355B',
        'monomerId': 3,
        'netWeight': 48.11000000,
        'projectId': 38,
        'quantity|1-100': 1,
        'remark': null,
        'serialNumber': '@cword',
        'specification': 'PL18*728',
        'totalGrossWeight': 48.11000000,
        'totalNetWeight': 48.11000000,
        'type': null,
        'usedQuantity|1-100': 1,
        'producedQuantity|1-100': 1
      }]
    }
  }
}

export default [
  getList,
  getDetail
]
