import {
  artifactListInfo, enclosureListInfo
} from '../common-mock-data/product-type-data'

const getList = {
  url: '/api/mes/building/warehouse/report',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': [{
        'id': 1,
        'name': '@cword',
        'shortName': '@cword(2,3)',
        'serialNumber': '@word',
        'productType|1': [2, 4],
        'unit|1': ['kg', 'm'],
        'beginMete|1-1000.1-8': 0,
        'inboundMete|1-1000.1-8': 300.00000000,
        'outboundMete|1-1000.1-8': 1,
        'endMete|1-1000.1-8': 300.00000000
      }]
    }
  }
}

const getSummary = {
  url: '/api/mes/building/warehouse/report/summary',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'beginMete|1-1000.1-8': 0,
        'inboundMete|1-1000.1-8': 300.00000000,
        'outboundMete|1-1000.1-8': 1,
        'endMete|1-1000.1-8': 300.00000000
      }
    }
  }
}

const getEnclosureDetail = {
  url: '/api/mes/building/warehouse/report/enclosure',
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
        'content|1-50': [{
          ...enclosureListInfo
        }]
      }
    }
  }
}

const getArtifactDetail = {
  url: '/api/mes/building/warehouse/report/artifact',
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
        'content|1-50': [{
          ...artifactListInfo
        }]
      }
    }
  }
}

export default [
  getList,
  getSummary,
  getEnclosureDetail,
  getArtifactDetail
]
