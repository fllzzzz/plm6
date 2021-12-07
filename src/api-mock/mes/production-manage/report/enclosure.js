import { enclosureListInfo } from '../../common-mock-data/product-type-data'

const getEnclosure = {
  url: '/api/mes/building/production_statements/enclosure/page',
  method: 'get',
  timeout: 1000,
  response: (res) => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'hasPreviousPage': false,
        'hasNextPage': false,
        'totalElements': 2,
        'content|1-100': [{
          ...enclosureListInfo,
          // 'category': Number(res.query.category),
          date: '@datetime'
        }]
      }
    }
  }
}

const getEnclosureSummary = {
  url: '/api/mes/building/production_statements/enclosure/summary',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'quantity|1-1000': 1,
        'totalLength|1-10000.1-8': 1
      }
    }
  }
}

export default [
  getEnclosure,
  getEnclosureSummary
]
