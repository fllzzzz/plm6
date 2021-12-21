import { enclosureListInfo } from '../common-mock-data/product-type-data'

const getEnclosure = {
  url: '/api/mes/building/print/enclosure',
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
        'content|1-5': [{
          ...enclosureListInfo,
          // 'category': Number(res.query.category),
          'printedQuantity|1-100': 1
        }]
      }
    }
  }
}

export default [
  getEnclosure
]
