import {
  enclosureListInfo
} from '../common-mock-data/product-type-data'

const getEnclosure = {
  url: '/api/mes/building/team/report/enclosure',
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
        'content|1-20': [{
          'factory': {
            'id': 1,
            'name': '工厂' + '@cword(2,15)'
          },
          'workshop': {
            'id': 1,
            'name': '车间' + '@cword(2,15)'
          },
          'productionLine': {
            'id': 1,
            'name': '生产线' + '@cword(2,15)'
          },
          'completeLength|100-998.1-8': 1,
          'taskLength|999-1000.1-8': 1
        }]
      }
    }
  }
}

const getEnclosureDetail = {
  url: '/api/mes/building/team/report/enclosure/detail',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'content|1-20': [{
          ...enclosureListInfo,
          'productTime': '@datetime'
        }]
      }
    }
  }
}

export default [
  getEnclosure,
  getEnclosureDetail
]
