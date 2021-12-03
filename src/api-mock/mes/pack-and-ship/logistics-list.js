import {
  patternLicensePlate,
  validatorPhone
} from '@/utils/validate/pattern'

const getLogisticsList = {
  url: '/api/mes/building/cargo/logistics',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'hasPreviousPage': false,
        'hasNextPage': false,
        'totalElements': 1,
        'content|1-50': [{
          'id|+1': 1,
          'project': {
            'id': 1,
            'name': '@cword(2,15)',
            'shortName': '@cword(2,9)',
            'contractNo': '@guid'
          },
          'supplier': {
            'id': 1,
            'name': '@cword(2,16)',
            'priceType|1': [1, 2],
            'price|1-1000.1-8': 10.00000000
          },
          'totalPrice|1-1000.1-8': 0,
          'manufactureType|1': [1, 2],
          'productType|1': [2, 4, 6, 8, 10, 12, 14],
          'serialNumber': '@datetime("yyyy-MM-dd")',
          'licensePlate': patternLicensePlate,
          'driverName': '@cname',
          'driverPhone': validatorPhone,
          'totalNetWeight|1-1000.1-8': 1.00000000,
          'totalGrossWeight|1-1000.1-8': 20.00000000,
          'actualWeight|1-1000.1-8': 20.00000000,
          'auditTime': '@datetime'
        }]
      }
    }
  }
}

const getLogisticsPrice = {
  url: '/api/mes/building/supper/price',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'hasPreviousPage': false,
        'hasNextPage': false,
        'totalElements': 1,
        'content|1-100': [{
          'id|+1': 1,
          'project': {
            'id': 1,
            'name': '@cword(2,15)',
            'shortName': '@cword(2,9)',
            'contractNo': '@guid'
          },
          'supplier': '@cword(2,16)',
          'priceType|1': [1, 2],
          'price|1-1000.1-8': 10.00000000,
          'boolContainTaxEnum|1-2': false,
          'tax|1': [2.00000000, null],
          'userId|1-100': 1,
          'userName': '@cname',
          'createTime': '@datetime'
        }]
      }
    }
  }
}

const addLogisticsPrice = {
  url: '/api/mes/building/supper/price',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const editLogisticsPrice = {
  url: '/api/mes/building/supper/price',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const delLogisticsPrice = {
  url: RegExp('/api/mes/building/supper/price/' + '[1-9][0-9]*'),
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

export default [
  getLogisticsList,
  getLogisticsPrice,
  addLogisticsPrice,
  editLogisticsPrice,
  delLogisticsPrice
]
