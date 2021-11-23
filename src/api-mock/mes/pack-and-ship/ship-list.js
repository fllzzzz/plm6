import {
  validatorLicensePlate,
  validatorPhone
} from '@/utils/validate/pattern'
import commonDetailRes from './common-detail'

const getShipList = {
  url: '/api/mes/building/cargo/ship',
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
          'auditTime': '@datetime',
          'manufactureType|1': [0, 1],
          'productType|1': [2, 4, 8],
          'serialNumber': '@datetime("yyyy-MM-dd")',
          'licensePlate': validatorLicensePlate,
          'driverName': '@cname',
          'driverPhone': validatorPhone,
          'totalNetWeight|1-1000.1-8': 1.00000000,
          'totalGrossWeight|1-1000.1-8': 20.00000000,
          'actualWeight|1-1000.1-8': 20.00000000,
          'auditUserName': '@cname'
        }]
      }
    }
  }
}

const getShipDetail = {
  url: RegExp('/api/mes/building/cargo/ship/' + '[1-9][0-9]*'),
  method: 'get',
  timeout: 1000,
  response: () => {
    return commonDetailRes
  }
}

const getSummaryShipMete = {
  url: '/api/mes/building/cargo/ship/summary',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'weight|1-10000.1-8': 1.00000000
      }
    }
  }
}

export default [
  getShipList,
  getShipDetail,
  getSummaryShipMete
]
