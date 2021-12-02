import {
  patternLicensePlate
} from '@/utils/validate/pattern'
import commonDetailRes from './common-detail'

const getShipAuditList = {
  url: '/api/mes/building/cargo/review',
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
          'licensePlate': patternLicensePlate,
          'serialNumber': '@datetime',
          'actualTime': '@datetime',
          'loadingTime': '@datetime',
          'totalNetWeight|1-1000.1-8': 1.00000000,
          'totalGrossWeight|1-1000.1-8': 20.00000000,
          'actualWeight|1-1000.1-8': 20.00000000,
          'structureMeasureMode|1': [1, 2],
          'enclosureMeasureMode|1': [1, 2],
          'shipAmount|1-1000.1-8': 1,
          'userName': '@cname',
          'actualUserName': '@cname',
          'auditUserName': '@cname',
          'checkStatus|1': [1, 2],
          'productType|1': [2, 4, 6, 8, 10, 12, 14]
        }]
      }
    }
  }
}

const shipAuditDetail = {
  url: RegExp('/api/mes/building/cargo/review/' + '[1-9][0-9]*'),
  method: 'get',
  timeout: 1000,
  response: () => {
    return commonDetailRes
  }
}

const shipAudit = {
  url: '/api/mes/building/cargo/review',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

export default [
  getShipAuditList,
  shipAudit,
  shipAuditDetail
]
