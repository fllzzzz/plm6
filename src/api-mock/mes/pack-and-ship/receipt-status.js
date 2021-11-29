import {
  patternLicensePlate,
  validatorPhone
} from '@/utils/validate/pattern'
import commonDetailRes from './common-detail'

const getReceiptList = {
  url: '/api/mes/building/cargo/receipt',
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
          'manufactureType|1': [0, 1],
          'productType|1': [2, 4, 8],
          'serialNumber': '@datetime("yyyy-MM-dd")',
          'licensePlate': patternLicensePlate,
          'driverName': '@cname',
          'driverPhone': validatorPhone,
          'auditUserName': '@cname',
          'auditTime': '@datetime',
          'receiptName': '@cname',
          'auditReceiptName': '@cname',
          'auditReceiptTime': '@datetime',
          'receiptStatus|1': [1, 2]
        }]
      }
    }
  }
}

const getReceiptDetail = {
  url: RegExp('/api/mes/building/cargo/receipt/' + '[1-9][0-9]*'),
  method: 'get',
  timeout: 1000,
  response: () => {
    return commonDetailRes
  }
}

export default [
  getReceiptList,
  getReceiptDetail
]
