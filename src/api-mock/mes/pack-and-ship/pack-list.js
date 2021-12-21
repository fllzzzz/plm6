import commonDetailRes from './common-detail'

const getPack = {
  url: '/api/mes/building/package/page',
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
        'content|1-5': [{
          'id|+1': 1,
          'project': {
            'id': 1,
            'name': '@cword(2,15)',
            'shortName': '@cword(2,9)',
            'contractNo': '@guid'
          },
          'serialNumber': '@datetime("yyyy-MM-dd")',
          'productType|1': [2, 4, 8],
          'status|1-2': false,
          'remark': '@cword(2,60)',
          'printType|1-2': false,
          'userId': 1,
          'userName': '@cname',
          'createTime': '@datetime',
          'quantity|1-100': 3,
          'enclLength|1-100': 1,
          'totalGrossWeight|1-1000.1-8': 60.00000000,
          'totalNetWeight|1-1000.1-8': 3.00000000
        }]
      }
    }
  }
}

const getPackDetail = {
  url: RegExp('/api/mes/building/package/' + '[1-9][0-9]*'),
  method: 'get',
  timeout: 1000,
  response: () => {
    return commonDetailRes
  }
}

const deletePack = {
  url: '/api/mes/building/package',
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
  getPack,
  getPackDetail,
  deletePack
]
