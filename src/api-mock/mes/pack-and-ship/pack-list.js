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
        'totalElements': 4,
        'content': [{
          'id': 9,
          'project': {
            'id': 1,
            'name': 'xm001',
            'shortName': 'xm001',
            'contractNo': '2121212'
          },
          'serialNumber': '2021-11-09-04',
          'productType': 2,
          'status': false,
          'remark': '',
          'printType': false,
          'userId': 1,
          'userName': '超级管理员',
          'createTime': 1636437879000,
          'quantity': 3,
          'enclLength': 0,
          'totalGrossWeight': 60.00000000,
          'totalNetWeight': 3.00000000
        }, {
          'id': 11,
          'project': {
            'id': 1,
            'name': 'xm001',
            'shortName': 'xm001',
            'contractNo': '2121212'
          },
          'serialNumber': '2021-11-09-05',
          'productType': 2,
          'status': false,
          'remark': '2222',
          'printType': false,
          'userId': 1,
          'userName': '超级管理员',
          'createTime': 1636440562000,
          'quantity': 3,
          'enclLength': 0,
          'totalGrossWeight': 60.00000000,
          'totalNetWeight': 3.00000000
        }, {
          'id': 12,
          'project': {
            'id': 1,
            'name': 'xm001',
            'shortName': 'xm001',
            'contractNo': '2121212'
          },
          'serialNumber': '2021-11-09-06',
          'productType': 2,
          'status': false,
          'remark': '1213',
          'printType': false,
          'userId': 1,
          'userName': '超级管理员',
          'createTime': 1636446532000,
          'quantity': 2,
          'enclLength': 0,
          'totalGrossWeight': 40.00000000,
          'totalNetWeight': 2.00000000
        }, {
          'id': 17,
          'project': {
            'id': 1,
            'name': 'xm001',
            'shortName': 'xm001',
            'contractNo': '2121212'
          },
          'serialNumber': '2021-11-10-04',
          'productType': 4,
          'status': false,
          'remark': '123213123213',
          'printType': false,
          'userId': 1,
          'userName': '超级管理员',
          'createTime': 1636512111000,
          'quantity': 3,
          'enclLength': 300.00000000,
          'totalGrossWeight': 0,
          'totalNetWeight': 0
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
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'artifactList': [{
          'id': 1,
          'project': {
            'id': 1,
            'name': 'xm001',
            'shortName': 'xm001',
            'contractNo': '2121212'
          },
          'monomer': {
            'id': 1,
            'name': '单体1'
          },
          'district': {
            'id': 1,
            'name': '区域'
          },
          'factory': {
            'id': 1,
            'name': '东厂'
          },
          'productId': 1,
          'name': '钢柱',
          'serialNumber': '001',
          'specification': '1700*63',
          'length': 100.00000000,
          'material': 'Q335B',
          'netWeight': 1.00000000,
          'totalNetWeight': 10.00000000,
          'grossWeight': 20.00000000,
          'totalGrossWeight': 1.00000000,
          'quantity': 100,
          'inQuantity': 10,
          'cargoQuantity': null,
          'packageQuantity': 3,
          'unPackageQuantity': 6
        }],
        'enclosureList': null,
        'remark': 'remark',
        'status': false
      }
    }
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
