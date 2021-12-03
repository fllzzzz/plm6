export default {
  'code': 20000,
  'message': '成功',
  'data': {
    'artifactList|1-10': [{
      'id|+1': 1,
      'project': {
        'id': 1,
        'name': '项目' + '@cword(2,15)',
        'shortName': '@cword(2,9)',
        'contractNo': '@guid'
      },
      'monomer': {
        'id': 1,
        'name': '单体' + '@cword(2,15)'
      },
      'district': {
        'id': 1,
        'name': '区域' + '@cword(2,15)'
      },
      'factory': {
        'id': 1,
        'name': '工厂' + '@cword(2,15)'
      },
      'productId|+1': 1,
      'name': '@cword(2,15)',
      'serialNumber': '@word(2,15)',
      'specification': '@word',
      'length|1-100000.1-8': 100.00000000,
      'material': 'Q335B',
      'netWeight|1-100000.1-8': 1.00000000,
      'totalNetWeight|1-100000.1-8': 10.00000000,
      'grossWeight|1-100000.1-8': 20.00000000,
      'totalGrossWeight|1-100000.1-8': 1.00000000,
      'quantity|1-100': 1,
      'shipQuantity|1-100': 1,
      'inQuantity|1-100': 1,
      'cargoQuantity|1-100': 1,
      'packageQuantity|1-100': 1,
      'unPackageQuantity|1-100': 1,
      'unitPrice|1-1000.1-8': 1
    }],
    'enclosureList|1-10': [{
      'id|+1': 1,
      'project': {
        'id': 1,
        'name': '项目' + '@cword(2,15)',
        'shortName': '@cword(2,9)',
        'contractNo': '@guid'
      },
      'monomer': {
        'id': 1,
        'name': '单体' + '@cword(2,15)'
      },
      'district': {
        'id': 1,
        'name': '区域' + '@cword(2,15)'
      },
      'factory': {
        'id': 1,
        'name': '工厂' + '@cword(2,15)'
      },
      'productId|+1': 1,
      'name': '@cword(2,15)',
      'serialNumber': '@word(2,15)',
      'specification': '@word',
      'length|1-100000.1-8': 100.00000000,
      'width|1-100000.1-8': 100.00000000,
      'thickness|1-100000.1-8': 100.00000000,
      'material': 'Q335B',
      'plate': '板型' + '@cword(2,15)',
      'color|1': ['红色', '蓝色'],
      'quantity|1-100': 1,
      'shipQuantity|1-100': 1,
      'inQuantity|1-100': 1,
      'cargoQuantity|1-100': 1,
      'packageQuantity|1-100': 1,
      'unPackageQuantity|1-100': 1,
      'unitPrice|1-1000.1-8': 1
    }],
    'review': {
      'actualTime': '2021-11-22T10:20:47',
      'actualUserName': '超级管理员',
      'availableAmount|1-10000.1-8': 0,
      'contractAmount|1-10000.1-8': 1111,
      'deliveryAmount|1-10000.1-8': 0,
      'id': null,
      'licensePlate': 'string',
      'loadTime': '2021-11-17T17:20:14',
      'loadUserName': '超级管理员',
      'managementFee|1-10000.1-8': 111.1,
      'name': 'xm001',
      'orderType': '',
      'safeAmount|1-10000.1-8': 0,
      'serialNumber': '2121212',
      'shortName': 'xm001',
      'totalCollectionAmount|1-10000.1-8': 0,
      'totalDeliveryAmount|1-10000.1-8': 0,
      'contractReceivableAmount|1-10000.1-8': 0,
      'billingReceivableAmount|1-10000.1-8': 0
    },
    'remark': 'remark',
    'status': false
  }
}
