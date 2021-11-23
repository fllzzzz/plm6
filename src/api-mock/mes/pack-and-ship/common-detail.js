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
      'inQuantity|1-100': 1,
      'cargoQuantity|1-100': 1,
      'packageQuantity|1-100': 1,
      'unPackageQuantity|1-100': 1
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
      'inQuantity|1-100': 1,
      'cargoQuantity|1-100': 1,
      'packageQuantity|1-100': 1,
      'unPackageQuantity|1-100': 1
    }],
    'remark': 'remark',
    'status': false
  }
}
