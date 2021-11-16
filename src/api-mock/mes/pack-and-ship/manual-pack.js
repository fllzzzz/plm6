const getArtifactPackList = {
  url: '/api/mes/building/package/artifact/use',
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
            'name': '东厂',
            shortName: '东厂'
          },
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
          'packageQuantity': 7,
          'unPackageQuantity': 3
        }, {
          'id': 2,
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
            'name': '东厂',
            shortName: '东厂'
          },
          'name': '11',
          'serialNumber': '002',
          'specification': '100*200',
          'length': 200.00000000,
          'material': 'Q3356',
          'netWeight': 1.00000000,
          'totalNetWeight': 10.00000000,
          'grossWeight': 20.00000000,
          'totalGrossWeight': 1.00000000,
          'quantity': 100,
          'inQuantity': 10,
          'packageQuantity': 1,
          'unPackageQuantity': 9
        }],
        'enclosureList': null
      }
    }
  }
}

const getEnclosurePackList = {
  url: '/api/mes/building/package/enclosure/use',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'artifactList': null,
        'enclosureList': [{
          'id': 3,
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
            'name': '东厂',
            shortName: '东厂'
          },
          'category': 1,
          'name': '嗡嗡嗡',
          'serialNumber': '001',
          'plate': 'x',
          'color': '红',
          'material': 'Q335',
          'weight': 9.90000000,
          'thickness': 20.00000000,
          'width': 50.00000000,
          'length': 100.00000000,
          'quantity': 100,
          'totalLength': 10000.00000000,
          'totalArea': 200.00000000,
          'brand': '1',
          'type': '1',
          'capacity': '1',
          'inQuantity': 10,
          'packageQuantity': 3,
          'unPackageQuantity': 7
        }]
      }
    }
  }
}

const getAuxiliaryPackList = {
  url: '/api/mes/building/package/auxiliaryMaterial/use',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {}
  }
}

const pack = {
  url: '/api/mes/building/package',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const editPack = {
  url: '/api/mes/building/package',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const additionalPack = {
  url: '/api/mes/building/package/add',
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
  getArtifactPackList,
  getEnclosurePackList,
  getAuxiliaryPackList,
  pack,
  editPack,
  additionalPack
]
