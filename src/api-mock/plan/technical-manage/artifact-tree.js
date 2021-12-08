
const getArtifactTree = {
  url: '/api/plan/artifactMachinePart/listByCondition',
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
        'content': [{
          'id': 8,
          'assembleSerialNumber': '',
          'name': '焊接H钢柱',
          'serialNumber': 'GZ1-4',
          'specification': 'BH600*600*14*28',
          'length': 9770.00000000,
          'material': 'Q355B',
          'quantity': 1,
          'netWeight': 3968.98000000,
          'totalNetWeight': 3968.98000000,
          'grossWeight': 3968.98000000,
          'totalGrossWeight': 3968.98000000,
          'area': 12.00000000,
          'drawingNumber': null,
          'remark': null,
          'boolStatusEnum': true,
          'machinePartDTOList': [{
            'id': 4,
            'name': null,
            'serialNumber': 'A11',
            'specification': 'PL10*509',
            'length': 728.00000000,
            'material': 'Q355B',
            'quantity': 1,
            'netWeight': 28.93000000,
            'totalNetWeight': 28.93000000,
            'grossWeight': 28.93000000,
            'totalGrossWeight': 28.93000000,
            'remark': null,
            'boolStatusEnum': true
          }]
        }, {
          'id': 7,
          'assembleSerialNumber': '',
          'name': '焊接H钢柱',
          'serialNumber': 'GZ1-3',
          'specification': 'BH600*600*14*28',
          'length': 10919.00000000,
          'material': 'Q355B',
          'quantity': 1,
          'netWeight': 4135.82000000,
          'totalNetWeight': 4135.82000000,
          'grossWeight': 4135.82000000,
          'totalGrossWeight': 4135.82000000,
          'area': 12.00000000,
          'drawingNumber': null,
          'remark': null,
          'boolStatusEnum': true,
          'machinePartDTOList': [{
            'id': 3,
            'name': null,
            'serialNumber': 'A10',
            'specification': 'PL18*728',
            'length': 544.00000000,
            'material': 'Q355B',
            'quantity': 1,
            'netWeight': 48.11000000,
            'totalNetWeight': 48.11000000,
            'grossWeight': 48.11000000,
            'totalGrossWeight': 48.11000000,
            'remark': null,
            'boolStatusEnum': true
          }]
        }]
      }
    }
  }
}

const editArtifactTree = {
  url: '/api/plan/artifactMachinePart',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': 10
    }
  }
}

const editArtifactTreeStatus = {
  url: RegExp('/api/plan/artifactMachinePart/updateStatus/' + '[1-9][0-9]*' + '/' + '[1-9][0-9]*'),
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': 10
    }
  }
}

const delArtifactTree = {
  url: '/api/plan/artifactMachinePart/deleteArtifact',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const artifactTreelistUpload = {
  url: '/api/plan/artifactMachinePart/import',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}
export default [
  getArtifactTree,
  editArtifactTree,
  editArtifactTreeStatus,
  delArtifactTree,
  artifactTreelistUpload
]