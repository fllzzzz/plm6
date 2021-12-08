
const getAssembly = {
  url: '/api/plan/assemble/listAssemble',
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
          'id': 2,
          'serialNumber': 'L-116',
          'quantity': 2,
          'producedQuantity': 0,
          'usedQuantity': 0,
          'remark': null,
          'detailDTOList': [{
            'id': 10,
            'type': 2,
            'serialNumber': 'L-116-腹板',
            'specification': 'PL12*718',
            'length': 9180.00000000,
            'material': 'Q355B',
            'netWeight': 10.00000000,
            'quantity': 2,
            'usedQuantity': 0,
            'producedQuantity': 0,
            'drawingNumber': null
          }, {
            'id': 11,
            'type': 2,
            'serialNumber': 'L-116-翼缘',
            'specification': 'PL16*350',
            'length': 9180.00000000,
            'material': 'Q355B',
            'netWeight': 10.00000000,
            'quantity': 4,
            'usedQuantity': 0,
            'producedQuantity': 0,
            'drawingNumber': null
          }],
          'artifactDTOList': [{
            'type': 1,
            'id': 9,
            'name': null,
            'serialNumber': '1-B1-3',
            'specification': 'H750*350*12*16',
            'length': 9180.00000000,
            'material': 'Q355B',
            'quantity': 2,
            'netWeight': 10.00000000,
            'producedQuantity': 0,
            'existStatus': 0
          }]
        }, {
          'id': 1,
          'serialNumber': 'L-115',
          'quantity': 0,
          'producedQuantity': 0,
          'usedQuantity': 0,
          'remark': null,
          'detailDTOList': [{
            'id': 2,
            'type': 2,
            'serialNumber': 'L-115-腹板',
            'specification': 'PL12*718',
            'length': 8480.00000000,
            'material': 'Q355B',
            'netWeight': 10.00000000,
            'quantity': 1,
            'usedQuantity': 0,
            'producedQuantity': 0,
            'drawingNumber': null
          }, {
            'id': 3,
            'type': 2,
            'serialNumber': 'L-115-翼缘',
            'specification': 'PL16*350',
            'length': 8480.00000000,
            'material': 'Q355B',
            'netWeight': 10.00000000,
            'quantity': 2,
            'usedQuantity': 0,
            'producedQuantity': 0,
            'drawingNumber': null
          }],
          'artifactDTOList': []
        }]
      }
    }
  }
}

const editAssembly = {
  url: '/api/plan/assemble/updateAssemble',
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


const delAssembly = {
  url:  RegExp('/api/plan/assemble/deleteAssemble/' + '.*' ),
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const delAssemblyArtifact= {
  url:  '/api/plan/assemble/deleteArtifact',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const addAssemblyArtifact = {
  url:  '/api/plan/assemble/saveArtifact',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const assemblylistUpload = {
  url:  '​/api​/plan​/assemble​/import',
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
  getAssembly,
  editAssembly,
  delAssembly,
  delAssemblyArtifact,
  addAssemblyArtifact,
  assemblylistUpload
]