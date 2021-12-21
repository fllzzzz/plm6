
const getMonomer = {
  url: '/api/plan/monomer',
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
          'createTime': 1635406933000,
          'id': 4,
          'name': '加工承揽单体',
          'date': 1635609600000,
          'projectId': 42,
          'mainStructure': 0.000,
          'mainStructureDate': 1635350400000,
          'contourPlate': 0.000,
          'contourPlateDate': 1635350400000,
          'battenBoard': 787.000,
          'battenBoardDate': 1635350400000,
          'pressureBearingPlate': 787.000,
          'pressureBearingPlateDate': 1635350400000,
          'trussFloorPlate': 787.000,
          'trussFloorPlateDate': 1635350400000,
          'flangingPiece': 0.000,
          'flangingPieceDate': 1635350400000,
          'sort': 1,
          'remark': '发顺丰'
        }, {
          'createTime': 1637547122000,
          'id': 5,
          'name': '1',
          'date': 1635696000000,
          'projectId': 42,
          'mainStructure': 11.000,
          'mainStructureDate': 1635696000000,
          'contourPlate': 0.000,
          'contourPlateDate': 1635696000000,
          'battenBoard': 1.000,
          'battenBoardDate': 1635696000000,
          'pressureBearingPlate': 1.000,
          'pressureBearingPlateDate': 1635696000000,
          'trussFloorPlate': 1.000,
          'trussFloorPlateDate': 1635696000000,
          'flangingPiece': 1.000,
          'flangingPieceDate': 1635696000000,
          'sort': 1,
          'remark': ''
        }]
      }
    }
  }
}

const addMonomer = {
  url: '/api/plan/monomer',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': 10
    }
  }
}

const editMonomer = {
  url: '/api/plan/monomer',
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

const delMonomer = {
  url: '/api/plan/monomer',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}
const monomerAll = {
  url: RegExp('/api/plan/monomer/listByProjectId/' + '.*'),
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'totalElements': 2,
        'content': [{
          'id': 4,
          'name': '加工承揽单体',
          'productTypeList': [{
            'name': '夹芯板加工',
            'type': 1
          }, {
            'name': '压型楼承板加工',
            'type': 4
          }, {
            'name': '压型楼承板加工',
            'type': 4
          }, {
            'name': '压型楼承板加工',
            'type': 4
          }],
          'areaSimpleList': [{
            'name': '放松放松',
            'id': 5,
            'type': 0,
            'axis': '热望热望'
          }, {
            'name': '烦烦烦',
            'id': 6,
            'type': 1,
            'axis': '热情我认为'
          }, {
            'name': '也一样',
            'id': 7,
            'type': 1,
            'axis': '也有人要'
          }, {
            'name': '1',
            'id': 8,
            'type': 0,
            'axis': '1'
          }]
        }, {
          'id': 5,
          'name': '1',
          'productTypeList': [],
          'areaSimpleList': []
        }]
      }
    }
  }
}

const monomerDetail = {
  url: RegExp('/api/plan/monomer/listProductTypeListById/' + '.*'),
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'id': 4,
        'date': 1635609600000,
        'name': '加工承揽单体',
        'productTypeList': [{
          'no': 1,
          'name': '夹芯板',
          'date': 1635350400000
        }, {
          'no': 3,
          'name': '桁架楼承板',
          'date': 1635350400000
        }, {
          'no': 4,
          'name': '压型楼承板',
          'date': 1635350400000
        }, {
          'no': 6,
          'name': '折边件',
          'date': 1635350400000
        }]
      }
    }
  }
}
export default [
  getMonomer,
  addMonomer,
  editMonomer,
  delMonomer,
  monomerAll,
  monomerDetail
]
