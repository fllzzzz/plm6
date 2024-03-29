const getBoardForArtifact = {
  url: '/api/mes/building/warehouse/artifact/page',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'hasPreviousPage|1-2': false,
        'hasNextPage|1-2': false,
        'totalElements|1-100': 1,
        'content|1-10': [{
          'createTime': '@datetime',
          'id|+1': 1,
          'projectId|1-100': 1,
          'monomerId|1-100': 1,
          'areaId|1-100': 1,
          'boolStatusEnum|1-2': true,
          'name': '@cword(2,10)',
          'serialNumber': '@word',
          'specification': '@word',
          'length|1-100000.1-8': 10919.00000000,
          'material': 'Q355B',
          'quantity|1-10000': 1,
          'netWeight|1-100000.1-8': 4135.82000000,
          'totalNetWeight|1-100000.1-8': 4135.82000000,
          'grossWeight|1-100000.1-8': 4135.82000000,
          'totalGrossWeight|1-100000.1-8': 4135.82000000,
          'surfaceArea|1-100000.1-8': 12.00000000,
          'drawingNumber': '@word',
          'remark': '@cword(4,60)',
          'businessId|1-100': 1,
          'assembleId|1-100': 1,
          'assembleSerialNumber': '@word',
          'producedQuantity|1-10000': 1,
          'totalSchedulingQuantity|1-10000': 1,
          'totalTaskQuantity|1-10000': 1,
          'projectName': '@cword(2,10)',
          'monomerName': '@cword(2,10)',
          'areaName': '@cword(2,10)',
          'packageQuantity|1-10000': 1,
          'unPackageQuantity|1-10000': 1,
          'inboundQuantity|1-10000': 1,
          'outboundQuantity|1-10000': 1,
          'taskQuantity|1-10000': 1
        }]
      }
    }
  }
}

const getBoardForEnclosure = {
  url: '/api/mes/building/warehouse/enclosure/page',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'hasPreviousPage|1-2': false,
        'hasNextPage|1-2': false,
        'totalElements|1-100': 1,
        'content|1-10': [{
          'createTime': '@datetime',
          'id|+1': 1,
          'projectId|1-100': 1,
          'monomerId|1-100': 1,
          'areaId|1-100': 1,
          'boolStatusEnum|1-2': true,
          'category|1-100': 1,
          'name': '@cword(2,10)',
          'serialNumber': '@word',
          'plate': '@word',
          'color|1': ['红色', '蓝色'],
          'material': '@word',
          'weight|1-100': 1,
          'thickness|1-100': 1,
          'width|1-100000.1-8': 700.00000000,
          'length|1-100000.1-8': 12270.00000000,
          'quantity|1-10000': 147,
          'totalLength|1-100000.1-8': 1803.69000000,
          'totalArea|1-100000.1-8': 1262.58300000,
          'remark': '@cword(4,60)',
          'businessId|1-100': 1,
          'brand': '品牌',
          'type': null,
          'capacity|1-100': 1,
          'producedQuantity|1-100': 1,
          'totalSchedulingQuantity|1-100': 1,
          'totalTaskQuantity|1-100': 1,
          'projectName': '@cword(2,10)',
          'monomerName': '@cword(2,10)',
          'areaName': '@cword(2,10)',
          'inboundQuantity|1-10000': 1,
          'outboundQuantity|1-10000': 1,
          'taskQuantity|1-10000': 1
        }]
      }
    }
  }
}

const getBoardForArtifactSummary = {
  url: '/api/mes/building/warehouse/artifact/summary',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'quantity': 200,
        'inboundQuantity': 130,
        'outboundQuantity': 3,
        'stockQuantity': 127,
        'mete': 40000,
        'inboundMete': 29000,
        'outboundMete': 500,
        'stockMete': 28500
      }
    }
  }
}

const getBoardForEnclosureSummary = {
  url: '/api/mes/building/warehouse/enclosure/summary',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'quantity': 100,
        'inboundQuantity': 10,
        'outboundQuantity': 1,
        'stockQuantity': 9,
        'mete': 10000,
        'inboundMete': 1000,
        'outboundMete': 100,
        'stockMete': 900
      }
    }
  }
}

export default [
  getBoardForArtifact,
  getBoardForEnclosure,
  getBoardForArtifactSummary,
  getBoardForEnclosureSummary
]
