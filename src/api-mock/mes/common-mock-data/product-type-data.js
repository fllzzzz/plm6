export const artifactListInfo = {
  'createTime': '@datetime',
  'id|+1': 1,
  'project': {
    'id': 1,
    'name': '@cword(2,15)',
    'shortName': '@cword(2,9)',
    'contractNo': '@guid'
  },
  'monomer': {
    'id': 1,
    'name': '单体1'
  },
  'area': {
    'id': 1,
    'name': '区域'
  },
  'name': '@cword(2,5)',
  'serialNumber': '@word(2,6)',
  'specification': '@word(2,10)',
  'length|1-10000.1-8': 1.00000000,
  'material': 'Q355B',
  'netWeight|1-10000.1-8': 4135.82000000,
  'totalNetWeight|1-10000.1-8': 4135.82000000,
  'grossWeight|1-10000.1-8': 4135.82000000,
  'totalGrossWeight|1-10000.1-8': 4135.82000000,
  'surfaceArea|1-10000.1-8': 12.00000000,
  'drawingNumber': '@word(2,6)',
  'remark': '@cword(2,60)',
  'quantity|1-100': 10
}

export const enclosureListInfo = {
  'createTime': '@datetime',
  'id|+1': 1,
  'project': {
    'id': 1,
    'name': '@cword(2,15)',
    'shortName': '@cword(2,9)',
    'contractNo': '@guid'
  },
  'monomer': {
    'id': 1,
    'name': '单体1'
  },
  'area': {
    'id': 1,
    'name': '区域'
  },
  'name': '@word(2,10)',
  'serialNumber': '@word(2,6)',
  'plate': '@word(2,6)',
  'color|1': ['蓝色', '红色'],
  'material': '@word(2,6)',
  'weight|1-10000.1-8': 111.00000000,
  'thickness|1-10000.1-8': 0.53000000,
  'width|1-10000.1-8': 820.00000000,
  'length|1-10000.1-8': 12270.00000000,
  'quantity|1-1000': 147,
  'totalLength|1-10000.1-8': 1803.69000000,
  'totalArea|1-10000.1-8': 1479.02580000,
  'remark': '@cword(2,60)',
  'brand': '@cword(2,10)',
  'type': 4,
  'capacity|1-10000.1-8': 1
}
