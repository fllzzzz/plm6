import { enabledEnum } from '@enum-ms/common'
import { measureTypeEnum } from '@enum-ms/wms'

// 获取仓库位置
const getMaterialIW = {
  url: '/api/wms/material/inventory-warning',
  method: 'get',
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'content': [
          {
            'id|+1': 1,
            'subjectId|+1': 1,
            'serialNumber': /[0-9]{10}/,
            'factoryName': '萧山工厂',
            'fullClassifyName': '紧固件>高强螺栓>大六角',
            'classifyName': '大六角',
            'specification': /([A-Z0-9]{2,3}\*){1,3}[A-Z0-9]{2,3}/,
            'unit': '套',
            'unitType': measureTypeEnum.MEASURE.V,
            'minimumInventory|10-200': 10,
            'enabled': enabledEnum.TRUE.V,
            'createTime': '@datetime(T)',
            'updateTime': '@datetime(T)'
          },
          {
            'id|+1': 1,
            'subjectId|+1': 1,
            'serialNumber': /[0-9]{10}/,
            'factoryName': undefined,
            'fullClassifyName': '紧固件>螺母',
            'classifyName': '螺母',
            'specification': /([A-Z0-9]{2,3}\*){1,3}[A-Z0-9]{2,3}/,
            'unit': '个',
            'unitType': measureTypeEnum.ACCOUNTING.V,
            'minimumInventory|10-200': 10,
            'enabled': enabledEnum.FALSE.V,
            'createTime': '@datetime(T)',
            'updateTime': '@datetime(T)'
          }
        ],
        'totalElements': 2
      }
    }
  }
}

// 批量添加仓库
const batchAddMaterialIW = {
  url: '/api/wms/material/inventory-warning/batch',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 修改库存预警数量
const editMaterialIW = {
  url: '/api/wms/material/inventory-warning/minimumInventory',
  method: 'put',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 修改启用状态
const editEnabled = {
  url: RegExp('/api/wms/material/inventory-warning/enabled'),
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 删除仓库
const delMaterialIW = {
  url: '/api/wms/material/inventory-warning',
  method: 'delete',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

export default [
  getMaterialIW,
  batchAddMaterialIW,
  editMaterialIW,
  editEnabled,
  delMaterialIW
]
