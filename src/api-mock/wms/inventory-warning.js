import { enabledEnum } from '@enum-ms/common'
import { measureTypeEnum } from '@enum-ms/wms'

// 获取库存预警
const getMaterialIW = {
  url: '/api/wms/material/inventory-warning',
  method: 'get',
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            id: 1,
            classifyId: 204,
            // 'serialNumber': /[0-9]{10}/,
            workshop: { id: 3, name: '1号车间' },
            // 'fullClassifyName': '紧固件>高强螺栓>大六角',
            // 'classifyName': '大六角',
            specification: 'M26 * 65',
            // 'unit': '套',
            unitType: measureTypeEnum.ACCOUNTING.V,
            'minimumInventory|10-200': 10,
            enabled: enabledEnum.TRUE.V,
            createTime: '@datetime(T)',
            updateTime: '@datetime(T)'
          },
          {
            id: 2,
            classifyId: 204,
            // 'serialNumber': /[0-9]{10}/,
            // workshop: undefined,
            // 'fullClassifyName': '紧固件>螺母',
            // 'classifyName': '螺母',
            specification: 'M26 * 70',
            // 'unit': '个',
            unitType: measureTypeEnum.ACCOUNTING.V,
            'minimumInventory|10-200': 10,
            enabled: enabledEnum.FALSE.V,
            createTime: '@datetime(T)',
            updateTime: '@datetime(T)'
          }
        ],
        totalElements: 2
      }
    }
  }
}

// 批量添加库存预警
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

// 删除库存预警
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

// 获取库存预警通知配置
const getInventoryNotifyConf = {
  url: '/api/wms/material/inventory-warning/notify-config',
  method: 'get',
  timeout: 5000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [
          {
            workshopId: 3,
            userIds: [23, 36, 52],
            deptIds: [22]
          }
        ]
      }
    }
  }
}

// 保存库存预警通知配置
const setInventoryNotifyConf = {
  url: '/api/wms/material/inventory-warning/notify-config',
  method: 'put',
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
  delMaterialIW,
  getInventoryNotifyConf,
  setInventoryNotifyConf
]
