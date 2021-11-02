
const getUnit = {
  url: '/api/config/base-config/unit',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'content': [
          { 'name': '毫米', 'symbol': '㎜', 'type': '长度单位', 'state': true, isSystem: true },
          { 'name': '米', 'symbol': 'm', 'type': '长度单位', 'state': true, isSystem: true },
          { 'name': '平方米', 'symbol': '㎡', 'type': '面积单位', 'state': true, isSystem: true },
          { 'name': '立方米', 'symbol': 'm³', 'type': '体积单位', 'state': false, isSystem: true },
          { 'name': '千克', 'symbol': '㎏', 'type': '质量单位', 'state': true, isSystem: true },
          { 'name': '张', 'symbol': '', 'type': '计数单位', 'state': true, isSystem: false },
          { 'name': '卷', 'symbol': '', 'type': '计数单位', 'state': true, isSystem: false },
          { 'name': '桶', 'symbol': '', 'type': '计数单位', 'state': true, isSystem: false },
          { 'name': '箱', 'symbol': '', 'type': '计数单位', 'state': true, isSystem: false }
        ],
        totalElements: 9
      }
    }
  }
}

// 批量添加单位
const batchAddUnit = {
  url: '/api/config/base-config/unit/batch',
  method: 'post',
  timeout: 5000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 删除单位
const delUnit = {
  url: '/api/config/base-config/unit',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

export default [
  getUnit,
  batchAddUnit,
  delUnit
]
