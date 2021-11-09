import { ElMessage } from 'element-plus'

const prefix = 'source_'

// 行内修改
export default function useRowEdit(editApi, fields = []) {
  const fieldsMap = new Map()
  // 生成map
  const _fields = Array.isArray(fields) ? fields : [fields]
  _fields.forEach(f => {
    fieldsMap.set(f, `${prefix}${f}`)
  })

  // row初始化
  const rowInit = (list) => {
    list.forEach(row => {
      row.editMode = false
      row.editLoading = false
      fieldsMap.forEach((sourceKey, key) => {
        row[sourceKey] = row[key]
      })
    })
  }

  // 取消修改
  const cancelRowEdit = (row) => {
    fieldsMap.forEach((sourceField, field) => {
      row[field] = row[sourceField]
    })
    row.editMode = false
    ElMessage({ message: '已退出修改', type: 'warning' })
  }

  // 确认修改
  const confirmRowEdit = async (row) => {
    const flag = validateChange(row)
    if (!flag) {
      row.editMode = false
      ElMessage({ message: '未发生修改', type: 'info' })
      return
    }
    row.editLoading = true
    const updateFrom = {
      id: row.id
    }
    _fields.forEach(field => {
      updateFrom[field] = row[field]
    })
    try {
      await editApi(updateFrom)
      row.editMode = false
      // 重新给source赋值
      fieldsMap.forEach((sourceKey, key) => {
        row[sourceKey] = row[key]
      })
      ElMessage({ message: '修改成功', type: 'success' })
    } catch (error) {
      console.log('修改最低库存', error)
      ElMessage({ message: '保存失败', type: 'error' })
    } finally {
      row.editLoading = false
    }
  }

  // 校验值是否发生改变
  const validateChange = (row) => {
    let flag = false
    for (const [field, sourceField] of fieldsMap) {
      if (row[field] !== row[sourceField]) {
        flag = true
        break
      }
    }
    return flag
  }

  return {
    rowInit,
    cancelRowEdit,
    confirmRowEdit
  }
}
