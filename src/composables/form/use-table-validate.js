import { obj2arr } from '@/utils/convert/type'
import { isBlank, isNotBlank } from '@data-type/index'
import * as lodash from 'lodash'
import { ElMessage } from 'element-plus'

/**
 * form-table-校验
 * @param {array} list 校验列表
 * @param {object} rules 校验规则
 * @param {map} ditto=[] 含“同上”选项或值的字段。例： { name：'id', value: -1 }
 */
export default function useTableValidate({ rules = {}, ditto = new Map(), errorMsg = '请修正表格中标红的信息' }) {
  return {
    tableValidate: (list) => tableValidate(list, rules, ditto, errorMsg),
    wrongCellMask: (tableInfo) => wrongCellMask(tableInfo, rules),
    cleanUpData: (list) => cleanUpData(list, ditto)
  }
}

function tableValidate(list, tableRules, ditto, errorMsg) {
  const rules = getRules(tableRules)
  let flag = true
  let message = '请填写数据'
  const copyList = JSON.parse(JSON.stringify(list))
  const blankRowsIndex = [] // 数据为空的下标
  if (list && list.length > 0) {
    let isFirstRow = true // 首行，第一条有数据的记录

    for (const i in list) {
      const row = list[i]
      delete row.verify // 删除验证字段，以及进行空行处理

      // ------ 空行处理 start ------
      // 为了不删除有数据row的同上字段，因此拷贝一个row
      const rowCopy = JSON.parse(JSON.stringify(row))

      ditto.forEach((val, name) => {
        if (rowCopy[name] === val) {
          delete rowCopy[name] // 删除同上
        }
      })

      const rowArr = obj2arr(rowCopy)
      const notBlankRow = rowArr.some((v) => isNotBlank(v))
      if (!notBlankRow) {
        blankRowsIndex.push(i)
        continue
      }

      // ------ 空行处理 end------

      // 首行处理, 只删除首行的同上
      if (isFirstRow) {
        // 处理首行"同上"问题，若首行填写“同上”则视为未填写
        ditto.forEach((val, name) => {
          if (row[name] === val) {
            delete row[name] // 删除同上
          }
        })
        isFirstRow = false
      }

      row.verify = {}
      for (const rule in rules) {
        row.verify[rule] = validate(rules[rule], row[rule], row)
        if (!row.verify[rule]) {
          flag = false
        }
      }
    }

    // 删除空行
    for (const i in blankRowsIndex) {
      const index = blankRowsIndex[i]
      list.splice(index - i, 1)
    }

    if (!flag) {
      message = errorMsg
    }

    // 数据为空(全部空行的情况)
    if (list.length === 0) {
      flag = false
    }

    // 设置空行
    const _blankRow = {}
    ditto.forEach((value, key) => {
      _blankRow[key] = value
    })

    // 将列表数量填充回原来的数量。(若两个数据行中间有空行，则数据行会前移)
    for (let i = list.length; i < copyList.length; i++) {
      if (i === 0) {
        list.push({})
      } else {
        // 插入空行，能进入循环，则代表 blankRowsIndex 一定不为空
        list.push(lodash.cloneDeep(_blankRow))
      }
    }
  } else {
    flag = false
  }

  if (!flag) {
    ElMessage({ message, type: 'error' })
  }

  return { dealList: list, validResult: flag }
}

// 处理表格变色 el-table cell-class-name
export function wrongCellMask({ row, column }, tableRules) {
  if (!row) return
  const rules = getRules(tableRules)
  let flag = true
  if (row.verify && Object.keys(row.verify) && Object.keys(row.verify).length > 0) {
    if (row.verify[column.property] === false) {
      flag = validate(rules[column.property], row[column.property], row)
    }
    if (flag) {
      row.verify[column.property] = true
    }
  }
  return flag ? '' : 'mask-td'
}

// 校验
export function validate(rules, value, row = {}) {
  let flag = true
  // 判断是否存在校验
  if (isBlank(rules)) {
    return flag
  }
  for (const rule of rules) {
    const pattern = rule.pattern
    if (pattern && !pattern.test(value || '')) {
      flag = false
      break
    }
    const validator = rule.validator
    if (typeof validator === 'function') {
      const validatorResult = validator(value, row)
      if (validatorResult === false) {
        flag = false
        break
      }
    }
    const required = rule.required
    if (required === true && isBlank(value)) {
      flag = false
      break
    }
    const type = rule.type
    if (type && typeof value !== type) {
      flag = false
      break
    }
    const max = rule.max
    const min = rule.min
    if (typeof value === 'string' && ((min && min !== 0 && value.length < min) || (max && max !== 0 && max < value.length))) {
      flag = false
      break
    }
  }
  return flag
}

// 清理数据
export function cleanUpData(list, ditto = new Map()) {
  const copyList = lodash.cloneDeep(list)
  list.length = 0
  // 清空数组, 保留数组地址不变
  copyList.forEach((row, index) => {
    ditto.forEach((val, name) => {
      if (row[name] === val) {
        delete row[name] // 删除同上
      }
    })

    // delete rowCopy.verify // 删除验证字段
    const rowArr = obj2arr(row)
    const notBlankRow = rowArr.some((v) => isNotBlank(v))
    if (notBlankRow) {
      list.push(row)
    }
  })

  // 给同上赋值
  const prevAttr = new Map()
  list.forEach((v) => {
    delete v.verify
    ditto.forEach((val, key) => {
      if (isBlank(v[key])) {
        v[key] = prevAttr.get(key)
      } else {
        prevAttr.set(key, v[key])
      }
    })
  })
  return list
}

// 获取校验规则
function getRules(rules) {
  let _rules
  switch (rules.constructor.name) {
    case 'RefImpl':
    case 'ComputedRefImpl':
      _rules = rules.value
      break
    default:
      _rules = rules
      break
  }
  return _rules
}
