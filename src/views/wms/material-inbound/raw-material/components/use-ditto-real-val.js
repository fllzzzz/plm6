import { isBlank, isNotBlank } from '@/utils/data-type'
import { ref } from 'vue'

export default function useDittoRealVal(dittoKey, dittoVal = -1) {
  const scopeList = ref([])
  return {
    scopeList,
    getNotDittoArr: () => scopeList.value.map((v) => v.start),
    initScopeList: (list, customMethod) => initScopeList(list, customMethod, { scopeList, dittoKey, dittoVal }),
    handleValueChange: (val, index) => handleValueChange(val, index, { scopeList, dittoVal }),
    getRealVal: (index) => getRealVal(index, { scopeList })
  }
}

/**
 * 初始化范围列表
 * @param {*} list 需要初始化的列表
 * @param {*} customMethod 自定义方法
 */
function initScopeList(list, customMethod, { scopeList, dittoKey, dittoVal }) {
  if (typeof customMethod === 'function') {
    customMethod({ scopeList, dittoKey, dittoVal })
  }
  scopeList.value = []
  if (isBlank(list)) return
  let start = 0
  let end = -1
  let val
  for (let i = 0; i < list.length; i++) {
    const currentVal = list[i][dittoKey]
    if (isNotBlank(dittoVal) && currentVal === dittoVal) {
      // 为空或于val相等
      ++end
    } else {
      if (end !== -1) scopeList.value.push({ start, end, val })
      start = i
      end = i
      val = currentVal
    }
  }
  scopeList.value.push({ start, end, val })
}

// 行发生变化
function handleValueChange(val, index, { scopeList, dittoVal }) {
  const di = scopeList.value.findIndex((v) => index >= v.start && index <= v.end)
  const currentScope = scopeList.value[di]
  if (isBlank(currentScope)) return undefined
  if (isNotBlank(dittoVal) && val !== dittoVal) {
    // 选中的值不为“同上”
    if (index === currentScope.start) {
      currentScope.val = val // 更新原节点val
    } else {
      const newScope = {
        start: index,
        end: currentScope.end,
        val: val
      }
      currentScope.end = index - 1 // 更新原节点end
      scopeList.value.splice(di + 1, 0, newScope)
    }
  } else {
    // 选中的值为“同上”
    const prevDI = di - 1
    if (index === currentScope.start) {
      // 与前一个节点范围合并
      if (prevDI > -1) {
        const prevScope = scopeList.value[prevDI]
        prevScope.end = currentScope.end
        prevScope.val = undefined
        scopeList.value.splice(di, 1) // 删除当前节点
      } else {
        // 不存在前一个节点
        currentScope.val = undefined
      }
    }
  }
}

// 获取真实的值
function getRealVal(index, { scopeList }) {
  for (const v of scopeList.value) {
    if (index >= v.start && index <= v.end) {
      return v.val
    }
  }
  return undefined
}
