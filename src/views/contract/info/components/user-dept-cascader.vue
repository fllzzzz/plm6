<!-- 项目：部门人员列表 -->
<template>
  <el-cascader
    ref="userDeptRef"
    class="user-dept-cascader"
    v-model="copyValue"
    :placeholder="placeholder"
    :options="options"
    :props="cascaderProps"
    :show-all-levels="showAllLevels"
    :separator="separator"
    :clearable="clearable"
    :disabled="disabled"
    filterable
    :size="size"
    @change="handleChange"
  />
</template>

<script setup>
import { defineExpose, defineProps, defineEmits, computed, watch, ref } from 'vue'
import { isNotBlank, isBlank, deepClone, judgeSameValue } from '@data-type/index'

import useUserDeptTree from '@compos/store/use-user-dept-tree'

const emit = defineEmits(['change', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: [Array, Number, String],
    default: ''
  },
  // 启用 id
  noDisabledVal: {
    type: Array,
    default: () => []
  },
  // 部门id
  deptIds: {
    type: Array,
    default: () => []
  },
  // 大小
  size: {
    type: String,
    default: 'small'
  },
  // 是否可清除
  clearable: {
    type: Boolean,
    default: false
  },
  // 是否禁用
  disabled: {
    type: Boolean,
    default: false
  },
  // 输入框显示全路径
  showAllLevels: {
    type: Boolean,
    default: false
  },
  // 分隔符
  separator: {
    type: String,
    default: '/'
  },
  // 提示
  placeholder: {
    type: String,
    default: '请选择用户'
  },
  // 多选
  multiple: {
    type: Boolean,
    default: false
  },
  checkStrictly: {
    type: Boolean,
    default: false
  },
  // 返回结果全路径
  emitPath: {
    type: Boolean,
    default: false
  },
  // 额外的选项
  showExtra: {
    type: Boolean,
    default: false
  },
  extraOptionLabel: {
    type: String,
    default: '同上'
  },
  extraOptionValue: {
    type: [Number, String, Array, Boolean],
    default: -1
  }
})

const userDeptRef = ref()
const copyValue = ref()

const options = ref([])

const { userDeptTree } = useUserDeptTree()
const cascaderProps = computed(() => {
  return {
    value: 'id',
    label: 'label',
    children: 'children',
    expandTrigger: 'hover',
    emitPath: props.emitPath,
    multiple: props.multiple,
    checkStrictly: props.checkStrictly
  }
})

// 监听全局科目选项
watch(
  [userDeptTree, () => props.deptIds],
  ([list]) => {
    setOptions(list)
  },
  { deep: true, immediate: true }
)

watch(
  () => props.modelValue,
  (value) => {
    if (value instanceof Array) {
      copyValue.value = [...value]
    } else {
      copyValue.value = value
    }
    handleChange(value)
  },
  { immediate: true }
)

watch(
  () => props.noDisabledVal,
  () => {
    setNodeDisabled(options.value)
  },
  { immediate: true, deep: true }
)

// 发生change
function handleChange(val) {
  // 发生变化
  const isChange = !judgeSameValue(val, props.modelValue)
  // 两个值都为空
  const allBlank = isBlank(val) && isBlank(props.modelValue)

  if (isChange && !allBlank) {
    emit('update:modelValue', val)
    emit('change', val)
    return true
  }
  return false
}

// 设置级联数据
function setOptions(tree) {
  options.value = []
  try {
    if (tree) {
      // 过滤空部门
      options.value = filterBlankDept(deepClone(tree))
      // 加入额外的选项
      if (props.showExtra) {
        options.value.unshift({ id: props.extraOptionValue, label: props.extraOptionLabel })
      }
    }
  } catch (error) {
    console.log('获取人员部门树失败', error)
  }
}

// 过滤空部门
function filterBlankDept(tree) {
  return tree.filter((node) => {
    if (node.isUser) {
      node.disabled = true
    }
    if (isNotBlank(node.children)) {
      node.children.forEach((v) => {
        v.parentDeptId = node.id
        v.parentDeptName = node.label
      })
      node.children = filterBlankDept(node.children)
      return isNotBlank(node.children)
    }
    return node.isUser
  })
}

// 设置启用的节点
function setNodeDisabled(list) {
  list.forEach((v) => {
    v.disabled = v.isUser ? !props.noDisabledVal.includes(v.id) : v.disabled
    if (isNotBlank(v.children)) {
      setNodeDisabled(v.children)
    }
    return v
  })
}

// 单选 获取选中节点信息
function getNodeInfo() {
  const node = userDeptRef.value.getCheckedNodes(true)
  return node.length ? node[0].data : {}
}

// 获取选中子节点的顶级父节点
function getParentNode(node) {
  if (node.parent) {
    return getParentNode(node.parent)
  } else {
    return node
  }
}

defineExpose({
  getNodeInfo,
  getParentNode
})
</script>

<style lang="scss" scoped>
.user-dept-cascader {
  width: 220px;
}
</style>
