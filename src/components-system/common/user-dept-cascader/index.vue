<!-- 项目：部门人员列表 -->
<template>
  <el-cascader
    ref="userDeptCascaderRef"
    class="user-dept-cascader"
    v-model="copyValue"
    :placeholder="placeholder"
    :options="userDept.tree"
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
import { defineExpose, defineProps, defineEmits, computed, watch, reactive, ref } from 'vue'
import { useStore } from 'vuex'
import { mapGetters } from '@/store/lib'

import { isNotBlank, isBlank } from '@data-type/index'

const emit = defineEmits(['change', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: [Array, Number, String],
    default: ''
  },
  // 禁用值 id
  disabledVal: {
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
  extraOption: {
    type: Object,
    require: false
  }
})

const store = useStore()
const userDeptCascaderRef = ref()
const copyValue = ref()

const userDept = reactive({
  tree: [],
  treeOrigin: []
})

const refreshLoading = ref(true)

const { userDeptTree } = mapGetters('userDeptTree')

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
  [() => userDeptTree.value, () => props.deptIds],
  ([list]) => {
    setCascader(list)
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
  () => props.disabledVal,
  () => {
    setNodeDisabled(userDept.tree)
  },
  { immediate: true, deep: true }
)

if (isBlank(userDeptTree.value)) {
  store.dispatch('config/fetchUserDeptTree')
}

// 发生change
function handleChange(val) {
  // 发生变化
  let isChange = val !== props.modelValue
  if (val instanceof Array) {
    // 两个数组不相等
    isChange = !val.equals(props.modelValue)
  }
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
function setCascader(tree) {
  try {
    if (tree) {
      userDept.treeOrigin = tree

      // 过滤部门
      const origin = JSON.parse(JSON.stringify(tree))
      // origin = getFilterOptions(origin, props.deptIds)
      userDept.tree = dataFormat(origin)
      // 加入额外的选项
      if (props.extraOption) {
        userDept.tree.unshift(props.extraOption)
      }
    } else {
      userDept.treeOrigin = []
      userDept.tree = []
    }
  } catch (error) {
    console.log('获取人员部门树失败', error)
  } finally {
    refreshLoading.value = false
  }
}

// 过滤部门
// function getFilterOptions(options, ids, pollingTimes = 1) {
//   if (ids.length > 0 && ids.length < pollingTimes) {
//     return options
//   }
//   options = options.filter(node => {
//     if (node.id === -ids[pollingTimes - 1]) {
//       if (node.children && node.children.length > 0 && ids.length >= pollingTimes) {
//         node.children = getFilterOptions(node.children, ids, ++pollingTimes)
//       }
//       return true
//     }
//     return false
//   })
//   return options
// }

// 过滤空部门
function dataFormat(tree) {
  return tree.filter((node) => {
    if (isNotBlank(node.children)) {
      node.children = dataFormat(node.children)
      return isNotBlank(node.children)
    }
    return node.isUser
  })
}

// 设置禁用的节点
function setNodeDisabled(list) {
  list.forEach((v) => {
    if (props.disabledVal.includes(v.id)) {
      v.disabled = true
      v.initDisabled = true // 是否是起始禁用
    } else {
      v.disabled = false
      v.initDisabled = false
    }
    if (isNotBlank(v.children)) {
      setNodeDisabled(v.children)
    }
    return v
  })
}

// 单选 获取选中节点信息
function getNodeInfo() {
  const node = userDeptCascaderRef.value.getCheckedNodes(true)
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
