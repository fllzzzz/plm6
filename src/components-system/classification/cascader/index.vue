<!-- 项目：级联列表（一级科目/二级科目/三级科目） -->
<template>
  <el-cascader
    ref="cascaderRef"
    class="classification-cascader"
    v-model="copyValue"
    :placeholder="placeholder"
    :options="classification.tree"
    :props="cascaderProps"
    :show-all-levels="showAllLevels"
    :separator="separator"
    :clearable="clearable"
    :disabled="disabled"
    :collapse-tags="collapseTags"
    filterable
    :size="size"
    :filter-method="filterMethod"
    @change="handleChange"
  >
    <template v-slot="{ data }">
      <span>{{ data.name }}</span>
      <span v-if="showSubjectCode" style="float: right; color: #8492a6; font-size: 13px; padding-left: 6px">
        {{ data.serialNumber }}
      </span>
    </template>
  </el-cascader>
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
  // 科目材料类型
  basicClass: {
    type: Number,
    default: undefined
  },
  // 显示科目编码
  showSubjectCode: {
    type: Boolean,
    default: true
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
  // 多选折叠
  collapseTags: {
    type: Boolean,
    default: true
  },
  // 提示
  placeholder: {
    type: String,
    default: '可搜索：科目名称、编码'
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
  deep: {
    type: Number,
    default: undefined
  },
  onlyShowCurrentDeep: {
    type: Boolean,
    default: false
  },
  showExtra: {
    type: Boolean,
    default: false
  },
  // 额外的选项
  extraOptionLabel: {
    type: String,
    default: '同上'
  },
  extraOptionValue: {
    type: [Number, String, Array, Boolean],
    default: -1
  }
})

const store = useStore()
const cascaderRef = ref()
const copyValue = ref()

const classification = reactive({
  tree: [],
  treeOrigin: []
})

const refreshLoading = ref(true)

const { clsTree } = mapGetters('clsTree')

const cascaderProps = computed(() => {
  return {
    value: 'id',
    label: 'name',
    children: 'children',
    expandTrigger: 'hover',
    emitPath: props.emitPath,
    multiple: props.multiple,
    checkStrictly: props.checkStrictly
  }
})

// 监听全局科目选项
watch(
  [() => clsTree.value, () => props.basicClass],
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
    setNodeDisabled(classification.tree)
  },
  { immediate: true, deep: true }
)

if (isBlank(clsTree.value)) {
  store.dispatch('config/fetchClassificationTree')
}

// 搜索匹配，code从开始位置匹配
function filterMethod(node, keyword) {
  const { data, pathLabels } = node
  const length = keyword.length
  if (length === 0) return true
  // 名称匹配
  const nameMatch = data.name.includes(keyword)
  //
  const pathLabelsMatch = pathLabels.includes(keyword)
  // 编码匹配
  const codeMatch = data.code && data.code.substr(0, length) === keyword
  return nameMatch || pathLabelsMatch || codeMatch
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
      classification.treeOrigin = tree

      // 根据基础材料类型 筛选一级科目
      classification.tree = dataFormat(tree.filter((v) => !props.basicClass || v.basicClass === props.basicClass))
      // 加入额外的选项
      if (props.showExtra) {
        classification.tree.unshift({ id: props.extraOptionValue, name: props.extraOptionLabel })
      }
    } else {
      classification.treeOrigin = []
      classification.tree = []
    }
  } catch (error) {
    console.log('获取科目级联列表失败', error)
  } finally {
    refreshLoading.value = false
  }
}

// 格式转换
function dataFormat(tree, deep = 1) {
  if (isBlank(props.deep) || deep <= props.deep) {
    const t = tree.map((node) => {
      let maxDeep = 1
      const n = { id: node.id, name: node.name, disabled: false }
      // 显示层级
      if (isNotBlank(node.children)) {
        const children = dataFormat(node.children, deep + 1)
        if (isNotBlank(children)) {
          maxDeep++
          n.children = children
        }
      }
      n.maxDeep = maxDeep
      return n
    })
    if (deep === 1 && props.onlyShowCurrentDeep) {
      return t.filter((n) => {
        return n.maxDeep >= props.deep
      })
    } else {
      return t
    }
  } else {
    return []
  }
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
  const node = cascaderRef.value.getCheckedNodes(true)
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
.classification-cascader {
  width: 220px;
}
</style>
