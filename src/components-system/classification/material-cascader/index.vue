<!-- 项目：级联列表（一级科目/二级科目/三级科目） -->
<template>
  <el-cascader
    ref="cascaderRef"
    class="classification-cascader"
    v-model="currentValue"
    :placeholder="placeholder"
    :options="classification.tree"
    :props="cascaderProps"
    :show-all-levels="showAllLevels"
    :separator="separator"
    :clearable="clearable"
    :disabled="disabled || !clsLoad"
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
import { defineExpose, defineProps, defineEmits, computed, watch, reactive, ref, nextTick } from 'vue'
import { useStore } from 'vuex'
import { mapGetters } from '@/store/lib'

import { isNotBlank, isBlank } from '@data-type/index'
import { getChildIds, getExistNodeIds, getFirstNode, removeNodeByExistIds } from '@/utils/data-type/tree'
import useMatClsList from '@/composables/store/use-mat-class-list'

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
  // 显示的科目
  classifyId: {
    type: Array
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
    default: '可搜索：科目名称、编号'
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
  },
  default: {
    type: Boolean,
    default: false
  }
})

const store = useStore()
const cascaderRef = ref()
const currentValue = ref()
const includeIds = ref()
const classification = reactive({
  tree: [],
  treeOrigin: []
})

const refreshLoading = ref(true)

const { matClsTree } = mapGetters('matClsTree')

const { loaded: clsLoad, rawMatClsKV } = useMatClsList()

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
  [() => matClsTree.value, () => props.basicClass, () => props.classifyId, clsLoad],
  ([list, basicClass, classifyId, clsLoad]) => {
    if (clsLoad) {
      // FIXME: 若A,B页面皆包含该组件（且页面切换时需要加载），会在数据量过多时，产生内存溢出的问题
      nextTick(() => {
        setCascader(list)
      })
    }
  },
  { deep: true, immediate: true }
)

watch(
  () => props.modelValue,
  (value) => {
    let curVal = Array.isArray(value) ? [...value] : value
    const defaultVal = setDefault(false)
    curVal = isBlank(value) ? defaultVal : curVal
    currentValue.value = curVal
    handleChange(curVal)
  },
  { immediate: true }
)

watch(
  [() => props.disabledVal, () => classification.tree],
  () => {
    if (isNotBlank(classification.tree)) {
      setNodeDisabled(classification.tree)
    }
  },
  { immediate: true, deep: true }
)

if (isBlank(matClsTree.value)) {
  store.dispatch('config/fetchMatClsTree')
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
  const codeMatch = data.serialNumber && data.serialNumber.substr(0, length) === keyword
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

      includeIds.value = getIncludeIds()
      // 待处理树，过滤分类
      let pendingTree = tree.filter((v) => !props.basicClass || v.basicClass & props.basicClass)
      // 格式转换
      pendingTree = dataFormat(pendingTree)
      if (includeIds.value) {
        // 过滤不包含的
        pendingTree = removeNodeByExistIds(pendingTree, includeIds.value)
      }
      classification.tree = pendingTree
      // // 加入额外的选项
      // if (props.showExtra) {
      //   classification.tree.unshift({ id: props.extraOptionValue, name: props.extraOptionLabel })
      // }
    } else {
      classification.treeOrigin = []
      classification.tree = []
    }
    if (currentValue.value) {
      const existIds = getExistNodeIds(classification.tree, currentValue.value)
      if (existIds?.length === 0) {
        handleChange(undefined)
      } else {
        // 数组
        if (Array.isArray(currentValue.value)) {
          if (currentValue.value.length !== existIds.length) {
            handleChange(existIds)
          }
        }
        // 非数组存在的情况，则不需要处理
      }
    } else {
      setDefault()
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
      const n = { id: node.id, name: node.name, serialNumber: node.serialNumber, disabled: false }
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

// 获取包含的节点
function getIncludeIds() {
  let classifyId = props.classifyId
  if (classifyId === undefined || classifyId === null) return null
  const includeIds = []

  if (!Array.isArray(classifyId)) {
    classifyId = [classifyId]
  }
  classifyId.forEach((id) => {
    const classifyInfo = rawMatClsKV.value[id]
    const fullPathId = classifyInfo.fullPathId
    includeIds.push.apply(includeIds, fullPathId)
    includeIds.push.apply(includeIds, getChildIds(classifyInfo.children))
  })
  return includeIds
}

// 设置默认值
function setDefault(triggerChange = true) {
  if (isBlank(props.modelValue)) {
    if (props.default) {
      const firstNode = getFirstNode(classification.tree)
      if (isNotBlank(firstNode)) {
        let defaultVal
        if (props.emitPath) {
          defaultVal = firstNode.map((node) => node.id)
        } else {
          defaultVal = firstNode[firstNode.length - 1].id
        }
        if (triggerChange) {
          handleChange(defaultVal)
        }
        return defaultVal
      }
    }
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
