<template>
  <div v-loading="props.loading" class="table-tree">
    <div :key="key" v-if="isNotBlank(copyOptions)">
      <table-item v-for="item in copyOptions" :key="item.key" :item="item" />
    </div>
    <span class="tip" v-else>暂无数据</span>
  </div>
</template>

<script setup>
import { isNotBlank } from '@/utils/data-type'
import { defineEmits, defineProps, ref, watch, provide } from 'vue'
import TableItem from './TableItem.vue'
import lodash from 'lodash'

const emit = defineEmits(['update:modelValue', 'change'])

const props = defineProps({
  options: {
    // 数据源
    type: Array,
    default: () => []
  },
  modelValue: {
    type: Array,
    default: () => []
  },
  checkStrictly: {
    // 是否严格的遵守父子节点不互相关联
    type: Boolean,
    default: false
  },
  checkable: {
    // 选择模式
    type: Boolean,
    default: false
  },
  onlyShowChecked: {
    // 只显示选中的数据
    type: Boolean,
    default: false
  },
  returnLeaf: {
    // 只返回叶子节点的值
    type: Boolean,
    default: true
  },
  disabled: {
    // 是否禁用
    type: Boolean,
    default: false
  },
  loading: {
    // 加载中
    type: Boolean,
    default: false
  },
  dataStructure: {
    // 数据结构
    type: Object,
    default: () => {
      return { key: 'id', label: 'label', children: 'children' }
    }
  }
})

const checkedSet = ref()

provide('checkedSet', checkedSet)
provide('checkStrictly', props.checkStrictly)
provide('returnLeaf', props.returnLeaf)
provide('disabled', props.disabled)
provide('checkable', props.checkable)
provide('onlyShowChecked', props.onlyShowChecked)

const copyOptions = ref([])
const key = ref(1)

watch(
  () => props.options,
  (tree) => {
    if (!props.loading) { // 传入options，且options加载中时，不触发（即接口情况下，第一次加载不触发，即option为undefined时）
      key.value += 1
      copyOptions.value = format(tree)
    }
  },
  { immediate: true, deep: true }
)

watch(
  () => props.modelValue,
  (val) => {
    checkedSet.value = new Set(val)
  },
  { immediate: true, deep: true }
)

watch(
  checkedSet,
  () => {
    handleChange()
  },
  { deep: true }
)

// 格式化
function format(tree) {
  // options中不存在的选项
  const surplusSet = lodash.cloneDeep(checkedSet.value)
  const opt = treeFormat(tree, null, surplusSet)
  // 删除options中不存在的选项
  surplusSet.forEach((val) => {
    checkedSet.value.delete(val)
  })
  return opt
}

function treeFormat(tree = [], parent, surplusSet) {
  return tree.map((n) => {
    const checked = checkedSet.value.has(n[props.dataStructure.key])
    if (checked) { // surplusSet删除存在的key
      surplusSet.delete(n[props.dataStructure.key])
    }
    const node = {
      parent: parent,
      key: n[props.dataStructure.key],
      label: n[props.dataStructure.label],
      checked: checked,
      semiChecked: false,
      children: undefined
    }
    if (isNotBlank(n[props.dataStructure.children])) {
      node.children = treeFormat(n.children, node, surplusSet)
      // 此处会通过子节点更新父节点的值。在node的key可能重复时，避免勾选状态错误
      node.semiChecked = node.children.some((v) => v.semiChecked || v.checked)
      node.checked = node.children.every((v) => v.checked)
    }
    return node
  })
}

function handleChange() {
  const res = Array.from(checkedSet.value)
  if (res.equals(props.modelValue)) return
  emit('update:modelValue', res)
  emit('change', res)
}
</script>

<style lang="scss" scoped>
.table-tree {
  min-height: 60px;
  border: 1px solid #e5e5e5;
  // border-bottom: 0;
  .tip {
    display: inline-block;
    line-height: 60px;
    width: 100%;
    text-align: center;
    color: gray;
  }
}
</style>
