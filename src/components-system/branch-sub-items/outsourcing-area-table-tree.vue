<template>
  <table-tree
    v-model="copyValue"
    :options="tree"
    :loading="loading"
    :disabled="props.disabled"
    :data-structure="{ key: 'uid', label: 'name', children: 'children' }"
    @change="handleChange"
  />
</template>

<script setup>
import { defineProps, defineEmits, watch, ref } from 'vue'
import { deepClone, isBlank, isNotBlank } from '@data-type/index'
import tableTree from '@/components-system/common/table-tree/index.vue'
import { getAreaOutsourcingTree } from '@/api/branch-sub-items/common'
import { componentTypeEnum } from '@/utils/enum/modules/building-steel'

const emit = defineEmits(['change', 'update:modelValue', 'update:struc', 'update:encl'])

const props = defineProps({
  modelValue: {
    type: Array
  },
  struc: {
    // 构件区域
    type: Array
  },
  encl: {
    // 围护区域
    type: Array
  },
  showStruc: {
    // 显示构件
    type: Boolean,
    default: false
  },
  showEncl: {
    // 显示围护
    type: Boolean,
    default: false
  },
  projectIds: [Array, Number],
  disabled: {
    // 是否禁用
    type: Boolean,
    default: false
  }
})

const tree = ref([])
const sourceTree = ref([])
const loading = ref(false)
const copyValue = ref([])

watch(
  [() => props.struc, () => props.encl],
  ([struc, encl]) => {
    copyValue.value = formatVal(struc, encl)
  },
  { immediate: true, deep: true }
)

watch(
  () => props.projectIds,
  (val) => {
    fetchAreaOutsourcingTree(val)
  },
  { immediate: true }
)

watch([() => props.showStruc, () => props.showEncl], () => {
  tree.value = format(deepClone(sourceTree.value))
})

// 加载区域外包树
async function fetchAreaOutsourcingTree(projectIds) {
  if (isBlank(projectIds)) {
    tree.value = []
    return
  }
  loading.value = true
  try {
    const _tree = await getAreaOutsourcingTree(projectIds)
    sourceTree.value = _tree
    tree.value = format(deepClone(_tree))
  } catch (error) {
    console.log('外包区域', error)
  } finally {
    loading.value = false
  }
}

// 格式化
function format(tree = []) {
  return tree
    .filter((v) => {
      const f1 = !v.type
      const f2 = props.showStruc && v.type && v.type === componentTypeEnum.STRUCTURE.V
      const f3 = props.showEncl && v.type && v.type === componentTypeEnum.ENCLOSURE.V
      return f1 || f2 || f3
    })
    .map((v) => {
      v.uid = v.type ? `${v.type}_${v.id}` : v.id
      if (isNotBlank(v.children)) {
        v.children = format(v.children)
      }
      return v
    })
}

function formatVal(struc = [], encl = []) {
  const arr = []
  struc.forEach((v) => {
    arr.push(`${componentTypeEnum.STRUCTURE.V}_${v}`)
  })
  encl.forEach((v) => {
    arr.push(`${componentTypeEnum.ENCLOSURE.V}_${v}`)
  })
  return arr
}

function handleChange(values) {
  const struc = []
  const encl = []
  const selectMenus = Array.from(values)
    .map((v) => typeof v === 'string' ? v.split('_') : v)
    .filter((v) => v.length === 2)
  selectMenus.forEach((v) => {
    if (+v[0] === componentTypeEnum.STRUCTURE.V) {
      struc.push(v[1])
    }
    if (+v[0] === componentTypeEnum.ENCLOSURE.V) {
      encl.push(v[1])
    }
  })
  emit('change', selectMenus)
  emit('update:modelValue', selectMenus)
  emit('update:encl', encl)
  emit('update:struc', struc)
}
</script>
