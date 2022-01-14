<!-- 模块类型 -->
<template>
  <el-cascader
    ref="tableTemplateCascaderRef"
    v-model="c_value"
    :options="options"
    :props="cascaderProps"
    :filterable="props.filterable"
    :clearable="props.clearable"
    :disabled="props.disabled"
    :show-all-levels="props.showAllLevels"
    :placeholder="props.placeholder"
    :size="props.size"
  />
</template>

<script setup>
import { ref, watch, computed, defineProps, defineEmits, nextTick } from 'vue'
import { getByCondition } from '@/api/config/system-config/table-print-template'

import { tableType } from '@/utils/print/table/type'

import printTemplate from '@/utils/print/default-template'
import { isBlank, isNotBlank, judgeSameValue } from '@data-type/index'

const emit = defineEmits(['update:value', 'change', 'onload'])

const props = defineProps({
  value: {
    type: [Array, Number, String],
    default: undefined
  },
  cacheTemplateId: {
    type: String,
    default: undefined
  },
  tableTypes: {
    type: Array,
    default: () => []
  },
  initial: {
    type: Boolean,
    default: false
  },
  size: {
    type: String,
    default: 'small'
  },
  checkStrictly: { // 启用该功能后，可让父子节点取消关联，选择任意一级选项。
    type: Boolean,
    default: false
  },
  expandTrigger: { // 次级菜单的展开方式
    type: String,
    default: 'hover'
  },
  filterable: {
    type: Boolean,
    default: false
  },
  clearable: {
    type: Boolean,
    default: false
  },
  disabled: {
    type: Boolean,
    default: false
  },
  showAllLevels: {
    type: Boolean,
    default: false
  },
  default: {
    type: Boolean,
    default: false
  },
  placeholder: {
    type: String,
    default: '请选择表格模板'
  }
})

const options = ref([])
const dataSource = ref()
const loading = ref(false)
const c_value = ref(undefined)
const tableTemplateCascaderRef = ref()

const cascaderProps = computed(() => {
  return { value: 'id', label: 'name', children: 'children', checkStrictly: props.checkStrictly, expandTrigger: props.expandTrigger, emitPath: false }
})

watch(
  () => props.value,
  (value) => {
    c_value.value = value
  },
  { immediate: true }
)

watch(
  () => c_value.value,
  (value) => {
    handleChange(value)
  }
)

watch(
  () => props.cacheTemplateId,
  (value) => {
    const flag = isNotBlank(dataSource.value) && isNotBlank(props.cacheTemplateId) && dataSource.value.some(v => v.id === props.cacheTemplateId)
    if (flag) {
      c_value.value = props.cacheTemplateId
    }
  },
  { immediate: true }
)

watch(
  () => props.tableTypes,
  (value, oldValue) => {
    if (!judgeSameValue(value, oldValue)) {
      fetch()
    }
  },
  { immediate: true, deep: true }
)

function handleChange(val) {
  nextTick(() => {
    if (!(isBlank(val) && isBlank(props.value)) && val !== props.value) {
      const node = tableTemplateCascaderRef.value.getCheckedNodes(false)
      emit('update:value', val)
      emit('change', node[0]['data']['config'])
    }
  })
}
async function fetch() {
  loading.value = true
  const _options = [] // 初始化options
  let dataSource = [] // 初始化数据源
  const types = props.tableTypes
  const typeMap = {}
  let defaultValue // 默认模板，存在在则指定平台模板为默认模板
  let hasValue // value存在且value在option中
  let hasCacheValue // 是否有缓存的templateId
  try {
    if (isNotBlank(types)) {
      types.forEach(v => {
        typeMap[v] = []
      })
      const { content = [] } = await getByCondition({ types }) || {}
      dataSource = content
      content.forEach(v => {
        typeMap[v.type].push({
          id: v.id,
          name: v.name,
          config: v.config
        })
        if (v.isDefault && !defaultValue) {
          defaultValue = v.id
        }
      })
      hasValue = isNotBlank(props.value) && content.some(v => v.id === props.value)
    }
  } catch (error) {
    console.log('模板列表', error)
  } finally {
    for (const k in typeMap) {
      if (isNotBlank(k) && isNotBlank(printTemplate[k])) {
        typeMap[k].unshift({
          id: k,
          name: printTemplate[k].name,
          config: printTemplate[k]
        })
        defaultValue = defaultValue || k
      }
      if (tableType[k]) {
        _options.push({
          id: `-${tableType[k].V}`,
          name: tableType[k].L,
          key: k,
          children: typeMap[k]
        })
      }
    }
    // 按key排序
    _options.sort((a, b) => {
      const aIndex = props.tableTypes.indexOf(a.key)
      const bIndex = props.tableTypes.indexOf(b.key)
      return aIndex - bIndex
    })
    hasCacheValue = isNotBlank(props.cacheTemplateId) && dataSource.some(v => v.id === props.cacheTemplateId)
    if (props.default && !hasValue) {
      if (hasCacheValue) {
        c_value.value = props.cacheTemplateId
      } else {
        c_value.value = defaultValue
      }
    }
    dataSource.value = dataSource
    options.value = _options
    loading.value = false
    emit('onload')
  }
}
</script>
