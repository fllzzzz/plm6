<!-- 表格类型:下拉选择框 -->
<template>
  <el-select
    v-model="c_value"
    :size="props.size"
    :disabled="props.disabled"
    :loading="loading"
    :clearable="props.clearable"
    :filterable="props.filterable"
    :placeholder="props.placeholder"
    @change="selectChange"
  >
    <template v-for="item in options">
      <el-option
        v-if="unshowOptions.indexOf(item[selectProps.key]) === -1"
        :key="item[selectProps.key]"
        :label="item[selectProps.label]"
        :value="item[selectProps.value]"
      />
    </template>
  </el-select>
</template>

<script setup>
import { ref, watch, defineProps, defineEmits } from 'vue'
import { getByType } from '@/api/config/system-config/table-print-template'

import printTemplate from '@/utils/print/default-template'
import { isBlank, isNotBlank } from '@data-type/index'

const emit = defineEmits(['update:value', 'change', 'onload'])

const props = defineProps({
  value: {
    type: [Number, String],
    default: undefined
  },
  tableType: {
    type: [Number, String],
    default: undefined
  },
  cacheTemplateId: {
    type: [Number, String],
    default: undefined
  },
  size: {
    type: String,
    default: 'small'
  },
  clearable: {
    type: Boolean,
    default: false
  },
  filterable: {
    type: Boolean,
    default: false
  },
  disabled: {
    type: Boolean,
    default: false
  },
  showAll: {
    type: Boolean,
    default: false
  },
  default: {
    type: Boolean,
    default: false
  },
  unshowOptions: { // value
    type: Array,
    default: () => []
  },
  placeholder: {
    type: String,
    default: '请选择表格模板'
  },
  allLabelText: {
    type: String,
    default: '全部'
  }
})

const options = ref([])
const loading = ref(false)
const c_value = ref(undefined)
const p_value = ref(undefined) // 传入的value，避免value不传时，change出现问题
const selectProps = ref({ key: 'id', label: 'name', value: 'id' })

watch(
  () => props.tableType,
  () => {
    fetch()
  },
  { immediate: true }
)

watch(
  () => props.value,
  (value) => {
    c_value.value = p_value.value = value
  },
  { immediate: true }
)

watch(
  () => props.cacheTemplateId,
  (value) => {
    const flag = isNotBlank(options.value) && isNotBlank(props.cacheTemplateId) && options.value.some(v => v.id === props.cacheTemplateId)
    if (flag) {
      c_value.value = props.cacheTemplateId
    }
  },
  { immediate: true }
)

watch(
  () => c_value.value,
  (value) => {
    selectChange(value)
  }
)

function initVal() {
  options.value = []
}

function selectChange(val) {
  if (!(isBlank(val) && isBlank(p_value.value)) && val !== p_value.value) {
    p_value.value = val
    emit('update:value', val)
    emit('change', getItem(val))
  }
}

function getItem(val) {
  if (isBlank(options.value)) {
    return
  }
  for (const item of options.value) {
    if (item[selectProps.value.value] === val) {
      return item.config
    }
  }
  return
}

// eslint-disable-next-line no-unused-vars
function getOptions() {
  return options.value
}

async function fetch() {
  initVal()
  if (isBlank(props.tableType)) {
    c_value.value = undefined
    return
  }
  loading.value = true
  let defaultValue // 默认模板，存在在则指定平台模板为默认模板
  let hasValue // value存在且value在option中
  let hasCacheValue // 是否有缓存的templateId
  try {
    const { content } = await getByType(props.tableType)
    options.value = content.map(v => {
      if (v.config) {
        v.config = JSON.parse(v.config)
      }
      if (v.isDefault) {
        defaultValue = v.id
      }
      return v
    })
    hasValue = isNotBlank(p_value.value) && options.value.some(v => v.id === p_value.value)
  } catch (error) {
    console.log('表格模板', error)
  } finally {
    const typeKey = props.tableType
    if (isNotBlank(typeKey) && isNotBlank(printTemplate[typeKey])) {
      options.value.unshift({
        id: typeKey,
        name: printTemplate[typeKey].name,
        config: printTemplate[typeKey]
      })
      defaultValue = defaultValue || typeKey
    }
    hasCacheValue = isNotBlank(props.cacheTemplateId) && options.value.some(v => v.id === props.cacheTemplateId)
    if (props.default && !hasValue) {
      if (hasCacheValue) {
        c_value.value = props.cacheTemplateId
      } else {
        c_value.value = defaultValue
      }
    }
    if (isBlank(options.value)) {
      // 不在方法头部处理是为了避免触发c_value 的change
      c_value.value = undefined
    }
    loading.value = false
    emit('onload')
  }
}
</script>
