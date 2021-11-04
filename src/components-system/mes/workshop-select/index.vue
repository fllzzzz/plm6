<!-- 车间:下拉选择框 -->
<template>
  <el-select
    v-model="selectValue"
    :size="size"
    :disabled="disabled"
    :multiple="multiple"
    :collapse-tags="collapseTags"
    :loading="loading"
    :clearable="clearable"
    filterable
    :placeholder="placeholder"
    @change="handleChange"
  >
    <el-option v-for="item in options" :key="item.value" :label="item.label" :value="item.value" />
  </el-select>
</template>

<script setup>
import { getWorkshopAllSimple as getAll } from '@/api/mes/common'
import { defineProps, defineEmits, ref, watch } from 'vue'
import { isNotBlank, isBlank } from '@data-type/index'

const emit = defineEmits(['change', 'update:value'])

const props = defineProps({
  value: {
    type: [Number, String],
    default: undefined
  },
  factoryId: {
    type: [Number, String],
    default: undefined
  },
  size: {
    type: String,
    default: 'small'
  },
  multiple: {
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
  collapseTags: {
    type: Boolean,
    default: false
  },
  default: {
    type: Boolean,
    default: false
  },
  placeholder: {
    type: String,
    default: '请选择车间'
  }
})

const loading = ref(false)
const selectValue = ref()
const options = ref([])

watch(
  () => props.value,
  (value) => {
    selectValue.value = value
    if (props.default && isBlank(value) && isNotBlank(options)) {
      selectValue.value = options.value[0].value
      handleChange(selectValue.value)
    }
  },
  { immediate: true }
)

watch(
  () => props.factoryId,
  (value) => {
    fetch()
  },
  { immediate: true }
)

function handleChange(val) {
  if (props.value !== val) {
    emit('update:value', val)
    emit('change', val)
  }
}

async function fetch() {
  options.value = []
  let _options = []
  try {
    loading.value = true
    const { content } = await getAll({ factoryId: props.factoryId })
    if (content && content.length > 0) {
      _options = content.map((o) => {
        return {
          value: o.id,
          label: o.name
        }
      })
    }
  } catch (error) {
    console.log('获取车间列表', error)
  } finally {
    options.value = _options
    if (isNotBlank(options.value) && props.default && !selectValue.value) {
      selectValue.value = options.value[0].value
    }
    handleChange(selectValue.value)
    loading.value = false
  }
}
</script>
