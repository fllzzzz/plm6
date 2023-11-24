<!-- 包单号：下拉选择框 -->
<template>
  <el-select
    v-model="selectValue"
    :size="size"
    :disabled="disabled"
    :multiple="multiple"
    :collapse-tags="collapseTags"
    :loading="loading"
    :clearable="clearable"
    :placeholder="placeholder"
    filterable
    @change="selectChange"
  >
    <el-option v-for="item in options" :key="item.value" :label="item.label" :value="item.value" />
  </el-select>
</template>

<script setup>
import { getAllPackage as getAll, getBridgeAllPackage } from '@/api/mes/common'
import { defineProps, watch, defineEmits, defineExpose, ref } from 'vue'
import { isNotBlank } from '@data-type'
import { packEnum } from '@enum-ms/ship-manage'

const emit = defineEmits(['update:modelValue', 'change'])
const props = defineProps({
  modelValue: {
    type: [Number, String]
  },
  projectId: {
    type: Number,
    default: undefined
  },
  packType: {
    type: Number,
    default: undefined
  },
  size: {
    type: String,
    default: 'small'
  },
  placeholder: {
    type: String,
    default: '请选择包单号'
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
  // 判断桥梁还是建钢
  typeVal: {
    type: Number
  }
})

const loading = ref(false)
const selectValue = ref()
const options = ref([])
const originOptions = ref([])

watch(
  () => props.modelValue,
  (val) => {
    selectValue.value = val
  },
  { immediate: true }
)

watch(selectValue, (val) => {
  selectChange(val)
})

watch(
  () => props.packType,
  (val) => {
    if (val) {
      options.value = originOptions.value.filter((v) => v.productType === val)
    }
  },
  { immediate: true }
)

function selectChange(val) {
  emit('update:modelValue', val)
  emit('change', val)
}

function getOptions() {
  return options.value
}

async function fetch() {
  options.value = []
  let _options = []
  loading.value = true
  try {
    const { content } = props.type === packEnum.BOX.V ? (await getBridgeAllPackage({ projectId: props.projectId })) : (await getAll({ projectId: props.projectId })) || []
    _options = content.map((o) => {
      return {
        value: o.id,
        label: `${o.serialNumber}`,
        productType: o.productType
      }
    })
  } catch (error) {
    console.log('获取包单号列表', error)
  } finally {
    originOptions.value = _options
    options.value = props.packType ? originOptions.value.filter((v) => v.productType === props.packType) : _options
    if (props.default && isNotBlank(options.value)) {
      selectValue.value = options.value[0].value
    } else {
      selectValue.value = undefined
    }
    selectChange(selectValue.value)
    loading.value = false
  }
}

fetch()

defineExpose({
  getOptions
})
</script>
