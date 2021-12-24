<!-- 工序：下拉选择框 -->
<template>
  <el-select
    v-model="selectIds"
    :size="size"
    :multiple="multiple"
    :collapse-tags="collapseTags"
    :loading="!loaded"
    :clearable="clearable"
    filterable
    placeholder="请选择工序"
    @change="handleChange"
  >
    <el-option-group v-for="group in processOptions" :key="group.type" :label="group.name">
      <el-option
        v-for="item in group.options"
        :key="item.id"
        :label="item.name"
        :value="item.id"
        :disabled="disabledValue ? disabledValue.indexOf(item.id) > -1 : false"
      />
    </el-option-group>
  </el-select>
</template>

<script setup>
import { defineExpose, defineProps, defineEmits, computed, watch, ref } from 'vue'
import { componentTypeEnum as typeEnum } from '@enum-ms/mes'
import { isNotBlank } from '@data-type/index'
import RAF from '@/utils/raf'

import useProcess from '@compos/store/use-process'

const emit = defineEmits(['change', 'update:modelValue'])

const props = defineProps({
  // 查询指定工序次序，不传查所有
  productType: {
    // Value
    type: [Number, Boolean],
    default: undefined
  },
  // eslint-disable-next-line vue/require-default-prop
  modelValue: {
    type: [Number, Array]
  },
  multiple: {
    type: Boolean,
    default: true
  },
  collapseTags: {
    type: Boolean,
    default: true
  },
  clearable: {
    type: Boolean,
    default: false
  },
  size: {
    type: String,
    default: 'small'
  },
  disabledValue: {
    type: Array,
    default: () => []
  }
})

const hasLoad = ref(false)
const selectIds = ref([])
const options = ref([])
const sourceData = ref()

const { loaded, process } = useProcess()

const processOptions = computed(() => {
  return options.value.filter((v) => {
    if (v.originOptions && v.originOptions.length) {
      v.options = v.originOptions.filter((o) => {
        return isNotBlank(props.productType) ? props.productType === o.productType : true
      })
    }
    return isNotBlank(props.productType) ? props.productType === v.type : true
  })
})

watch(
  () => props.modelValue,
  (val) => {
    selectIds.value = val
    handleChange(val)
  },
  { immediate: true }
)

watch(
  process,
  (list) => {
    dataFormat()
  },
  { immediate: true, deep: true }
)

function handleChange(val) {
  emit('update:modelValue', val)
  emit('change', val)
}

async function getSourceData() {
  if (!hasLoad.value) {
    await waitLoad()
  }
  return sourceData
}

async function waitLoad() {
  return new Promise((resolve, reject) => {
    RAF.setInterval(() => {
      if (hasLoad.value) {
        RAF.clearInterval()
        resolve()
      }
    }, 100)
  })
}

function dataFormat() {
  options.value = []
  const _options = []
  try {
    sourceData.value = JSON.parse(JSON.stringify(process.value))
    typeEnum.KEYS.forEach((type) => {
      const _optionObj = {}
      _optionObj.type = typeEnum[type].V
      _optionObj.name = typeEnum[type].L + '工序'
      _optionObj.options = process.value.filter((v) => {
        return v.productType === typeEnum[type].V
      })
      _optionObj.originOptions = process.value.filter((v) => {
        return v.productType === typeEnum[type].V
      })
      if (_optionObj.options && _optionObj.options.length) {
        _options.push(_optionObj)
      }
    })
  } catch (error) {
    console.log('获取工序', error)
  } finally {
    options.value = _options
    hasLoad.value = true
  }
}

defineExpose({
  getSourceData
})
</script>
