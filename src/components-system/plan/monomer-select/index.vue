<!-- 单体:下拉选择框 -->
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
    :no-data-text="projectId ? '无数据': '未选择项目'"
    @change="selectChange"
  >
    <el-option
      v-if="showAll"
      label="全部单体"
      :value="undefined"
    />
    <el-option
      v-for="item in options"
      :key="item.value"
      :label="item.label"
      :value="item.value"
    />
  </el-select>
</template>

<script setup>
import { ref, defineProps, defineEmits, watch, defineExpose } from 'vue'
import { monomerAll as getAll } from '@/api/plan/monomer'
import { isNotBlank } from '@data-type/index'
const emit = defineEmits(['change', 'update:modelValue', 'getAreaInfo'])

const loading = ref(false)
const selectValue = ref()
const options = ref([])
const originOptions = ref([])
const props = defineProps({
  projectId: {
    type: [Number, String]
  },
  modelValue: {
    type: [Number, String]
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
  showAll: {
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
    default: true
  },
  placeholder: {
    type: String,
    default: '请选择单体'
  }
})

watch(
  () => props.projectId,
  (val) => {
    if (val) {
      fetchData()
    }
  },
  { deep: true, immediate: true }
)

watch(
  () => props.modelValue,
  (val) => {
    if (val) {
      selectValue.value = val
    }
  },
  { immediate: true }
)

watch(
  () => selectValue.value,
  (val) => {
    selectChange(val)
  }
)

fetchData()

async function fetchData() {
  options.value = []
  originOptions.value = []
  if (!props.projectId) {
    return
  }
  let optionData = []
  loading.value = true
  try {
    const { content = [] } = await getAll(props.projectId) || {}
    originOptions.value = content || []
    optionData = content.map(o => {
      return {
        value: o.id,
        label: o.name
      }
    })
  } catch (error) {
    console.log('获取单体列表', error)
  } finally {
    options.value = optionData
    if (props.default && isNotBlank(optionData)) {
      selectValue.value = optionData[0].value
      selectChange(selectValue.value)
    } else {
      selectValue.value = undefined
    }
    loading.value = false
  }
}

// 获取单体信息
function getOption(val) {
  return originOptions.value.find(k => k.id === val)
}

function selectChange(val) {
  let monomerVal = {}
  if (!val) {
    val = undefined
    monomerVal = {}
  } else {
    monomerVal = originOptions.value.find(k => k.id === val)
  }
  const areaInfo = monomerVal && monomerVal.areaSimpleList ? monomerVal.areaSimpleList : []
  emit('update:modelValue', val)
  emit('change', val)
  emit('getAreaInfo', areaInfo)
}

defineExpose({
  getOption
})

</script>
