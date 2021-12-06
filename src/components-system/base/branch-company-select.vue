<!-- 分公司:下拉选择框 -->
<template>
  <common-select
    v-model="selectValue"
    :options="options"
    :type="'other'"
    :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
    :size="size"
    clearable
    filterable
    :placeholder="placeholder"
    style="width:200px"
    @change="selectChange"
  />
</template>

<script setup>
import { ref, defineProps, defineEmits, watch, computed } from 'vue'
import { getBranchCompanyAllSimple as getAll } from '@/api/contract/project'

const emit = defineEmits(['update:modelValue', 'change'])
const props=defineProps({
  modelValue: {
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
  showAll: {
    type: Boolean,
    default: false
  },
  labelName: {
    type: String,
    default: '全部分公司'
  },
  default: {
    type: Boolean,
    default: false
  },
  placeholder: {
    type: String,
    default: '请选择公司主体'
  }
})
const loading = ref(false)
const selectValue = ref()
const options = ref([])
const defaultValue = ref() 

watch(
  () => props.modelValue,
  (val) => {
    if (!val && val !== 0) {
      if (props.default && (defaultValue.value || defaultValue.value === 0)) {
        selectValue.value = defaultValue.value
        selectChange(defaultValue.value)
      } else {
        selectValue.value = undefined
      }
    } else {
      selectValue.value = val
    }
  },
  { immediate: true }
)

function initVal() {
  selectValue.value = undefined
  defaultValue.value = undefined
  options.value = []
}
function selectChange(val) {
  emit('update:modelValue', val)
  emit('change', val)
}
function getOptions() {
  return options.value
}

fetch()

async function fetch() {
  let _options = []
  initVal()
  try {
    loading.value = true
    const { content } = await getAll()
    if (content && content.length > 0) {
      _options = content
      if (props.default) {
        selectValue.value = _options[0].id
        defaultValue.value = _options[0].id
      } else {
        selectValue.value = props.modelValue
      }
    }
  } catch (error) {
    console.log('获取分公司列表', error)
  } finally {
    options.value = _options
    selectChange(selectValue.value)
    loading.value = false
  }
}
</script>
