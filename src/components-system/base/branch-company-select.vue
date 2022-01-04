<!-- 分公司:下拉选择框 -->
<template>
  <common-select
    v-model="selectValue"
    :loading="loading"
    :options="options"
    :type="'other'"
    :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
    :size="size"
    :clearable="clearable"
    :default="props.default"
    :disabled="props.disabled"
    filterable
    :placeholder="placeholder"
    style="width: 200px"
    @change="selectChange"
  />
</template>

<script setup>
import { ref, defineProps, defineEmits, watch } from 'vue'
import { getBranchCompanyAllSimple as getAll } from '@/api/contract/project'

const emit = defineEmits(['update:modelValue', 'change'])
const props = defineProps({
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

watch(
  () => props.modelValue,
  (val) => {
    selectValue.value = val
  },
  { immediate: true }
)

function selectChange(val) {
  emit('update:modelValue', val)
  emit('change', val)
}

fetch()

// TODO: 改！设置进缓存
async function fetch() {
  let _options = []
  options.value = []
  try {
    loading.value = true
    const { content } = await getAll()
    if (content && content.length > 0) {
      _options = content
    }
  } catch (error) {
    console.log('获取分公司列表', error)
  } finally {
    options.value = _options
    loading.value = false
  }
}
</script>
