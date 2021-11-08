<!-- 工序：下拉选择框 -->
<template>
  <el-select
    v-model="selectIds"
    :size="size"
    :multiple="multiple"
    :collapse-tags="collapseTags"
    :loading="loading"
    :clearable="clearable"
    filterable
    placeholder="请选择工序"
    @change="handleChange"
  >
    <el-option-group v-for="group in processOptions" :key="group.name" :label="group.name">
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
import { getProcessAllSimple as getAllProcess } from '@/api/mes/common'
import { defineExpose, defineProps, defineEmits, computed, watch, ref } from 'vue'
import { processMaterialListTypeEnum as typeEnum } from '@enum-ms/mes'
import { isNotBlank } from '@data-type/index'
import RAF from '@/utils/raf'

const emit = defineEmits(['change', 'update:value'])

const props = defineProps({
  // 查询指定工序次序，不传查所有
  processType: {
    // Value
    type: [Number, Boolean],
    default: undefined
  },
  // 查询指定产品类型，不传查所有
  sequenceType: {
    // Value
    type: Number,
    default: undefined
  },
  // eslint-disable-next-line vue/require-default-prop
  value: {
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

const loading = ref(false)
const hasLoad = ref(false)
const selectIds = ref([])
const options = ref([])
const sourceData = ref()

const processOptions = computed(() => {
  return options.value.filter((v) => {
    if (v.originOptions && v.originOptions.length) {
      v.options = v.originOptions.filter((o) => {
        return isNotBlank(props.processType) ? props.processType === o.type : true
      })
    }
    return isNotBlank(props.sequenceType) ? props.sequenceType === v.type : true
  })
})

watch(
  () => props.value,
  (val) => {
    selectIds.value = val
    handleChange(val)
  },
  { immediate: true }
)

function handleChange(val) {
  emit('update:value', val)
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

async function fetchAllProcess() {
  options.value = []
  const _options = []
  try {
    loading.value = true
    const { content } = await getAllProcess()
    sourceData.value = JSON.parse(JSON.stringify(content))
    typeEnum.KEYS.forEach((type) => {
      const _optionObj = {}
      _optionObj.type = typeEnum[type].V
      _optionObj.name = typeEnum[type].L + '工序'
      _optionObj.options = content.filter((v) => {
        return v.sequenceType === typeEnum[type].V
      })
      _optionObj.originOptions = content.filter((v) => {
        return v.sequenceType === typeEnum[type].V
      })
      if (_optionObj.options && _optionObj.options.length) {
        _options.push(_optionObj)
      }
    })
  } catch (error) {
    console.log('获取工序', error)
  } finally {
    options.value = _options
    loading.value = false
    hasLoad.value = true
  }
}

fetchAllProcess()

defineExpose({
  getSourceData
})
</script>
