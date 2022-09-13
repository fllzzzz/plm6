<!-- 单体:下拉选择框 -->
<template>
  <div style="display:inline-block;">
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
      :no-data-text="projectId ? '无数据' : '未选择项目'"
      @change="selectChange"
    >
      <el-option v-if="showAll" label="全部单体" :value="undefined" />
      <el-option v-for="item in options" :key="item.value" :label="item.label" :value="item.value" />
    </el-select>
    <span v-if="showTips" style="font-size:12px;color:red;margin-left:10px;">*当前单体下未创建区域</span>
  </div>

</template>

<script setup>
import { ref, defineProps, defineEmits, watch, defineExpose } from 'vue'
import { monomerAll as getAll } from '@/api/plan/monomer'
import { isNotBlank } from '@data-type/index'
const emit = defineEmits(['change', 'update:modelValue', 'getAreaInfo', 'getCurrentInfo'])

const loading = ref(false)
const selectValue = ref()
const options = ref([])
const originOptions = ref([])
const props = defineProps({
  projectId: {
    type: [Number, String]
  },
  modelValue: {
    type: [Number, String, Array]
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
  defaultValue: {
    type: [Number, String, Array],
    default: undefined
  },
  placeholder: {
    type: String,
    default: '请选择单体'
  },
  productType: {
    type: [Number, String], // 产品类型
    default: undefined
  },
  mainProductType: {
    type: [Number, String], // 产品分类 2构件 4围护 8配套件
    default: undefined
  },
  filterArea: {
    type: Boolean,
    default: true
  },
  showTips: {
    type: Boolean,
    default: false
  }
})

watch(
  () => props.projectId,
  (val) => {
    fetchData()
  },
  { deep: true, immediate: true }
)

watch(
  () => props.mainProductType,
  (val) => {
    if (val) {
      fetchData()
    }
  }
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

async function fetchData() {
  options.value = []
  originOptions.value = []
  if (!props.projectId) {
    selectValue.value = undefined
    selectChange(selectValue.value)
    return
  }
  let optionData = []
  loading.value = true
  try {
    const { content = [] } = (await getAll(props.projectId, { productType: props.mainProductType })) || {}
    originOptions.value = content || []
    optionData = content.map((o) => {
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
      selectValue.value = props.defaultValue ? props.defaultValue : undefined
      if (selectValue.value) {
        selectChange(selectValue.value)
      }
    }
    loading.value = false
  }
}

// 获取单体信息
function getOption(val) {
  return originOptions.value.find((k) => k.id === val)
}

// 获取单体类型
function getProductType(val) {
  return originOptions.value
    .find((k) => k.id === val)
    ?.productTypeList.reduce((res, cur) => {
      return res | cur.type
    }, 0)
}

function selectChange(val) {
  if (!Array.isArray(val)) {
    let monomerVal = {}
    if (!val) {
      val = undefined
      monomerVal = {}
    } else {
      monomerVal = originOptions.value.find((k) => k.id === val)
    }
    let areaInfo = []
    if (props.filterArea && props.productType) {
      areaInfo = (monomerVal?.areaSimpleList?.length && monomerVal.areaSimpleList.filter((v) => v.productType & props.productType)) || []
    } else {
      areaInfo = monomerVal?.areaSimpleList
    }
    emit('getAreaInfo', areaInfo)
    emit('getCurrentInfo', monomerVal)
  }
  emit('update:modelValue', val)
  emit('change', val)
}

defineExpose({
  getOption,
  getProductType
})
</script>
