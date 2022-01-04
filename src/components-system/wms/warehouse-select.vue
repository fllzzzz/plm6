<!-- 物料仓库:下拉选择框 -->
<template>
  <common-select
    v-model="selectValue"
    :size="size"
    :disabled="disabled"
    :multiple="multiple"
    :collapse-tags="collapseTags"
    :loading="!loaded"
    :clearable="clearable"
    filterable
    :placeholder="placeholder"
    :options="options"
    @change="handleChange"
  >
    <template #view="{ data }">
      <span class="customize-option-item">
        <span class="label">{{ data.name }}</span>
        <span>
          <span class="extra-label">
            <span class="title">存储类型：</span>
            <span v-parse-enum="{ e: rawMatClsEnum, v: data.materialType, bit: true, split: ' | ' }"></span>
          </span>
          <span class="extra-label">
            <span class="title">类型：</span>
            <span v-parse-enum="{ e: warehouseTypeEnum, v: data.type }"></span>
          </span>
        </span>
      </span>
    </template>
  </common-select>
</template>

<script setup>
import { defineProps, defineEmits, ref, watch, computed } from 'vue'
import { warehouseTypeEnum } from '@/utils/enum/modules/wms'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { isBlank, judgeSameValue } from '@data-type/index'
import useWarehouse from '@compos/store/use-warehouse'
const emit = defineEmits(['change', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: [Array, Number, String]
  },
  factoryId: {
    // 工厂id
    type: [Array, Number]
  },
  // 在不传入工厂的时候查所有，不包含被禁用的
  showAll: {
    type: Boolean,
    default: false
  },
  // 显示禁用
  showForbidden: {
    type: Boolean,
    default: false
  },
  basicClass: {
    // 基础分类
    type: Number
  },
  viewsDetail: {
    // 可查看详情
    type: Boolean,
    default: false
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
  onlyOneDefault: {
    type: Boolean,
    default: false
  },
  placeholder: {
    type: String,
    default: '选择仓库位置'
  }
})

const selectValue = ref()

const { loaded, warehouse } = useWarehouse()

const options = computed(() => {
  let list = warehouse.value
  // 只查可使用的
  if (!props.showForbidden) {
    list = list.filter((v) => v.enabled)
  }
  // 筛选工厂
  if (props.factoryId) {
    if (Array.isArray(props.factoryId)) {
      list = list.filter((v) => props.factoryId.includes(v.factoryId))
    } else {
      list = list.filter((v) => props.factoryId === v.factoryId)
    }
  } else {
    list = props.showAll ? list : []
  }
  // 筛选物料分类
  if (props.basicClass) {
    list = list.filter((v) => v.materialType & props.basicClass)
  }
  return list
})

watch(options, (opt) => {
  loadedCallBack()
})

watch(
  () => props.modelValue,
  (value) => {
    selectValue.value = value
    setDefault()
  },
  { immediate: true }
)

function handleChange(val) {
  let data = val
  if (isBlank(data)) data = undefined
  // 发生变化
  const isChange = !judgeSameValue(data, props.modelValue)
  // 两个值都为空
  const allBlank = isBlank(data) && isBlank(props.modelValue)

  if (isChange && !allBlank) {
    emit('update:modelValue', data)
    emit('change', data)
  }
}

function loadedCallBack() {
  setDefault()
}

/**
 * 设置默认值
 * 有默认值的情况，并且value为空，则给value赋值
 */
function setDefault() {
  if (isBlank(options.value) || selectValue.value) {
    return
  }
  if (props.onlyOneDefault && options.value.length === 1) {
    selectValue.value = options.value[0].value
    handleChange(selectValue.value)
    return
  }
  if (props.default) {
    selectValue.value = options.value[0].value
    handleChange(selectValue.value)
    return
  }
  handleChange(selectValue.value)
}
</script>

<style lang="scss" scoped>
.option-item {
  width: 100%;
  display: inline-flex;
  justify-content: space-between;
}

.option-item > span:nth-child(1) {
  flex: none;
  margin-right: 15px;
}
.option-item > span:nth-child(2) {
  // flex: auto;
  color: #8492a6;
  font-size: 13px;
  .label {
    color: #9b6161;
  }
}
</style>
