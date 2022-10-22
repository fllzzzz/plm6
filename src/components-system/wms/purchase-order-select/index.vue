<!-- 采购合同:下拉选择框 -->
<template>
  <span class="purchase-order-select-container">
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
      :data-structure="DS"
      class="purchase-order-select"
      @change="handleChange"
    >
      <template #view="{ data }">
        <span class="customize-option-item">
          <span class="flex-rsc label">
            <span>{{ data.serialNumber }}</span>
          </span>
          <span>
            <span class="extra-label">
              <span class="title">类型：</span>
              <span v-parse-enum="{ e: rawMatClsEnum, v: data.basicClass, bit: true, split: ' | ' }"></span>
            </span>
            <span v-if="data.supplier" class="extra-label">
              <span class="title">供应商：</span>
              <span class="more-text-info ellipsis-text">{{ data.supplier.name }}</span>
            </span>
            <span v-if="data.projectNames" class="extra-label">
              <span class="title">项目：</span>
              <span class="more-text-info ellipsis-text">{{ data.projectNames }}</span>
            </span>
          </span>
        </span>
      </template>
    </common-select>
  </span>
</template>

<script setup>
import { defineProps, defineEmits, ref, watch, computed, nextTick } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { isNotBlank, isBlank, judgeSameValue, deepClone } from '@data-type/index'

import usePurchaseOrder from '@compos/store/use-purchase-order'

const emit = defineEmits(['change', 'info-change', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: [Array, Number, String]
  },
  info: {
    type: Object
  },
  reload: {
    // 重新加载选项
    type: Boolean,
    default: true
  },
  detailable: {
    // 可查看详情
    type: Boolean,
    default: true
  },
  basicClass: {
    // 基础分类
    type: Number
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
    default: '选择采购合同'
  },
  supplierId: { // 有值时只保留当前供应商的采购合同
    type: [Number, String],
    default: ''
  },
  year: { // 有值时只保留当前年份的采购合同
    type: String,
    default: ''
  }
})

const DS = computed(() => {
  return {
    value: 'id',
    label: 'serialNumber',
    key: 'id'
  }
})

const selectValue = ref()

const { loaded, purchaseOrders, purchaseOrderKV } = usePurchaseOrder(loadedCallBack, props.reload)

const options = computed(() => {
  let list = deepClone(purchaseOrders.value)
  if (props.basicClass) {
    list = list.filter((v) => v.basicClass & props.basicClass)
    list = list.map((v) => {
      v.projectNames = v.projects ? v.projects.map((v) => v.shortName).join('、') : ''
      return v
    })
  }
  // 只保留当前供应商
  if (props.supplierId) {
    list = list.filter(v => v.supplier?.id === props.supplierId)
  }
  // 只保留当前年份
  if (props.year) {
    list = list.filter(v => v.createYear === props.year)
  }
  return list
})

watch(
  () => props.modelValue,
  (value) => {
    selectValue.value = value
  },
  { immediate: true }
)

// watch(
//   [() => props.basicClass, () => props.supplierId, () => props.year],
//   () => {
//     setDefault()
//   }
// )

function handleChange(val) {
  let data = val
  if (isBlank(data)) data = undefined
  // 发生变化
  const isChange = !judgeSameValue(data, props.modelValue)
  // 两个值都为空
  const allBlank = isBlank(data) && isBlank(props.modelValue)

  if (isChange && !allBlank) {
    emit('update:modelValue', data)
    emit('change', data, props.modelValue)
    emitInfo(data, props.modelValue)
  }
}

function emitInfo(val, oldVal) {
  const res = val ? purchaseOrderKV.value[val] : null
  const oldRes = oldVal ? purchaseOrderKV.value[oldVal] : null
  emit('info-change', res, oldRes)
}

function loadedCallBack() {
  if (isNotBlank(selectValue.value)) {
    emitInfo(selectValue.value)
  } else {
    setDefault()
  }
}

/**
 * 设置默认值
 * 有默认值的情况，并且value为空，则给value赋值
 */
function setDefault() {
  nextTick(() => {
    if (isBlank(options.value) || selectValue.value) {
      return
    }
    if (props.onlyOneDefault && options.value.length === 1) {
      selectValue.value = options.value[0].id
      handleChange(selectValue.value)
      return
    }
    if (props.default) {
      selectValue.value = options.value[0].id
      handleChange(selectValue.value)
      return
    }
    // 未赋予默认值
    if (isBlank(selectValue.value) && isNotBlank(props.info)) {
      emitInfo()
    }
  })
}
</script>

<style lang="scss" scoped>
.purchase-order-select-container {
  display: inline-flex;
  position: relative;

  .purchase-order-select {
    width: 100%;
  }
}

.more-text-info {
  display: inline-block;
  line-height: 12px;
  max-width: 250px;
}
</style>
