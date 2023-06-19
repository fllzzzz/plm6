<!-- 仓库所属车间:下拉选择框 -->
<template>
  <common-select
    v-bind="$attrs"
    v-model="selectValue"
    :size="size"
    :disabled="disabled"
    :multiple="multiple"
    :loading="!loaded"
    :clearable="clearable"
    :showOptionAll="showOptionAll"
    :allLabelText="'全部'"
    :showExtra="showExtra"
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
            <span class="title">车间类型：</span>
            <span>{{data.workshopId?'车间':'普通'}}</span>
          </span>
        </span>
      </span>
    </template>
  </common-select>
</template>

<script setup>
import { defineProps, defineEmits, ref, watch, computed } from 'vue'
import { isBlank, judgeSameValue } from '@data-type/index'

import useWorkshopName from '@compos/store/use-workshop-name'
import { warehouseTypeEnum } from '@enum-ms/wms'

const emit = defineEmits(['change', 'update:modelValue'])

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
  showOptionAll: {
    type: Boolean,
    default: false
  },
  placeholder: {
    type: String,
    default: '请选择车间'
  },
  type: {
    type: [Number, String],
    default: undefined
  },
  showExtra: {
    type: Boolean,
    default: false
  }
})

const selectValue = ref()

const { loaded, workshopName } = useWorkshopName()

const options = computed(() => {
  let list = workshopName.value
  // 只查可使用的
  if (props.type) {
    list = props.type === warehouseTypeEnum.NORMAL.V ? list.filter((v) => !v.workshopId) : list.filter((v) => v.workshopId)
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
</script>
